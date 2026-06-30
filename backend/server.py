import asyncio
import base64
import hashlib
import json
import logging
import mimetypes
import os
import re
import shutil
import subprocess
import tempfile
import time
from contextlib import asynccontextmanager
from pathlib import Path
from typing import Dict, List, Optional, Tuple

import httpx
from brotli_asgi import BrotliMiddleware
from dotenv import load_dotenv
from fastapi import Body, FastAPI, HTTPException, Request, UploadFile
from fastapi.responses import FileResponse, HTMLResponse, RedirectResponse, Response
from fastapi.staticfiles import StaticFiles
from fastapi.templating import Jinja2Templates
from pydantic import BaseModel

TIMEOUT_SECONDS = 10
MAX_CONCURRENT_REQUESTS = 100

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

# Load environment variables from .env file
load_dotenv()

# Check for NETWORK_ID
NETWORK_ID = os.getenv("NETWORK_ID", "0")  # defaults to 0 if not set
if not os.getenv("NETWORK_ID"):
    logger.warning("NETWORK_ID not set, using default value of 0 (Preview)")


BLOCKFROST_DEFAULT_RPC_URL = "https://ipfs.blockfrost.io/api/v0"


# Build the list of pre-configured IPFS providers.
#
# IPFS_PRECONFIGS_JSON contains a JSON list of objects.
# Common fields: id, label, description, format ("basic" | "nmkr" | "blockfrost").
# Per-format additional fields:
#   basic      : rpcUrl, userId, password
#   nmkr       : rpcUrl, userId, bearerToken
#   blockfrost : projectId (rpcUrl optional; defaults to Blockfrost's public host)
def _validate_preconfig(entry: dict, index: int) -> dict:
    common = ["id", "label", "description", "format"]
    for key in common:
        if not entry.get(key):
            raise Exception(f"IPFS preconfig #{index}: missing or empty field '{key}'")
    fmt = entry["format"].lower()
    if fmt not in ("basic", "nmkr", "blockfrost"):
        raise Exception(
            f"IPFS preconfig #{index} ('{entry['id']}'): unknown format '{fmt}'"
        )
    entry["format"] = fmt

    def _require(key: str) -> None:
        if not entry.get(key):
            raise Exception(
                f"IPFS preconfig #{index} ('{entry['id']}'): "
                f"format '{fmt}' requires '{key}'"
            )

    if fmt == "basic":
        for key in ("rpcUrl", "userId", "password"):
            _require(key)
    elif fmt == "nmkr":
        for key in ("rpcUrl", "userId", "bearerToken"):
            _require(key)
    elif fmt == "blockfrost":
        _require("projectId")
        # rpcUrl is optional for blockfrost — fall back to the public endpoint.
        if not entry.get("rpcUrl"):
            entry["rpcUrl"] = BLOCKFROST_DEFAULT_RPC_URL
        # filecoin is optional; admin opt-in for Filecoin pinning of rationale JSON.
        entry["filecoin"] = bool(entry.get("filecoin", False))
    return entry


IPFS_PRECONFIGS_JSON_RAW = os.getenv("IPFS_PRECONFIGS_JSON", "").strip()
IPFS_PRECONFIGS: List[dict] = []
if IPFS_PRECONFIGS_JSON_RAW:
    try:
        parsed = json.loads(IPFS_PRECONFIGS_JSON_RAW)
    except Exception as e:
        raise Exception(f"Invalid JSON for IPFS_PRECONFIGS_JSON: {e}")
    if not isinstance(parsed, list):
        raise Exception("IPFS_PRECONFIGS_JSON must be a JSON array")
    for index, entry in enumerate(parsed):
        if not isinstance(entry, dict):
            raise Exception(f"IPFS preconfig #{index} must be an object")
        IPFS_PRECONFIGS.append(_validate_preconfig(entry, index))
    seen_ids: set = set()
    for entry in IPFS_PRECONFIGS:
        if entry["id"] in seen_ids:
            raise Exception(f"Duplicate IPFS preconfig id: '{entry['id']}'")
        seen_ids.add(entry["id"])

if not IPFS_PRECONFIGS:
    logger.warning(
        "No IPFS preconfig available (IPFS_PRECONFIGS_JSON is unset or empty). "
        "All IPFS requests will need to provide a custom RPC config or will fail."
    )

# Indexed by id for fast lookup during pin requests.
IPFS_PRECONFIGS_BY_ID: Dict[str, dict] = {p["id"]: p for p in IPFS_PRECONFIGS}


# Public view exposed to the frontend (never includes secrets).
# `supportsFilecoin` is always included (default false) so the Elm flag
# decoder can rely on the field being present.
def _public_view(p: dict) -> dict:
    return {
        "id": p["id"],
        "label": p["label"],
        "description": p["description"],
        "supportsFilecoin": bool(p.get("format") == "blockfrost" and p.get("filecoin")),
    }


IPFS_PRECONFIGS_PUBLIC_JSON = json.dumps([_public_view(p) for p in IPFS_PRECONFIGS])

# Matomo Analytics
MATOMO_URL = os.getenv("MATOMO_URL")
MATOMO_SITE_ID = os.getenv("MATOMO_SITE_ID")
MATOMO_JS_URL = os.getenv("MATOMO_JS_URL")
MATOMO_SCRIPT = os.getenv("MATOMO_SCRIPT")

# Process Matomo script if all variables are present
if MATOMO_SCRIPT and MATOMO_URL and MATOMO_SITE_ID and MATOMO_JS_URL:
    MATOMO_SCRIPT = MATOMO_SCRIPT.replace("MATOMO_URL_PLACEHOLDER", MATOMO_URL)
    MATOMO_SCRIPT = MATOMO_SCRIPT.replace("MATOMO_SITE_ID_PLACEHOLDER", MATOMO_SITE_ID)
    MATOMO_SCRIPT = MATOMO_SCRIPT.replace("MATOMO_JS_URL_PLACEHOLDER", MATOMO_JS_URL)
else:
    MATOMO_SCRIPT = None

# Open Graph (social share card) configuration.
#
# Social crawlers (Facebook, X, Slack, Discord, ...) do not run the JS app, so
# they only ever see the meta tags in the server-rendered HTML. We inject the
# proposal title server-side when a link pre-selects a proposal, falling back to
# the generic card otherwise.
DEFAULT_OG_TITLE = "Cardano Governance Voting Tool"
DEFAULT_OG_DESCRIPTION = (
    "A simple tool to help every Cardano stakeholder participate in "
    "on-chain governance with confidence."
)
PROPOSAL_OG_DESCRIPTION = "Vote on this proposal at voting.cardanofoundation.org."
# Path (served from the static frontend build) to the generic fallback card.
OG_IMAGE_PATH = "/logo/og-image.jpg"

# og:image URLs must be absolute and public, since crawlers fetch them. By
# default we derive the origin (scheme + host) from the incoming request, so the
# tool works on whatever domain it's deployed to without configuration. Set
# OG_BASE_URL to force a specific origin (e.g. a CDN host distinct from the app).
OG_BASE_URL = os.getenv("OG_BASE_URL", "").strip()


def _request_base_url(request: Request) -> str:
    """Absolute origin (scheme://host) to build og: URLs from.

    Crawlers fetch og:image from wherever they loaded the page, so we derive the
    origin from the request rather than hardcoding a domain — this keeps every
    self-hosted deployment correct with no config. Behind a reverse proxy the
    original scheme/host arrive via X-Forwarded-*. OG_BASE_URL overrides all.
    """
    if OG_BASE_URL:
        return OG_BASE_URL
    proto = request.headers.get("x-forwarded-proto", "").split(",")[0].strip()
    host = (
        request.headers.get("x-forwarded-host", "").split(",")[0].strip()
        or request.headers.get("host", "").strip()
    )
    if not host:
        return str(request.base_url).rstrip("/")
    return f"{proto or request.url.scheme}://{host}"


# Per-proposal cards are rendered once, then cached on disk. Proposal off-chain
# metadata is immutable, so a rendered card never goes stale and repeat crawls
# become plain static-file serves. Pillow is optional: if it (or a usable font)
# is missing, we degrade gracefully to the generic static image.
OG_CARD_CACHE_DIR = (
    Path(os.getenv("OG_CARD_CACHE_DIR", tempfile.gettempdir())) / "og-cards"
)
try:
    from og_card import render_proposal_card

    OG_CARD_CACHE_DIR.mkdir(parents=True, exist_ok=True)
    _OG_CARD_AVAILABLE = True
except Exception as e:  # pragma: no cover - depends on optional Pillow install
    logger.warning(f"OG card rendering unavailable ({e}); using static image")
    _OG_CARD_AVAILABLE = False


def _og_network_slug(network_id: Optional[str]) -> str:
    return "mainnet" if network_id == "Mainnet" else "preview"


# Koios endpoints used to resolve a proposal's off-chain title
# (meta_json.body.title). The network comes from the request's `networkId`
# query param ("Mainnet"/"Preview"), since a proposal id is network-specific.
KOIOS_MAINNET_URL = "https://api.koios.rest/api/v1"
KOIOS_PREVIEW_URL = "https://preview.koios.rest/api/v1"


def _koios_api_url(network_id: Optional[str]) -> str:
    return KOIOS_MAINNET_URL if network_id == "Mainnet" else KOIOS_PREVIEW_URL


# Koios API token, used server-side only: both for the server's own Koios calls
# (e.g. resolving proposal titles) and to authenticate the Koios requests proxied
# for the frontend through the /koios endpoint. It is never sent to the browser.
# Set it via the env var; when unset, Koios is queried anonymously (subject to
# stricter rate limits).
KOIOS_API_TOKEN = os.getenv("KOIOS_API_TOKEN", "").strip()
if not KOIOS_API_TOKEN:
    logger.warning(
        "KOIOS_API_TOKEN not set; Koios requests will be unauthenticated "
        "and subject to stricter rate limits."
    )


def _koios_auth_headers() -> dict:
    """Authorization header for Koios, omitted when no token is configured."""
    return {"Authorization": f"Bearer {KOIOS_API_TOKEN}"} if KOIOS_API_TOKEN else {}


# Aggressive caching: proposal off-chain metadata is immutable once submitted,
# so a successful (or stably-empty) lookup is cached for an hour. A failed
# lookup is cached only briefly so a transient Koios outage recovers quickly.
OG_CACHE_TTL_SECONDS = 3600
OG_CACHE_FAILURE_TTL_SECONDS = 60
OG_CACHE_MAX_ENTRIES = 5000
# Short timeout so a slow Koios never blocks a crawler — we fall back to the
# generic card instead. A failed lookup is only briefly negative-cached, so the
# next crawl retries; a success is cached for an hour.
OG_FETCH_TIMEOUT_SECONDS = 4

# proposal_id -> (monotonic_expiry, title_or_None)
_og_title_cache: Dict[str, Tuple[float, Optional[str]]] = {}

# Bech32 CIP-129 governance action id (e.g. "gov_action1..."). Validating the
# shape before hitting Koios blocks junk query params from unbounded fetches.
_PROPOSAL_ID_RE = re.compile(r"^gov_action1[02-9ac-hj-np-z]{50,70}$")


def _og_cache_get(proposal_id: str) -> Tuple[bool, Optional[str]]:
    entry = _og_title_cache.get(proposal_id)
    if entry is not None and time.monotonic() < entry[0]:
        return True, entry[1]
    return False, None


def _og_cache_set(proposal_id: str, title: Optional[str], ttl: float) -> None:
    if len(_og_title_cache) >= OG_CACHE_MAX_ENTRIES:
        now = time.monotonic()
        for key in [k for k, v in _og_title_cache.items() if v[0] <= now]:
            del _og_title_cache[key]
        if len(_og_title_cache) >= OG_CACHE_MAX_ENTRIES:
            _og_title_cache.clear()
    _og_title_cache[proposal_id] = (time.monotonic() + ttl, title)


async def fetch_proposal_title(
    proposal_id: str, network_id: Optional[str]
) -> Optional[str]:
    """Return the off-chain title for a proposal, or None. Cached aggressively.

    The Koios network is chosen from the request's `networkId` param, since a
    proposal id only exists on its own network.
    """
    if not _PROPOSAL_ID_RE.match(proposal_id):
        return None

    api_url = _koios_api_url(network_id)
    cache_key = f"{api_url}|{proposal_id}"
    cached, title = _og_cache_get(cache_key)
    if cached:
        return title

    try:
        response = await app.async_client.get(  # type: ignore
            f"{api_url}/proposal_list",
            params={
                "proposal_id": f"eq.{proposal_id}",
                "select": "meta_json,proposal_id",
            },
            headers={
                "accept": "application/json",
                **_koios_auth_headers(),
            },
            timeout=OG_FETCH_TIMEOUT_SECONDS,
        )
        response.raise_for_status()
        rows = response.json()
    except Exception as e:
        logger.warning(f"OG: failed to fetch title for {proposal_id}: {e}")
        _og_cache_set(cache_key, None, OG_CACHE_FAILURE_TTL_SECONDS)
        return None

    try:
        raw_title = rows[0]["meta_json"]["body"]["title"]
    except (IndexError, KeyError, TypeError):
        raw_title = None
    title = (
        raw_title.strip() if isinstance(raw_title, str) and raw_title.strip() else None
    )

    _og_cache_set(cache_key, title, OG_CACHE_TTL_SECONDS)
    return title


def _og_context(title: Optional[str], image_url: str) -> dict:
    """Open Graph template variables: proposal-specific when a title is known."""
    if title:
        return {
            "og_title": title,
            "og_description": PROPOSAL_OG_DESCRIPTION,
            "og_image": image_url,
        }
    return {
        "og_title": DEFAULT_OG_TITLE,
        "og_description": DEFAULT_OG_DESCRIPTION,
        "og_image": image_url,
    }


# Preconfigured voter templates
PRECONFIGURED_VOTERS_JSON = os.getenv("PRECONFIGURED_VOTERS_JSON", "[]")
try:
    json.loads(PRECONFIGURED_VOTERS_JSON)
except Exception as e:
    raise Exception(f"Invalid JSON for PRECONFIGURED_VOTERS_JSON: {e}")

# Preconfigured authors for rationale signatures
PRECONFIGURED_AUTHORS_JSON = os.getenv("PRECONFIGURED_AUTHORS_JSON", "[]")
try:
    json.loads(PRECONFIGURED_AUTHORS_JSON)
except Exception as e:
    raise Exception(f"Invalid JSON for PRECONFIGURED_AUTHORS_JSON: {e}")


# Define an async HTTP client (using httpx) and attach it to the FastAPI app
@asynccontextmanager
async def lifespan(app: FastAPI):
    async with httpx.AsyncClient(
        timeout=httpx.Timeout(TIMEOUT_SECONDS),
        limits=httpx.Limits(max_connections=MAX_CONCURRENT_REQUESTS),
    ) as client:
        app.async_client = client  # type: ignore
        yield


# Initialize FastAPI app
app = FastAPI(title="Cardano Gov Voting Server", lifespan=lifespan)

# Add BrotliMiddleware to compress responses
app.add_middleware(BrotliMiddleware)

# Statically served directory (contains the frontend build)
static_dir = Path("../frontend/static")
templates = Jinja2Templates(directory=static_dir)


def _base_context(request: Request) -> dict:
    return {
        "request": request,
        "network_id": NETWORK_ID,
        "ipfs_preconfigs": IPFS_PRECONFIGS_PUBLIC_JSON,
        "preconfigured_voters": PRECONFIGURED_VOTERS_JSON,
        "preconfigured_authors": PRECONFIGURED_AUTHORS_JSON,
        "matomo_script": MATOMO_SCRIPT,
    }


@app.get("/", response_class=HTMLResponse)
async def read_root(request: Request):
    image_url = f"{_request_base_url(request)}{OG_IMAGE_PATH}"
    return templates.TemplateResponse(
        "index.html",
        {**_base_context(request), **_og_context(None, image_url)},
        headers={"Cache-Control": "no-cache"},
    )


@app.get("/page/{full_path:path}", response_class=HTMLResponse)
async def get_page(full_path: str, request: Request):
    # When a link pre-selects a proposal, inject its title into the OG card so
    # shared links show the proposal rather than the generic tool card.
    proposal_id = request.query_params.get("proposalId")
    network_id = request.query_params.get("networkId")
    title = await fetch_proposal_title(proposal_id, network_id) if proposal_id else None

    # Point og:image at the dynamic per-proposal card when we have a title and
    # the renderer is available; otherwise fall back to the generic image.
    base_url = _request_base_url(request)
    image_url = f"{base_url}{OG_IMAGE_PATH}"
    if title and _OG_CARD_AVAILABLE:
        # Pass the original networkId ("Mainnet"/"Preview") through unchanged so
        # the card route resolves the title on the same network we just did.
        image_url = f"{base_url}/og/proposal/{proposal_id}.jpg?networkId={network_id}"

    return templates.TemplateResponse(
        "index.html",
        {**_base_context(request), **_og_context(title, image_url)},
        headers={"Cache-Control": "no-cache"},
    )


@app.get("/og/proposal/{proposal_id}.jpg")
async def og_proposal_card(proposal_id: str, request: Request):
    """Serve the per-proposal social card, rendering+caching it on first hit."""
    network_id = request.query_params.get("networkId")
    fallback_url = f"{_request_base_url(request)}{OG_IMAGE_PATH}"
    title = (
        await fetch_proposal_title(proposal_id, network_id)
        if _OG_CARD_AVAILABLE
        else None
    )
    if not title:
        return RedirectResponse(fallback_url, status_code=302)

    net = _og_network_slug(network_id)
    path = OG_CARD_CACHE_DIR / f"{net}-{proposal_id}.jpg"
    if not path.exists():
        try:
            data = await asyncio.to_thread(render_proposal_card, title)
            tmp = path.with_suffix(".jpg.tmp")
            tmp.write_bytes(data)
            tmp.replace(path)  # atomic publish
        except Exception as e:
            logger.warning(f"OG: card render failed for {proposal_id}: {e}")
            return RedirectResponse(fallback_url, status_code=302)

    return FileResponse(
        path,
        media_type="image/jpeg",
        headers={"Cache-Control": "public, max-age=86400, immutable"},
    )


@app.post("/pretty-gov-pdf")
async def create_pretty_pdf(data: dict = Body(...)):
    """
    Generate a pretty PDF from JSON governance data.
    Returns the PDF file.
    """
    try:
        logger.info("Pretty PDF attempt")
        # Create a temporary directory for processing
        with tempfile.TemporaryDirectory() as temp_dir:
            # Write metadata to JSON file
            metadata_file = Path(temp_dir) / "metadata.json"
            with open(metadata_file, "w") as f:
                json.dump(data, f)

            output_path = Path(temp_dir) / "metadata.pdf"

            # Copy template file to temp directory
            template_dest = Path(temp_dir) / "template.typ"
            shutil.copyfile("template.typ", str(template_dest))

            # Change to temp directory and run typst command
            current_dir = os.getcwd()

            try:
                os.chdir(temp_dir)
                # Add more verbose logging
                logger.info(f"Running typst in directory: {temp_dir}")
                logger.info(f"Files in directory: {os.listdir('.')}")

                process = subprocess.run(
                    ["typst", "compile", "template.typ", "metadata.pdf"],
                    capture_output=True,
                    text=True,
                    check=True,
                )

                # Log the process output
                if process.stdout:
                    logger.info(f"Typst output: {process.stdout}")
                if process.stderr:
                    logger.warning(f"Typst stderr: {process.stderr}")

                # Check if the file was actually created
                if not output_path.exists():
                    logger.error("PDF file was not created after typst command")
                    raise HTTPException(
                        status_code=500,
                        detail="PDF generation failed - output file not created",
                    )

                # Log file size to ensure it's not empty
                logger.info(f"Generated PDF size: {output_path.stat().st_size} bytes")

                # Read the file into memory before the temp directory is cleaned up
                pdf_content = output_path.read_bytes()

                return Response(
                    content=pdf_content,
                    media_type="application/pdf",
                    headers={
                        "Content-Disposition": "attachment; filename=metadata.pdf"
                    },
                )

            finally:
                os.chdir(current_dir)

    except subprocess.CalledProcessError as e:
        logger.error(f"PDF generation failed: {e.stderr}")
        raise HTTPException(
            status_code=500, detail=f"PDF generation failed: {e.stderr}"
        )
    except Exception as e:
        logger.error(f"Unexpected error: {str(e)}")
        raise HTTPException(status_code=500, detail="An unexpected error occurred")


class IPFSPinRequest(BaseModel):
    ipfsServer: str | None = None
    headers: List[Tuple[str, str]] | None = None
    preconfigId: str | None = None
    filecoin: bool = False


class IPFSPinJSONRequest(IPFSPinRequest):
    fileName: str
    jsonContent: str


def _pick_preconfig(preconfig_id: Optional[str]) -> Optional[dict]:
    if not IPFS_PRECONFIGS:
        return None
    if preconfig_id is None:
        # Back-compat: when no id is supplied, use the first preconfig.
        return IPFS_PRECONFIGS[0]
    entry = IPFS_PRECONFIGS_BY_ID.get(preconfig_id)
    if entry is None:
        raise HTTPException(
            status_code=400,
            detail=f"Unknown IPFS preconfig id: '{preconfig_id}'",
        )
    return entry


def get_ipfs_config(
    preconfig_id: Optional[str] = None,
    ipfs_server: Optional[str] = None,
    custom_headers: Optional[List[Tuple[str, str]]] = None,
    filecoin_requested: bool = False,
) -> Tuple[str, Dict[str, str], str, bool]:
    """Resolve the IPFS endpoint + auth headers, format, and effective Filecoin flag.

    Precedence:
      1. Explicit ipfs_server + custom_headers from the request (custom provider).
      2. The named preconfig (preconfig_id).
      3. The first available preconfig (back-compat).

    The effective Filecoin flag is `True` only when the caller asks for it AND
    the resolved provider is a Blockfrost preconfig that opted in via `filecoin: true`.
    """

    # Explicit custom provider from the request: pass through as-is, format
    # is assumed to be "basic" (kubo-style /add) for custom RPCs.
    if ipfs_server and custom_headers:
        return ipfs_server, dict(custom_headers), "basic", False

    entry = _pick_preconfig(preconfig_id)
    if entry is None:
        raise HTTPException(
            status_code=500,
            detail=(
                "No IPFS preconfig is available on the server and no custom "
                "IPFS server was provided in the request."
            ),
        )

    server_url = entry["rpcUrl"]
    fmt = entry["format"]

    if fmt == "nmkr":
        user_id = entry["userId"]
        bearer_token = entry["bearerToken"]
        if not server_url.endswith(f"/{user_id}"):
            server_url = f"{server_url}/{user_id}"
        return (
            server_url,
            {
                "Authorization": f"Bearer {bearer_token}",
                "Accept": "application/json",
            },
            fmt,
            False,
        )

    if fmt == "blockfrost":
        use_filecoin = bool(filecoin_requested and entry.get("filecoin"))
        return (
            server_url,
            {
                "project_id": entry["projectId"],
                "Accept": "application/json",
            },
            fmt,
            use_filecoin,
        )

    else:
        # Default to basic auth format
        user_id = entry["userId"]
        password = entry["password"]
        auth_string = f"{user_id}:{password}"
        token = base64.b64encode(auth_string.encode("utf-8")).decode("ascii")
        return (
            server_url,
            {
                "Authorization": f"Basic {token}",
                "Accept": "application/json",
            },
            fmt,
            False,
        )


async def pin_to_ipfs_common(
    temp_file_path: Path,
    ipfs_server: Optional[str] = None,
    custom_headers: Optional[List[Tuple[str, str]]] = None,
    preconfig_id: Optional[str] = None,
    filecoin: bool = False,
):
    """Common IPFS pinning logic used by both endpoints."""
    try:
        server_url, headers, ipfs_format, use_filecoin = get_ipfs_config(
            preconfig_id, ipfs_server, custom_headers, filecoin_requested=filecoin
        )

        # Different handling based on format
        if ipfs_format == "nmkr":
            # For Nmkr format, we need to convert file to base64 and include metadata
            file_name = temp_file_path.name

            # Determine MIME type
            mime_type, _ = mimetypes.guess_type(file_name)
            if not mime_type:
                mime_type = "application/octet-stream"

            # Read file and convert to base64
            with open(temp_file_path, "rb") as f:
                file_bytes = f.read()
                file_base64 = base64.b64encode(file_bytes).decode("utf-8")

            # Create JSON payload
            payload = {
                "mimetype": mime_type,
                "name": file_name,
                "fileFromBase64": file_base64,
            }

            # Make request
            response = await app.async_client.post(  # type: ignore
                url=server_url,  # Not adding /add for Nmkr
                headers=headers,
                json=payload,
            )

            # For Nmkr, pinning happens automatically
            return Response(
                content=response.content,
                status_code=response.status_code,
                media_type=response.headers.get("content-type"),
            )

        elif ipfs_format == "blockfrost":
            # Blockfrost IPFS: /ipfs/add returns the CID but does not pin.
            # We then call /ipfs/pin/add/{cid} to actually pin. The frontend's
            # IpfsAnswer decoder reads `ipfs_hash` from the /add response, so
            # we return that body whether or not the pin step succeeds — the
            # status code reflects pin failure when applicable.
            with open(temp_file_path, "rb") as f:
                add_response = await app.async_client.post(  # type: ignore
                    url=f"{server_url}/ipfs/add",
                    headers=headers,
                    files={"file": f},
                )

            if add_response.status_code != 200:
                return Response(
                    content=add_response.content,
                    status_code=add_response.status_code,
                    media_type=add_response.headers.get("content-type"),
                )

            try:
                cid = add_response.json()["ipfs_hash"]
            except (KeyError, json.JSONDecodeError) as e:
                logger.error(f"Blockfrost /add returned no ipfs_hash: {e}")
                raise HTTPException(
                    status_code=502,
                    detail="Blockfrost IPFS add response missing ipfs_hash",
                )

            pin_url = f"{server_url}/ipfs/pin/add/{cid}"
            if use_filecoin:
                pin_url += "?filecoin=true"
            pin_response = await app.async_client.post(  # type: ignore
                url=pin_url,
                headers=headers,
            )

            if pin_response.status_code != 200:
                logger.warning(
                    "Blockfrost pin/add failed for CID %s (status %s): %s",
                    cid,
                    pin_response.status_code,
                    pin_response.text,
                )
                return Response(
                    content=pin_response.content,
                    status_code=pin_response.status_code,
                    media_type=pin_response.headers.get("content-type"),
                )

            return Response(
                content=add_response.content,
                status_code=add_response.status_code,
                media_type=add_response.headers.get("content-type"),
            )

        else:
            # Basic format - uses /add endpoint with file upload
            with open(temp_file_path, "rb") as f:
                add_response = await app.async_client.post(  # type: ignore
                    url=f"{server_url}/add?pin=true",  # Add pin=true parameter
                    headers=headers,
                    files={"file": f},
                )

                # Check if the add request was successful
                if add_response.status_code != 200:
                    return Response(
                        content=add_response.content,
                        status_code=add_response.status_code,
                        media_type=add_response.headers.get("content-type"),
                    )

                # Return the response directly since pinning is already done
                return Response(
                    content=add_response.content,
                    status_code=add_response.status_code,
                    media_type=add_response.headers.get("content-type"),
                )

    except HTTPException:
        raise
    except json.JSONDecodeError as e:
        logger.error(f"Failed to parse IPFS output: {str(e)}")
        raise HTTPException(status_code=500, detail="Failed to parse IPFS response")
    except Exception as e:
        logger.error(f"Unexpected error: {str(e)}")
        raise HTTPException(status_code=500, detail="An unexpected error occurred")


@app.post("/ipfs-pin/file")
async def pin_file_to_ipfs(
    file: UploadFile,
    ipfs_server: Optional[str] = None,
    preconfig_id: Optional[str] = None,
    filecoin: bool = False,
):
    """Pin a file to IPFS and return the hash.

    When `ipfs_server` is not provided, `preconfig_id` selects which
    pre-configured IPFS provider to use (defaults to the first one).
    `filecoin=true` additionally pins to Filecoin via Blockfrost, but only
    when the resolved preconfig is Blockfrost and was admin-opted-in.
    """
    logger.info("IPFS file pin attempt")

    with tempfile.TemporaryDirectory() as temp_dir:
        temp_file_path = Path(temp_dir) / file.filename  # type: ignore
        content = await file.read()

        with open(temp_file_path, "wb") as f:
            f.write(content)

        return await pin_to_ipfs_common(
            temp_file_path,
            ipfs_server=ipfs_server,
            custom_headers=None,
            preconfig_id=preconfig_id,
            filecoin=filecoin,
        )


@app.post("/ipfs-pin/json")
async def pin_json_to_ipfs(request: IPFSPinJSONRequest):
    """Pin JSON content to IPFS and return the hash."""
    logger.info("IPFS JSON pin attempt")

    with tempfile.TemporaryDirectory() as temp_dir:
        temp_file_path = Path(temp_dir) / request.fileName

        with open(temp_file_path, "w", encoding="utf-8") as f:
            f.write(request.jsonContent)

        return await pin_to_ipfs_common(
            temp_file_path,
            ipfs_server=request.ipfsServer,
            custom_headers=request.headers,
            preconfig_id=request.preconfigId,
            filecoin=request.filecoin,
        )


class KoiosRequest(BaseModel):
    networkId: str  # "Mainnet" selects mainnet; anything else uses Preview.
    path: str  # Koios path (with query string), e.g. "/ogmios" or "/vote_list?...".
    method: str = "GET"
    body: Optional[dict] = None


# ---------------------------------------------------------------------------
# Koios response cache
#
# The proxied Koios requests have very different freshness needs, so the cache
# TTL is chosen from the request's semantics:
#
#   * Immutable, content-addressed lookups — a tx, script, or datum is fixed by
#     its hash and never changes — are cached for a long time.
#   * Epoch-scoped data (protocol params, constitution, CC members, DRep voting
#     power, pool stake snapshot) only changes at epoch boundaries, so a few
#     minutes of staleness is harmless.
#   * Volatile lists (current epoch, active proposals, a voter's votes) change
#     within an epoch as txs land, so they get a short TTL — just enough to
#     absorb bursts of identical requests without serving stale data for long.
#   * UTxO liveness (is_spent) is never cached: a UTxO can be spent at any time
#     and a stale "unspent" answer would break tx building.
# ---------------------------------------------------------------------------

KOIOS_CACHE_TTL_IMMUTABLE = 24 * 3600  # tx_cbor, script_info, datum_info
KOIOS_CACHE_TTL_EPOCH = 600  # protocol params, constitution, CC, drep, pool stake
KOIOS_CACHE_TTL_VOLATILE = 30  # current epoch, proposal_list, vote_list
KOIOS_CACHE_MAX_ENTRIES = 2000

# cache_key -> (monotonic_expiry, status_code, media_type, content_bytes)
_koios_cache: Dict[str, Tuple[float, int, Optional[str], bytes]] = {}


def _koios_cache_ttl(path: str, body: Optional[dict]) -> Optional[float]:
    """Pick a cache TTL (seconds) from the request semantics, or None to skip."""
    # Match on the Koios resource only, ignoring the query string.
    resource = path.split("?", 1)[0].strip("/")

    if resource in ("tx_cbor", "script_info", "datum_info"):
        return KOIOS_CACHE_TTL_IMMUTABLE

    if resource == "utxo_info":
        # Spent status is mutable; never cache it.
        return None

    if resource in ("proposal_list", "vote_list"):
        return KOIOS_CACHE_TTL_VOLATILE

    if resource == "pool_stake_snapshot":
        return KOIOS_CACHE_TTL_EPOCH

    if resource == "ogmios":
        # The Ogmios JSON-RPC method disambiguates same-path queries.
        method = (body or {}).get("method", "")
        if method == "queryLedgerState/epoch":
            return KOIOS_CACHE_TTL_VOLATILE
        # protocolParameters, constitution, constitutionalCommittee and
        # delegateRepresentatives are all epoch-scoped.
        return KOIOS_CACHE_TTL_EPOCH

    # Unknown endpoint: don't cache.
    return None


def _koios_cache_key(
    network_id: str, method: str, path: str, body: Optional[dict]
) -> str:
    raw = json.dumps(
        {"n": network_id, "m": method, "p": path, "b": body},
        sort_keys=True,
        separators=(",", ":"),
    )
    return hashlib.sha256(raw.encode("utf-8")).hexdigest()


def _koios_cache_get(key: str) -> Optional[Tuple[int, Optional[str], bytes]]:
    entry = _koios_cache.get(key)
    if entry is not None and time.monotonic() < entry[0]:
        return entry[1], entry[2], entry[3]
    return None


def _koios_cache_set(
    key: str, status_code: int, media_type: Optional[str], content: bytes, ttl: float
) -> None:
    if len(_koios_cache) >= KOIOS_CACHE_MAX_ENTRIES:
        now = time.monotonic()
        for k in [k for k, v in _koios_cache.items() if v[0] <= now]:
            del _koios_cache[k]
        if len(_koios_cache) >= KOIOS_CACHE_MAX_ENTRIES:
            _koios_cache.clear()
    _koios_cache[key] = (time.monotonic() + ttl, status_code, media_type, content)


# Bodies that carry no usable result; not worth caching (and caching an empty
# tx/script/datum lookup for a day would wrongly pin a not-yet-indexed answer).
_KOIOS_EMPTY_BODIES = (b"", b"[]", b"null")


@app.post("/koios")
async def koios_request(request: KoiosRequest):
    """
    Proxy a request to the Koios API, authenticated with the server's token.

    The frontend hits this endpoint instead of calling Koios directly, so the
    Koios API token stays server-side and is never exposed to the browser. The
    base URL is derived from `networkId` here (not trusted from the client) and
    only the relative `path` is appended, so the token is never forwarded
    anywhere but Koios.

    Successful responses are cached with a TTL derived from the request
    semantics (see `_koios_cache_ttl`), so repeated lookups of immutable data
    (txs, scripts, datums) and bursts of identical queries are served locally.
    """
    try:
        ttl = _koios_cache_ttl(request.path, request.body)

        cache_key = None
        if ttl is not None:
            cache_key = _koios_cache_key(
                request.networkId, request.method, request.path, request.body
            )
            cached = _koios_cache_get(cache_key)
            if cached is not None:
                status_code, media_type, content = cached
                return Response(
                    content=content, status_code=status_code, media_type=media_type
                )

        api_url = _koios_api_url(request.networkId)
        response = await app.async_client.request(  # type: ignore
            method=request.method,
            url=f"{api_url}{request.path}",
            headers={
                "accept": "application/json",
                "content-type": "application/json",
                **_koios_auth_headers(),
            },
            json=request.body,
        )

        # Only cache successful, non-empty responses; errors and empty results
        # fall through so the next call retries against Koios.
        if (
            cache_key is not None
            and ttl is not None
            and response.status_code == 200
            and response.content.strip() not in _KOIOS_EMPTY_BODIES
        ):
            _koios_cache_set(
                cache_key,
                response.status_code,
                response.headers.get("content-type"),
                response.content,
                ttl,
            )

        return Response(
            content=response.content,
            status_code=response.status_code,
            media_type=response.headers.get("content-type"),
        )

    except Exception as e:
        logger.error(f"Unexpected error: {str(e)}")
        raise HTTPException(status_code=500, detail="An unexpected error occurred")


class ProxyRequest(BaseModel):
    url: str
    method: str = "GET"
    headers: dict = {}
    body: Optional[dict] = None


@app.post("/proxy/json")
async def proxy_request(request: ProxyRequest):
    """
    Proxy an HTTP request to the specified URL and return the response.
    Default "accept" and "content-type" headers for JSON are automatically added.
    """
    try:
        # Default headers for JSON
        default_headers = {
            "accept": "application/json",
            "content-type": "application/json",
        }

        # Make the request
        response = await app.async_client.request(  # type: ignore
            method=request.method,
            url=request.url,
            headers={**default_headers, **request.headers},
            json=request.body,
        )

        return Response(
            content=response.content,
            status_code=response.status_code,
            media_type=response.headers.get("content-type"),
        )

    except Exception as e:
        logger.error(f"Unexpected error: {str(e)}")
        raise HTTPException(status_code=500, detail="An unexpected error occurred")


class CompressedStaticFiles(StaticFiles):
    async def get_response(self, path: str, scope):
        """
        Serve pre-compressed `.br` or `.gz` files when the client supports them,
        always attaching a validator so browsers revalidate rather than serve a
        stale build.

        Assets keep stable filenames (e.g. `main.js`), so without an explicit
        Cache-Control browsers fall back to heuristic caching and can keep an
        outdated build for a long time. We send `Cache-Control: no-cache` ("you
        may store it, but revalidate before reusing") together with an ETag:
        unchanged files come back as a tiny 304, changed files are fetched
        fresh, so nobody gets stuck on an old version.
        """
        accept_encoding = ""
        if_none_match = ""
        for header, value in scope["headers"]:
            if header == b"accept-encoding":
                accept_encoding = value.decode()
            elif header == b"if-none-match":
                if_none_match = value.decode()

        full_path = os.path.join(self.directory, path)  # type: ignore

        # Pick a pre-compressed variant the client accepts, if one exists.
        encoding = None
        served_path = full_path
        if "br" in accept_encoding and os.path.exists(full_path + ".br"):
            encoding, served_path = "br", full_path + ".br"
        elif "gzip" in accept_encoding and os.path.exists(full_path + ".gz"):
            encoding, served_path = "gzip", full_path + ".gz"

        if encoding is None:
            # Uncompressed path: Starlette already sets ETag/Last-Modified and
            # handles conditional requests; we only add the revalidation policy.
            response = await super().get_response(path, scope)
            response.headers["Cache-Control"] = "no-cache"
            return response

        # Compressed variant: derive the ETag from the *uncompressed* file so the
        # validator tracks content changes regardless of which encoding is sent,
        # and answer conditional requests ourselves (FileResponse would otherwise
        # ignore the client's If-None-Match for these branches).
        try:
            stat = os.stat(full_path)
        except FileNotFoundError:
            stat = os.stat(served_path)
        etag = f'"{stat.st_mtime_ns:x}-{stat.st_size:x}"'
        headers = {
            "Content-Encoding": encoding,
            "Cache-Control": "no-cache",
            "ETag": etag,
            "Vary": "Accept-Encoding",
        }

        candidates = [t.strip() for t in if_none_match.split(",") if t.strip()]
        if "*" in candidates or etag in candidates or f"W/{etag}" in candidates:
            return Response(status_code=304, headers=headers)

        return FileResponse(
            served_path,
            media_type=mimetypes.guess_type(full_path)[0],
            headers=headers,
        )


# Mount static files from static directory
app.mount("/", CompressedStaticFiles(directory=static_dir), name="static")


if __name__ == "__main__":
    import uvicorn

    # Ensure the static directory exists
    if not static_dir.exists():
        logger.warning(
            "static directory does not exist. Static files will not be served."
        )

    # Run the server
    uvicorn.run(
        "server:app",
        host="0.0.0.0",
        port=8000,
        reload=True,
        limit_concurrency=MAX_CONCURRENT_REQUESTS,
    )

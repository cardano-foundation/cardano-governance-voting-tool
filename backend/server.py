import base64
import json
import logging
import mimetypes
import os
import shutil
import subprocess
import tempfile
from contextlib import asynccontextmanager
from pathlib import Path
from typing import Dict, List, Optional, Tuple

import httpx
from brotli_asgi import BrotliMiddleware
from dotenv import load_dotenv
from fastapi import Body, FastAPI, HTTPException, Request, UploadFile
from fastapi.responses import FileResponse, HTMLResponse, Response
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


@app.get("/", response_class=HTMLResponse)
async def read_root(request: Request):
    return templates.TemplateResponse(
        "index.html",
        {
            "request": request,
            "network_id": NETWORK_ID,
            "ipfs_preconfigs": IPFS_PRECONFIGS_PUBLIC_JSON,
            "preconfigured_voters": PRECONFIGURED_VOTERS_JSON,
            "preconfigured_authors": PRECONFIGURED_AUTHORS_JSON,
            "matomo_script": MATOMO_SCRIPT,
        },
    )


@app.get("/page/{full_path:path}", response_class=HTMLResponse)
async def get_page(full_path: str, request: Request):
    return templates.TemplateResponse(
        "index.html",
        {
            "request": request,
            "network_id": NETWORK_ID,
            "ipfs_preconfigs": IPFS_PRECONFIGS_PUBLIC_JSON,
            "preconfigured_voters": PRECONFIGURED_VOTERS_JSON,
            "preconfigured_authors": PRECONFIGURED_AUTHORS_JSON,
            "matomo_script": MATOMO_SCRIPT,
        },
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
        Serve pre-compressed `.gz` or `.br` files if available and the client supports it.
        """
        accept_encoding = ""
        for header, value in scope["headers"]:
            if header == b"accept-encoding":
                accept_encoding = value.decode()
                break

        full_path = os.path.join(self.directory, path)  # type: ignore

        # Serve Brotli (.br) if supported and available
        if "br" in accept_encoding and os.path.exists(full_path + ".br"):
            return FileResponse(full_path + ".br", headers={"Content-Encoding": "br"})

        # Serve Gzip (.gz) if supported and available
        if "gzip" in accept_encoding and os.path.exists(full_path + ".gz"):
            return FileResponse(full_path + ".gz", headers={"Content-Encoding": "gzip"})

        # Default to the uncompressed version
        return await super().get_response(path, scope)


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

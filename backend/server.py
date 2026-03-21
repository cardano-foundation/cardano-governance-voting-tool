import json
import logging
import os
import shutil
import base64
import subprocess
import tempfile
import mimetypes
from pathlib import Path
from typing import List, Optional, Tuple, Dict

import httpx
from pydantic import BaseModel
from dotenv import load_dotenv
from fastapi import Body, FastAPI, HTTPException, UploadFile, Request
from fastapi.responses import HTMLResponse, Response, FileResponse
from fastapi.staticfiles import StaticFiles
from fastapi.templating import Jinja2Templates
from contextlib import asynccontextmanager
from brotli_asgi import BrotliMiddleware
from survey_metadata import build_survey_response, resolve_linked_proposal

TIMEOUT_SECONDS = 10
MAX_CONCURRENT_REQUESTS = 100

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

# Load environment variables from .env file
load_dotenv()

# Check for basic auth format completeness
basic_auth_vars = ["IPFS_RPC_URL", "IPFS_USER_ID", "IPFS_PASSWORD"]
basic_auth_missing = [var for var in basic_auth_vars if not os.getenv(var)]
basic_auth_complete = len(basic_auth_missing) == 0

# Check for nmkr format completeness
nmkr_auth_vars = ["IPFS_RPC_URL", "IPFS_USER_ID", "IPFS_BEARER_TOKEN"]
nmkr_auth_missing = [var for var in nmkr_auth_vars if not os.getenv(var)]
nmkr_auth_complete = len(nmkr_auth_missing) == 0

# Only show warnings if both formats are incomplete
if not basic_auth_complete and not nmkr_auth_complete:
    logger.warning(
        "Neither basic nor nmkr IPFS authentication is completely configured."
    )
    if basic_auth_missing:
        logger.warning(f"Basic auth missing: {', '.join(basic_auth_missing)}")
    if nmkr_auth_missing:
        logger.warning(f"Nmkr auth missing: {', '.join(nmkr_auth_missing)}")
    logger.warning("All IPFS requests will need to provide RPC config or will fail")

# Check for NETWORK_ID
NETWORK_ID = os.getenv("NETWORK_ID", "0")  # defaults to 0 if not set
if not os.getenv("NETWORK_ID"):
    logger.warning("NETWORK_ID not set, using default value of 0 (Preview)")

# Check if using Nmkr format - default to basic if both are available
IPFS_FORMAT = os.getenv("IPFS_FORMAT", "basic").lower()
if IPFS_FORMAT not in ["basic", "nmkr"]:
    logger.warning(f"Unknown IPFS_FORMAT '{IPFS_FORMAT}', defaulting to 'basic'")
    IPFS_FORMAT = "basic"

# If the configured format is incomplete but the other one is complete, use the complete one
if IPFS_FORMAT == "basic" and not basic_auth_complete and nmkr_auth_complete:
    logger.info("Basic auth incomplete but nmkr auth complete. Using nmkr format.")
    IPFS_FORMAT = "nmkr"
elif IPFS_FORMAT == "nmkr" and not nmkr_auth_complete and basic_auth_complete:
    logger.info("Nmkr auth incomplete but basic auth complete. Using basic format.")
    IPFS_FORMAT = "basic"

# Set variables for selected format
IPFS_USER_ID = os.getenv("IPFS_USER_ID", "")
IPFS_PASSWORD = os.getenv("IPFS_PASSWORD", "")
IPFS_BEARER_TOKEN = os.getenv("IPFS_BEARER_TOKEN", "")

# Set variables for the IPFS server textual description
IPFS_LABEL = os.getenv("IPFS_LABEL", "Pre-configured IPFS server")
IPFS_DESCRIPTION = os.getenv(
    "IPFS_DESCRIPTION", "Files will be pinned using the pre-configured IPFS server."
)

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
        app.async_client = client  # pyright: ignore
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
            "ipfs_label": IPFS_LABEL,
            "ipfs_description": IPFS_DESCRIPTION,
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
            "ipfs_label": IPFS_LABEL,
            "ipfs_description": IPFS_DESCRIPTION,
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
    userId: str | None = None  # user ID for both formats


class IPFSPinJSONRequest(IPFSPinRequest):
    fileName: str
    jsonContent: str


def get_ipfs_config(
    request_user_id: Optional[str] = None,
    ipfs_server: Optional[str] = None,
    custom_headers: Optional[List[Tuple[str, str]]] = None,
) -> Tuple[str, Dict[str, str]]:
    """Get the appropriate IPFS configuration based on request parameters or environment variables."""

    # Use default IPFS settings if not provided
    server_url = ipfs_server or os.getenv("IPFS_RPC_URL") or ""

    # If custom headers are provided, use them (this overrides both formats)
    if ipfs_server and custom_headers:
        return server_url, dict(custom_headers)

    # Handle different authentication formats
    if IPFS_FORMAT == "nmkr":
        # Use Nmkr format (user ID + bearer token)
        user_id = request_user_id or IPFS_USER_ID
        bearer_token = os.getenv("IPFS_BEARER_TOKEN", "")

        # For Nmkr, the URL ends with the user ID
        if not server_url.endswith(f"/{user_id}"):
            server_url = f"{server_url}/{user_id}"

        return server_url, {
            "Authorization": f"Bearer {bearer_token}",
            "Accept": "application/json",
        }
    else:
        # Default to basic auth format
        user_id = request_user_id or IPFS_USER_ID
        password = IPFS_PASSWORD
        auth_string = f"{user_id}:{password}"
        token = base64.b64encode(auth_string.encode("utf-8")).decode("ascii")
        return server_url, {
            "Authorization": f"Basic {token}",
            "Accept": "application/json",
        }


async def pin_to_ipfs_common(
    temp_file_path: Path,
    ipfs_server: Optional[str] = None,
    custom_headers: Optional[List[Tuple[str, str]]] = None,
    user_id: Optional[str] = None,
):
    """Common IPFS pinning logic used by both endpoints."""
    try:
        server_url, headers = get_ipfs_config(user_id, ipfs_server, custom_headers)

        # Different handling based on format
        if IPFS_FORMAT == "nmkr":
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
    headers: Optional[List[Tuple[str, str]]] = None,
    user_id: Optional[str] = None,
):
    """Pin a file to IPFS and return the hash."""
    logger.info("IPFS file pin attempt")

    with tempfile.TemporaryDirectory() as temp_dir:
        temp_file_path = Path(temp_dir) / file.filename  # type: ignore
        content = await file.read()

        with open(temp_file_path, "wb") as f:
            f.write(content)

        return await pin_to_ipfs_common(temp_file_path, ipfs_server, headers, user_id)


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
            request.ipfsServer,
            request.headers,
            request.userId,
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
        response = await app.async_client.request(  # pyright: ignore
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


class LinkedActionIdRequest(BaseModel):
    txId: str
    govActionIx: int


class ResolveLinkedProposalRequest(BaseModel):
    networkId: str | int
    proposalType: str
    linkedActionId: LinkedActionIdRequest
    actionEndEpoch: int | None = None
    anchorJson: dict | str | None = None


class BuildLinkedResponseRequest(BaseModel):
    networkId: str | int
    proposalType: str
    linkedActionId: LinkedActionIdRequest
    actionEndEpoch: int | None = None
    responderRole: str
    surveyTxId: str | None = None
    answers: List[dict]
    anchorJson: dict | str | None = None


@app.post("/survey/resolve-linked-proposal")
async def resolve_survey_link(request: ResolveLinkedProposalRequest):
    try:
        return await resolve_linked_proposal(
            app.async_client,  # pyright: ignore
            network_id=request.networkId,
            proposal_type=request.proposalType,
            linked_action_id=request.linkedActionId.model_dump(),
            action_end_epoch=request.actionEndEpoch,
            anchor_json=request.anchorJson,
        )
    except httpx.HTTPError as error:
        logger.error(f"Linked survey resolution failed: {error}")
        raise HTTPException(
            status_code=502, detail="Failed to resolve linked survey details from Koios."
        )
    except Exception as error:
        logger.error(f"Linked survey resolution failed unexpectedly: {error}")
        raise HTTPException(status_code=500, detail="Failed to resolve linked survey.")


@app.post("/survey/build-linked-response")
async def build_survey_linked_response(request: BuildLinkedResponseRequest):
    try:
        context = await resolve_linked_proposal(
            app.async_client,  # pyright: ignore
            network_id=request.networkId,
            proposal_type=request.proposalType,
            linked_action_id=request.linkedActionId.model_dump(),
            action_end_epoch=request.actionEndEpoch,
            anchor_json=request.anchorJson,
        )
        requested_survey_tx_id = (
            request.surveyTxId.lower() if isinstance(request.surveyTxId, str) else None
        )
        resolved_survey_tx_id = context.get("surveyTxId")
        context_errors: List[str] = []

        if requested_survey_tx_id and resolved_survey_tx_id != requested_survey_tx_id:
            context_errors.append(
                "surveyResponse.surveyTxId does not match the linked survey."
            )

        if not context["linkValidation"]["valid"]:
            context_errors.extend(context["linkValidation"]["errors"])

        if not context["surveyDetailsValidation"]["valid"]:
            context_errors.extend(context["surveyDetailsValidation"]["errors"])

        if context_errors:
            return {
                "valid": False,
                "errors": context_errors,
                "surveyResponse": None,
            }

        response = build_survey_response(
            survey_details=context.get("surveyDetails"),
            linked_role_weighting=context["linkValidation"].get("linkedRoleWeighting"),
            survey_tx_id=resolved_survey_tx_id,
            responder_role=request.responderRole,
            answers=request.answers,
        )
        return response
    except httpx.HTTPError as error:
        logger.error(f"Linked survey response build failed: {error}")
        raise HTTPException(
            status_code=502, detail="Failed to resolve linked survey details from Koios."
        )
    except Exception as error:
        logger.error(f"Linked survey response build failed unexpectedly: {error}")
        raise HTTPException(status_code=500, detail="Failed to build linked survey response.")


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

        full_path = os.path.join(self.directory, path)  # pyright: ignore

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

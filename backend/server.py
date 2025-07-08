import json
import logging
import os
from pathlib import Path
from typing import Optional

import httpx
from pydantic import BaseModel
from dotenv import load_dotenv
from fastapi import FastAPI, HTTPException, Request
from fastapi.responses import HTMLResponse, Response, FileResponse
from fastapi.staticfiles import StaticFiles
from fastapi.templating import Jinja2Templates
from contextlib import asynccontextmanager
from brotli_asgi import BrotliMiddleware

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
            "preconfigured_voters": PRECONFIGURED_VOTERS_JSON,
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
            "preconfigured_voters": PRECONFIGURED_VOTERS_JSON,
            "matomo_script": MATOMO_SCRIPT,
        },
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

# Use a Python image with uv pre-installed to build the backend server
FROM ghcr.io/astral-sh/uv:python3.10-alpine AS backend-builder

# Install the server into `/backend`
WORKDIR /backend

# Get the architecture using ARG
ARG TARGETARCH

# Set the typst compiler download URL based on architecture
RUN if [ "$TARGETARCH" = "amd64" ]; then \
    ARCHIVE_NAME="typst-x86_64-unknown-linux-musl"; \
    elif [ "$TARGETARCH" = "arm64" ]; then \
    ARCHIVE_NAME="typst-aarch64-unknown-linux-musl"; \
    else \
    echo "Unsupported architecture: $TARGETARCH"; \
    exit 1; \
    fi && \
    wget "https://github.com/typst/typst/releases/download/v0.14.2/$ARCHIVE_NAME.tar.xz" -O "$ARCHIVE_NAME.tar.xz" && \
    tar xf "$ARCHIVE_NAME.tar.xz" && \
    mv "${ARCHIVE_NAME}/typst" .

# Copy the typst template for rationales
COPY backend/template.typ template.typ

# Enable bytecode compilation
ENV UV_COMPILE_BYTECODE=1

# Copy from the cache instead of linking since it's a mounted volume
ENV UV_LINK_MODE=copy

# Install the project's dependencies using the lockfile and settings
RUN --mount=type=cache,target=/root/.cache/uv \
    --mount=type=bind,source=backend/uv.lock,target=uv.lock \
    --mount=type=bind,source=backend/pyproject.toml,target=pyproject.toml \
    uv sync --frozen --no-install-project --no-dev

# Then, add the rest of the project source code and install it
# Installing separately from its dependencies allows optimal layer caching
ADD backend .
RUN --mount=type=cache,target=/root/.cache/uv \
    uv sync --frozen --no-dev

# ---------------------------------------------------------

# Use a Node image with npm pre-installed to build the frontend Elm app
FROM node:lts AS frontend-builder

# Install brotli to compress static files
RUN apt update && apt install -y brotli

# Install the dev dependencies, Elm, etc.
WORKDIR /frontend
RUN --mount=type=bind,source=frontend/package.json,target=package.json \
    --mount=type=bind,source=frontend/package-lock.json,target=package-lock.json \
    npm ci

# Then add the code and build the frontend app
ENV ELM_HOME=elm-home/elm-stuff
ADD frontend .
RUN npm run vdom:patch
RUN npm run make
RUN npm run css
RUN npm run compress

# ---------------------------------------------------------

# Then use the final image for the server without uv
# and with the compiled frontend app into /frontend/static/
FROM python:3.10-alpine
# It is important to use the image that matches the builder,
# as the path to the Python executable must be the same.

# Copy the python server code
COPY --from=backend-builder --chown=backend:backend /backend/.venv /backend/.venv
COPY --from=backend-builder /backend/server.py /backend/server.py

# Copy the typst compiler and template
COPY --from=backend-builder /backend/typst /backend/.venv/bin/typst
COPY --from=backend-builder /backend/template.typ /backend/template.typ

# Copy the compiled frontend code
COPY --from=frontend-builder /frontend/static /frontend/static

# Run the FastAPI server
# Uses `--host 0.0.0.0` to allow access from outside the container
WORKDIR /backend
ENV PATH="$PATH:/backend/.venv/bin"
CMD ["fastapi", "run", "--host", "0.0.0.0", "server.py"]

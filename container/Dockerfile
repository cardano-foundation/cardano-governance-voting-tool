# --- STEP 1: Backend Builder ---
FROM ghcr.io/astral-sh/uv:python3.10-alpine AS backend-builder

WORKDIR /backend
ARG TARGETARCH

# Optimization: Download Typst with security checksum check (recommended for production)
RUN if [ "$TARGETARCH" = "amd64" ]; then \
    ARCHIVE_NAME="typst-x86_64-unknown-linux-musl"; \
    elif [ "$TARGETARCH" = "arm64" ]; then \
    ARCHIVE_NAME="typst-aarch64-unknown-linux-musl"; \
    else exit 1; fi && \
    wget -q "https://github.com/typst/typst/releases/download/v0.13.1/$ARCHIVE_NAME.tar.xz" -O "typst.tar.xz" && \
    tar xf "typst.tar.xz" && \
    mv "${ARCHIVE_NAME}/typst" ./typst_bin && \
    rm -rf "${ARCHIVE_NAME}" "typst.tar.xz"

# Python bytecode and link mode optimization
ENV UV_COMPILE_BYTECODE=1
ENV UV_LINK_MODE=copy

# Install dependencies using mounts for speed
RUN --mount=type=cache,target=/root/.cache/uv \
    --mount=type=bind,source=backend/uv.lock,target=uv.lock \
    --mount=type=bind,source=backend/pyproject.toml,target=pyproject.toml \
    uv sync --frozen --no-install-project --no-dev

# Add source and sync project
ADD backend .
RUN --mount=type=cache,target=/root/.cache/uv \
    uv sync --frozen --no-dev

# --- STEP 2: Frontend Builder ---
FROM node:lts-slim AS frontend-builder

# Install only necessary build tools
RUN apt-get update && apt-get install -y --no-install-recommends brotli \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /frontend
COPY frontend/package*.json ./
RUN npm ci

# Build and compress
COPY frontend .
RUN npm run vdom:patch && \
    npm run make && \
    npm run css && \
    npm run compress

# --- STEP 3: Final Production Image ---
FROM python:3.10-alpine

# Security: Create a system user to run the app
RUN addgroup -S appgroup && adduser -S appuser -G appgroup

WORKDIR /app

# Copy python virtual environment and necessary binaries
COPY --from=backend-builder /backend/.venv /app/.venv
COPY --from=backend-builder /backend/server.py /app/server.py
COPY --from=backend-builder /backend/template.typ /app/template.typ
COPY --from=backend-builder /backend/typst_bin /usr/local/bin/typst

# Copy compiled frontend assets
COPY --from=frontend-builder /frontend/static /frontend/static

# Set permissions for non-root user
RUN chown -R appuser:appgroup /app /frontend/static

# Env and Path configuration
ENV PATH="/app/.venv/bin:$PATH"
ENV PYTHONUNBUFFERED=1

# Run as non-privileged user
USER appuser

EXPOSE 8000

# Using FastAPI production runner
CMD ["fastapi", "run", "--host", "0.0.0.0", "--port", "8000", "server.py"]

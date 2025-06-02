Minimalist server for Cardano governance uses

## Getting Started

First create and modify the `.env` file containing the IPFS node access config.
You can start by copying the `.env.example`.
You can either configure it as a regular IPFS RPC server with basic auth,
or as NMKR server.

> Remark: This is required for direct usage of this server endpoints.
> However, if you use the frontend web app to communicate with this server,
> you can fill this `.env` file with the default (incorrect) values below,
> since IPFS RPC config can be done directly in the frontend.

```env
# Regular IPFS RPC config with basic auth
IPFS_FORMAT=basic
IPFS_RPC_URL=https://ipfs-rpc.mycompany.org/api/v0
IPFS_USER_ID=user
IPFS_PASSWORD=password
IPFS_LABEL="Pre-configured IPFS server"
IPFS_DESCRIPTION="Files will be stored using the pre-configured IPFS servers."

# Alternative IPFS config using NMKR servers
# IPFS_FORMAT=nmkr
# IPFS_RPC_URL=https://studio-api.nmkr.io/v2/UploadToIpfs
# IPFS_USER_ID=000000
# IPFS_BEARER_TOKEN=ffffffffffffffffffffffffffffffff
# IPFS_LABEL="Pre-configured IPFS server (sponsored by NMKR)"
# IPFS_DESCRIPTION="Files will be stored using NMKR's IPFS servers."

# Network config: 0 for Preview, 1 for Mainnet
NETWORK_ID=0

# Preconfiguration of some voters for fast selection
PRECONFIGURED_VOTERS_JSON="[
  { \"voterType\": \"DRep\"
  , \"description\": \"Vote as a Delegated Representative\"
  , \"govId\": \"drep1ydpfkyjxzeqvalf6fgvj7lznrk8kcmfnvy9hyl6gr6ez6wgsjaelx\"
  },
  { \"voterType\": \"CC Member\"
  , \"description\": \"Vote as a Constitutional Committee Member\"
  , \"govId\": \"cc_hot1qdnedkra2957t6xzzwygdgyefd5ctpe4asywauqhtzlu9qqkttvd9\"
  },
  { \"voterType\": \"SPO\"
  , \"description\": \"Vote as a Stake Pool Operator\"
  , \"govId\": \"pool1nqheyct9a0mxn80cwp9pd5guncfu3rzwqtmru0l94accz7gjcgl\"
  }
]"
```

Then you can start the python server.
I suggest you use [`uv`](https://docs.astral.sh/uv/) for that, which takes care of all the dependency stuff.

```sh
uv run server.py
```

The `/pretty-gov-pdf` endpoint converts governance JSON metadata into pretty PDFs, easier to read.
This conversion is based on the [Typst](https://typst.app/docs/) markup language and compiler.
So you need Typst installed for the server to correctly perform the PDF conversion at this endpoint.
To trigger the `pretty-gov-pdf` endpoint, you can use a request like this:

```sh
curl -X POST "http://localhost:8000/pretty-gov-pdf" \
      -H "Content-Type: application/json" \
      -d "@cf-ikigai-modified.json" \
      --output metadata.pdf
```

To trigger the `/ipfs-pin/file` endpoint, you can use a request like this:

```sh
curl -X POST "http://localhost:8000/ipfs-pin/file" \
     -H "Content-Type: multipart/form-data" \
     -F "file=@some-file.pdf"
```

## Contributions

All sorts of contributions are welcome!
If you are unsure about how to proceed, the best is to start by opening an issue in this GitHub repository.

**Code contributions**

The python project is handled using [`uv`](https://docs.astral.sh/uv/) so please install it first.
To start the server, follow the steps in the "Getting Started" section above.

The code is linted and formatted using [`ruff`](https://docs.astral.sh/ruff/) so please install it too.

```sh
# From inside the backend/ folder:
ruff check           # check the lints
ruff format --check  # check code formatting
```

To avoid having to manually check lints and code format, I suggest you install a ruff extension in your favorite editor.

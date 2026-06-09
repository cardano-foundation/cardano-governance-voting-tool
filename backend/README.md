Minimalist server for Cardano governance uses

## Getting Started

First create and modify the `.env` file containing the IPFS node access config.
You can start by copying the `.env.example`, which documents every variable.
IPFS providers are configured through `IPFS_PRECONFIGS_JSON`, a JSON array of
pre-configured providers offered to users; each entry's `format` can be
`basic` (RPC + basic auth), `nmkr`, or `blockfrost`.

> Remark: This is required for direct usage of this server endpoints.
> However, if you use the frontend web app to communicate with this server,
> you can fill this `.env` file with the default (incorrect) values below,
> since IPFS RPC config can be done directly in the frontend.

```env
# IPFS preconfigs: a JSON array of pre-configured IPFS providers the frontend
# will offer to users. Each entry needs id, label, description, and format
# ("basic" | "nmkr" | "blockfrost"); per-format fields:
#   basic      : rpcUrl, userId, password
#   nmkr       : rpcUrl, userId, bearerToken
#   blockfrost : projectId (rpcUrl optional; filecoin optional)
# Only id/label/description reach the frontend; secrets stay server-side.
# See .env.example for a multi-provider example.
IPFS_PRECONFIGS_JSON="[
  { \"id\": \"default\"
  , \"label\": \"Pre-configured IPFS server\"
  , \"description\": \"Files will be stored using the pre-configured IPFS servers.\"
  , \"format\": \"basic\"
  , \"rpcUrl\": \"https://ipfs-rpc.mycompany.org/api/v0\"
  , \"userId\": \"user\"
  , \"password\": \"password\"
  }
]"

# Network config: 0 for Preview, 1 for Mainnet
NETWORK_ID=0

# Matomo Analytics (optional - can be removed for self-hosted deployments)
MATOMO_URL="https://cardanofoundation.matomo.cloud/"
MATOMO_SITE_ID="13"
MATOMO_JS_URL="https://cdn.matomo.cloud/cardanofoundation.matomo.cloud/matomo.js"
MATOMO_SCRIPT='
      var _paq = window._paq = window._paq || [];
      _paq.push(["trackPageView"]);
      _paq.push(["enableLinkTracking"]);
      (function() {
        var u="MATOMO_URL_PLACEHOLDER";
        _paq.push(["setTrackerUrl", u+"matomo.php"]);
        _paq.push(["setSiteId", "MATOMO_SITE_ID_PLACEHOLDER"]);
        var d=document, g=d.createElement("script"), s=d.getElementsByTagName("script")[0];
        g.async=true; g.src="MATOMO_JS_URL_PLACEHOLDER"; s.parentNode.insertBefore(g,s);
      })();
'

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

# Preconfigured authors for rationale signatures (private instance helper)
# Authors names are supported for pre-configuration.
# Format: { "name": "Author Name" }
PRECONFIGURED_AUTHORS_JSON="[
  { \"name\": \"Cardano Foundation\" }
]"
```

### Analytics Configuration

The application includes optional Matomo analytics integration. All Matomo-related environment variables are optional and can be omitted for self-hosted deployments or if you prefer not to use analytics.

- `MATOMO_URL`: Base URL of your Matomo instance
- `MATOMO_SITE_ID`: Your site ID in Matomo
- `MATOMO_JS_URL`: URL to the Matomo JavaScript file
- `MATOMO_SCRIPT`: Complete Matomo tracking script with placeholders

The placeholders in `MATOMO_SCRIPT` (`MATOMO_URL_PLACEHOLDER`, `MATOMO_SITE_ID_PLACEHOLDER`, `MATOMO_JS_URL_PLACEHOLDER`) will be automatically replaced with the corresponding environment variable values.

If any of these variables are not set, analytics will be disabled.

### Open Graph Social Cards

When a shared link pre-selects a proposal (`/page/...?proposalId=...&networkId=...`),
the server injects Open Graph / Twitter meta tags so the link unfurls with the
proposal title and a generated image. The image is a 1200×630 JPEG drawing the
proposal title over the brand-gradient background; it is rendered once with
Pillow and cached on disk, then served as a static file on repeat crawls. When
the title can't be resolved (or rendering is unavailable) it falls back to the
generic `/logo/og-image.jpg` card.

`og:image` URLs must be absolute. By default the server derives the origin
(scheme + host) from the incoming request — honoring `X-Forwarded-Proto` /
`X-Forwarded-Host` behind a reverse proxy — so a self-hosted deployment works on
its own domain with no configuration. All related variables are optional:

- `OG_BASE_URL`: force a specific origin for `og:` URLs (e.g. `https://votes.example.org`).
  Only needed when the public origin differs from what the app sees (e.g. a CDN
  host distinct from the app's own host). Leave unset to auto-derive per request.
- `OG_CARD_CACHE_DIR`: directory for the rendered card cache (default: a
  `og-cards/` folder under the system temp dir).
- `KOIOS_API_TOKEN`: override the Koios token used to resolve proposal titles
  (a public free-tier token is used by default).

Rendering needs Pillow (a project dependency) and a TrueType font. The Docker
image bundles one via the `font-dejavu` package; without a usable font the
server logs a warning and serves the generic image instead.

## Running the Server

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

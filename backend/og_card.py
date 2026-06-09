"""Render a per-proposal Open Graph card (1200x630 JPEG).

The background is a static, pre-rendered WebP (`assets/og/card-bg.webp`)
produced from `assets/og/card-bg.svg` — a clean white canvas with two blurred
brand gradient blobs mirroring the home-page hero. Per proposal we load that
image, draw the title on top, and JPEG-encode. Keeping rasterization at
authoring time means the runtime needs only Pillow (no Cairo/SVG renderer in the
container); WebP keeps the smooth gradient at ~7kB (vs ~230kB as PNG).

Resource notes:
  - The background is loaded once and memoized (`_background`), then copied per
    card — the only per-request work is drawing the title text and JPEG encoding
    (~10-30ms), which the caller runs in a thread.
  - Fonts are memoized per (size, weight). If no TTF is found we fall back to a
    bitmap font and log loudly; bundle a TTF under backend/assets/og/ (or set
    OG_FONT_BOLD / OG_FONT_REGULAR) for production-quality cards.
"""

import io
import logging
import os
from functools import lru_cache
from pathlib import Path

from PIL import Image, ImageDraw, ImageFont

logger = logging.getLogger(__name__)

CARD_W, CARD_H = 1200, 630
MARGIN = 90

TITLE_COLOR = (26, 26, 32)
EYEBROW_COLOR = (0, 132, 255)
FOOTER_COLOR = (120, 120, 130)

ASSETS = Path(__file__).parent / "assets" / "og"
BG_PATH = ASSETS / "card-bg.webp"

# Bundled font first, then common system locations, then bitmap fallback.
# Alpine's `font-dejavu` package installs under /usr/share/fonts/dejavu/;
# Debian/Ubuntu under /usr/share/fonts/truetype/dejavu/; macOS ships Helvetica.
_FONT_BOLD = [
    os.getenv("OG_FONT_BOLD", ""),
    str(ASSETS / "font-bold.ttf"),
    "/usr/share/fonts/dejavu/DejaVuSansCondensed-Bold.ttf",
    "/usr/share/fonts/truetype/dejavu/DejaVuSansCondensed-Bold.ttf",
    "/usr/share/fonts/dejavu/DejaVuSans-Bold.ttf",
    "/usr/share/fonts/truetype/dejavu/DejaVuSans-Bold.ttf",
    "/System/Library/Fonts/Helvetica.ttc",
]
_FONT_REGULAR = [
    os.getenv("OG_FONT_REGULAR", ""),
    str(ASSETS / "font-regular.ttf"),
    "/usr/share/fonts/dejavu/DejaVuSansCondensed.ttf",
    "/usr/share/fonts/truetype/dejavu/DejaVuSansCondensed.ttf",
    "/usr/share/fonts/dejavu/DejaVuSans.ttf",
    "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf",
    "/System/Library/Fonts/Helvetica.ttc",
]


@lru_cache(maxsize=8)
def _font(size: int, bold: bool) -> "ImageFont.FreeTypeFont | ImageFont.ImageFont":
    for path in _FONT_BOLD if bold else _FONT_REGULAR:
        if path and os.path.exists(path):
            try:
                return ImageFont.truetype(path, size)
            except Exception:
                continue
    logger.warning("OG: no TTF font found; using bitmap fallback (cards will look poor)")
    return ImageFont.load_default()


@lru_cache(maxsize=1)
def _background() -> Image.Image:
    bg = Image.open(BG_PATH).convert("RGB")
    if bg.size != (CARD_W, CARD_H):
        bg = bg.resize((CARD_W, CARD_H))
    return bg


def _wrap(draw, text, font, max_width, max_lines):
    """Greedy word-wrap; ellipsize the last line if the text overflows."""
    words = text.split()
    lines, cur, i = [], "", 0
    while i < len(words):
        trial = f"{cur} {words[i]}".strip()
        if cur and draw.textlength(trial, font=font) > max_width:
            lines.append(cur)
            cur = ""
            if len(lines) == max_lines:
                break
        else:
            cur = trial
            i += 1
    if cur and len(lines) < max_lines:
        lines.append(cur)
        i = len(words)

    if i < len(words) and lines:
        last = lines[-1]
        while last and draw.textlength(last + "…", font=font) > max_width:
            last = last.rsplit(" ", 1)[0] if " " in last else last[:-1]
        lines[-1] = last + "…"
    return lines


def _fit_title(draw, text, max_width, max_lines):
    """Pick the largest font size that fits without truncation, if possible."""
    font, lines = _font(48, True), [text]
    for size in (72, 64, 56, 48):
        font = _font(size, bold=True)
        lines = _wrap(draw, text, font, max_width, max_lines)
        if not lines[-1].endswith("…"):
            return font, lines, size
    return font, lines, 48


def render_proposal_card(title: str) -> bytes:
    img = _background().copy()
    draw = ImageDraw.Draw(img)
    max_width = CARD_W - 2 * MARGIN

    draw.text(
        (MARGIN, MARGIN),
        "CARDANO GOVERNANCE VOTING",
        font=_font(30, bold=True),
        fill=EYEBROW_COLOR,
    )

    title_font, lines, size = _fit_title(draw, title, max_width, max_lines=4)
    line_h = int(size * 1.18)
    y = (CARD_H - line_h * len(lines)) // 2
    for line in lines:
        draw.text((MARGIN, y), line, font=title_font, fill=TITLE_COLOR)
        y += line_h

    draw.text(
        (MARGIN, CARD_H - MARGIN - 28),
        "voting.cardanofoundation.org",
        font=_font(28, bold=False),
        fill=FOOTER_COLOR,
    )

    buf = io.BytesIO()
    img.save(buf, format="JPEG", quality=86, optimize=True)
    return buf.getvalue()

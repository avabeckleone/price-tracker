#!/usr/bin/env python3
"""
Amazon price tracker (manual run).

Reads product URLs from urls.txt, opens each in a browser, saves a full-page
screenshot, extracts the price, and appends a row to prices.csv.

Usage:
    python price_tracker.py                # run headless
    python price_tracker.py --show         # show the browser window (helps if you hit CAPTCHAs)
    python price_tracker.py --urls my.txt  # use a different URL file

Setup (one time):
    pip install playwright
    playwright install chromium
"""

import argparse
import csv
import random
import re
import sys
import tempfile
import time
from datetime import datetime
from pathlib import Path

from playwright.sync_api import TimeoutError as PlaywrightTimeout
from playwright.sync_api import sync_playwright

BASE_DIR = Path(__file__).resolve().parent
SCREENSHOT_DIR = BASE_DIR / "screenshots"
CSV_PATH = BASE_DIR / "prices.csv"
CSV_FIELDS = ["timestamp", "asin", "title", "price", "regular_price", "non_deal_price", "prime_exclusive", "status", "screenshot", "url"]

# Tried in order; first one that returns text wins.
PRICE_SELECTORS = [
    "#corePrice_feature_div span.a-offscreen",
    "#corePriceDisplay_desktop_feature_div span.a-offscreen",
    "span.priceToPay span.a-offscreen",
    "#apex_desktop span.a-offscreen",
    "#priceblock_dealprice",
    "#priceblock_ourprice",
    "span.a-price span.a-offscreen",
]

REGULAR_PRICE_SELECTORS = [
    "#corePrice_feature_div .basisPrice .a-offscreen",
    "#corePriceDisplay_desktop_feature_div .basisPrice .a-offscreen",
    "#corePrice_feature_div .basisPrice",
    "#corePriceDisplay_desktop_feature_div .basisPrice",
    "#corePrice_feature_div .a-text-price .a-offscreen",
    "#corePriceDisplay_desktop_feature_div .a-text-price .a-offscreen",
    "#price .a-text-price .a-offscreen",
]


def find_non_deal_price(page) -> str:
    # Amazon may show a separate selectable Non-Deal Price offer beside a deal.
    try:
        label = page.get_by_text("Non-Deal Price", exact=False).first
        return label.evaluate("""el => {
            let node = el;
            for (let i = 0; node && i < 7; i++, node = node.parentElement) {
                const price = node.querySelector(".a-price .a-offscreen");
                if (price && price.textContent.trim()) return price.textContent.trim();
            }
            return "";
        }""") or ""
    except Exception:
        return ""


PRIME_EXCLUSIVE_MARKERS = [
    "prime member exclusive", "prime exclusive deal",
    "prime members", "prime-only deal",
]

ACCESS_CHALLENGE_MARKERS = [
    "Enter the characters you see below",
    "Type the characters you see in this image",
    "Sorry, we just need to make sure you're not a robot",
    "/errors/validateCaptcha",
    "robot check",
    "automated access",
    "validatecaptcha",
    "Click the button below to continue shopping",
]


def load_urls(path: Path) -> list[str]:
    if not path.exists():
        sys.exit(f"Couldn't find {path}. Create it with one Amazon URL per line.")
    urls = []
    for line in path.read_text().splitlines():
        line = line.strip()
        if line and not line.startswith("#"):
            urls.append(line)
    if not urls:
        sys.exit(f"No URLs found in {path}.")
    return urls


def get_asin(url: str) -> str:
    match = re.search(r"/(?:dp|gp/product|gp/aw/d)/([A-Z0-9]{10})", url)
    return match.group(1) if match else "unknown"


def first_text(page, selectors: list[str]) -> str:
    for sel in selectors:
        try:
            el = page.query_selector(sel)
            if el:
                text = (el.text_content() or "").strip()
                if text:
                    return text
        except Exception:
            continue
    return ""


def screenshot_basename(title: str, asin: str, captured_at: datetime) -> str:
    # Use the product name in the screenshot filename; strip characters
    # that are awkward or invalid in common filesystems.
    safe_title = re.sub(r"[^\w.-]+", "_", title, flags=re.UNICODE).strip("._-")
    safe_title = re.sub(r"_+", "_", safe_title)[:100] or asin
    return f"{safe_title}_{captured_at.strftime('%Y%m%d_%H%M%S')}.png"


def is_access_challenge(page) -> bool:
    # Amazon's challenge pages vary by region and change their wording. Check
    # both the final URL and rendered page text, case-insensitively.
    try:
        if "validatecaptcha" in page.url.lower():
            return True
        page_text = page.locator("body").inner_text(timeout=3000)
    except Exception:
        return False
    page_text = page_text.lower()
    return any(marker.lower() in page_text for marker in ACCESS_CHALLENGE_MARKERS)


def append_row(row: dict) -> None:
    if CSV_PATH.exists() and CSV_PATH.stat().st_size:
        with CSV_PATH.open("r", newline="", encoding="utf-8") as f:
            reader = csv.DictReader(f)
            old_fields = reader.fieldnames or []
            if old_fields != CSV_FIELDS:
                old_rows = list(reader)
                with tempfile.NamedTemporaryFile("w", newline="", encoding="utf-8", dir=CSV_PATH.parent, delete=False) as temp:
                    writer = csv.DictWriter(temp, fieldnames=CSV_FIELDS)
                    writer.writeheader()
                    writer.writerows(old_rows)
                    temp_path = Path(temp.name)
                temp_path.replace(CSV_PATH)
    new_file = not CSV_PATH.exists() or CSV_PATH.stat().st_size == 0
    with CSV_PATH.open("a", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=CSV_FIELDS)
        if new_file:
            writer.writeheader()
        writer.writerow(row)


def check_url(page, url: str) -> dict:
    now = datetime.now()
    asin = get_asin(url)
    row = {
        "timestamp": now.strftime("%Y-%m-%d %H:%M:%S"),
        "asin": asin,
        "title": "",
        "price": "",
        "regular_price": "",
        "non_deal_price": "",
        "prime_exclusive": "unknown",
        "status": "",
        "screenshot": "",
        "url": url,
    }

    try:
        response = page.goto(url, wait_until="domcontentloaded", timeout=45000)
        page.wait_for_timeout(2500)  # let price render

        access_challenge = is_access_challenge(page)
        row["title"] = first_text(page, ["#productTitle"])
        row["price"] = "" if access_challenge else first_text(page, PRICE_SELECTORS)
        if not access_challenge and row["title"]:
            row["regular_price"] = first_text(page, REGULAR_PRICE_SELECTORS)
            row["non_deal_price"] = find_non_deal_price(page)
            try:
                page_text = (page.locator("body").inner_text() or "").lower()
                row["prime_exclusive"] = (
                    "yes" if any(marker in page_text for marker in PRIME_EXCLUSIVE_MARKERS)
                    else "no"
                )
            except Exception:
                pass

        if response and response.status >= 400:
            row["status"] = f"http_{response.status}"
        elif access_challenge:
            row["status"] = "access_challenge"
        elif not row["title"]:
            # A blank title across many products usually means an interstitial,
            # challenge, or unexpected page layout, not 19 genuinely missing prices.
            row["status"] = "page_unrecognized"
        elif row["price"]:
            row["status"] = "ok"
        else:
            row["status"] = "price_not_found"

        shot_name = screenshot_basename(row["title"], asin, now)
        shot_path = SCREENSHOT_DIR / shot_name
        page.screenshot(path=str(shot_path), full_page=True)
        row["screenshot"] = str(shot_path.relative_to(BASE_DIR))

    except PlaywrightTimeout:
        row["status"] = "timeout"
    except Exception as e:
        row["status"] = f"error: {type(e).__name__}"

    return row


def main() -> None:
    parser = argparse.ArgumentParser(description="Screenshot Amazon pages and log prices.")
    parser.add_argument("--urls", default=str(BASE_DIR / "urls.txt"), help="File with one URL per line")
    parser.add_argument("--show", action="store_true", help="Show the browser window")
    args = parser.parse_args()

    urls = load_urls(Path(args.urls))
    SCREENSHOT_DIR.mkdir(exist_ok=True)

    with sync_playwright() as p:
        browser = p.chromium.launch(headless=not args.show)
        context = browser.new_context(
            viewport={"width": 1366, "height": 900},
            locale="en-CA",
            user_agent=(
                "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 "
                "(KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36"
            ),
        )
        page = context.new_page()

        for i, url in enumerate(urls, start=1):
            print(f"[{i}/{len(urls)}] {url}")
            row = check_url(page, url)
            append_row(row)
            print(f"    status={row['status']}  price={row['price'] or '-'}  title={row['title'][:60]}")
            if row["status"] in {"access_challenge", "page_unrecognized"} or row["status"].startswith("http_"):
                try:
                    print(f"    page_title={page.title()[:100]}  final_url={page.url[:180]}")
                except Exception:
                    pass

            if i < len(urls):
                time.sleep(random.uniform(4, 9))  # be polite, avoid hammering

        browser.close()

    print(f"\nDone. Prices logged to {CSV_PATH}")
    print(f"Screenshots saved in {SCREENSHOT_DIR}")


if __name__ == "__main__":
    main()

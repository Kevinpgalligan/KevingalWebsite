import requests
from ratelimit import limits, sleep_and_retry

REQUEST_WAIT_SECONDS = 10

# Rate-limiting to prevent blacklisting by
# azlyrics. 10 seconds is probably overly
# conservative, but for ~400 songs it still
# took only ~1 hour to download all of the
# lyrics.
@sleep_and_retry
@limits(calls=1, period=REQUEST_WAIT_SECONDS)
def get_page(link):
    try:
        return requests.get(link)
    except Exception as e:
        print(f"Failed to get page {link}, exception:")
        print(e)
        return None

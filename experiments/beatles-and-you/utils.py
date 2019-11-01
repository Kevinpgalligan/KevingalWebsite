import requests
from ratelimit import limits, sleep_and_retry
from bs4 import BeautifulSoup
from retry import retry

REQUEST_WAIT_SECONDS = 10

# Rate-limiting to prevent blacklisting by
# azlyrics. 10 seconds is probably overly
# conservative, but for ~400 songs it still
# took only ~1 hour to download all of the
# lyrics.
@retry(requests.exceptions.RequestException, tries=3) # robustness
@sleep_and_retry                                      # rate-limiting behaviour
@limits(calls=1, period=REQUEST_WAIT_SECONDS)
def get_page(link):
    response = requests.get(link)
    if response.status_code == 200:
        return response
    if response.status_code == 404:
        return None
    raise Exception(f"Failed to get page {link}, got HTTP status code {response.status_code}.")

def get_lyrics(song_link):
    """Get lyrics from azlyrics.com at the given link.

    Returns None if the page doesn't exist, otherwise it'll bubble up
    an exception.
    """
    lyrics_page = get_page(song_link)
    if lyrics_page is None:
        return None
    soup = BeautifulSoup(lyrics_page.content, 'html.parser')
    lyrics_div = soup.find("div", class_="ringtone").find_next_sibling("div")
    return lyrics_div.text.strip()

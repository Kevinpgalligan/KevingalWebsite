import requests
from bs4 import BeautifulSoup
import progressbar
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

def get_lyrics(song_link):
    lyrics_page = get_page(song_link)
    if lyrics_page is None:
        return None
    soup = BeautifulSoup(lyrics_page.content, 'html.parser')
    lyrics_div = soup.find("div", class_="ringtone").find_next_sibling("div")
    return lyrics_div.text.strip()

def you_in_lyrics(lyrics):
    return "you" in lyrics.split("\n")[0].lower()

def main():
    beatles_songs_page = get_page("https://www.azlyrics.com/b/beatles.html")
    if beatles_songs_page is None:
        print("Failed to get list of songs.")
        return
    soup = BeautifulSoup(beatles_songs_page.content, 'html.parser')
    song_links = []
    for link in soup.find_all('a'):
        ref = link.get('href')
        if "lyrics/beatles" in ref:
            song_links.append(ref.replace("..", "https://www.azlyrics.com"))

    # Smarter would be to save progress, just in case the
    # program is interrupted mid-execution.
    songs_count = 0
    you_songs_count = 0
    for song_link in progressbar.progressbar(song_links):
        lyrics = get_lyrics(song_link)
        if lyrics is not None:
            songs_count += 1
            if you_in_lyrics(lyrics):
                you_songs_count += 1

    print(f"Total songs: {songs_count}, total 'you' songs: {you_songs_count}.")

if __name__ == "__main__":
    main()

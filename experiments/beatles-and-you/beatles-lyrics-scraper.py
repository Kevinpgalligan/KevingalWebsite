from bs4 import BeautifulSoup
import progressbar
import re
import os
import sys
from utils import get_page, get_lyrics

ALBUMS = [
    "Please Please Me",
    "With The Beatles",
    "A Hard Day's Night",
    "Beatles For Sale",
    "Help!",
    "Rubber Soul",
    "Revolver",
    "Sgt. Pepper's Lonely Hearts Club Band",
    "Magical Mystery Tour",
    "The Beatles (The White Album)",
    "Yellow Submarine",
    "Abbey Road",
    "Let It Be"
]

SONG_ID_REGEX = re.compile(r"/([a-zA-Z0-9-]+)\.html")

def main():
    beatles_songs_page = get_page("https://www.azlyrics.com/b/beatles.html")
    if beatles_songs_page is None:
        print("Failed to get list of songs.")
        sys.exit(1)
    soup = BeautifulSoup(beatles_songs_page.content, 'html.parser')
    album_headers = [header for header in soup.find_all("div", class_="album")
                     if any(album.lower() in header.text.lower() for album in ALBUMS)]
    if len(album_headers) != len(ALBUMS):
        print("Failed to find all album headers.")
        sys.exit(1)

    song_links = []
    for header in album_headers:
        for e in header.next_siblings:
            if isinstance(e, str):
                continue
            if e.name == "a":
                ref = e.get("href")
                if "lyrics/beatles" in ref:
                    song_links.append(ref.replace("..", "https://www.azlyrics.com"))
            if e.name == "div":
                # Have hit the next album, stop.
                break
    print(f"Num songs: {len(song_links)}")

    os.makedirs("/tmp/beatles/", exist_ok=True)

    for song_link in progressbar.progressbar(song_links):
        song_id_match = SONG_ID_REGEX.search(song_link)
        if song_id_match is None:
            print("Failed to find ID of a song from its link:", song_link)
            sys.exit(1)
        song_file_path = f"/tmp/beatles/{song_id_match.group(1)}"
        if os.path.isfile(song_file_path):
            continue # already downloaded
        lyrics = get_lyrics(song_link)
        if lyrics is not None:
            song_file = open(song_file_path, "w")
            song_file.write(lyrics)
            song_file.close()

if __name__ == "__main__":
    main()

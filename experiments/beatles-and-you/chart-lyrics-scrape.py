import os
from utils import get_lyrics
import re
import progressbar

DIR = "/tmp/music-charts/"
MISSING_LYRICS_FILE_NAME = "missing-lyrics.txt"
INVALID_CHARS_REGEX = "[^A-Za-z0-9]"

def main():
    chart_file_names = [f"{DIR}{f}" for f in os.listdir(DIR)]
    top_position_by_song = {}
    for name in chart_file_names:
        content = open(name, "r").read()
        songs = parse_chart_file(content)
        for (position, name, artist) in songs:
            tag = (name, artist)
            if tag not in top_position_by_song:
                top_position_by_song[tag] = position
            else:
                top_position_by_song[tag] = min(position, top_position_by_song[tag])

    os.makedirs("/tmp/chart-lyrics/", exist_ok=True)
    if os.path.exists(MISSING_LYRICS_FILE_NAME): os.remove(MISSING_LYRICS_FILE_NAME)

    with open(MISSING_LYRICS_FILE_NAME, "w") as missing_lyrics_file:
        for (name, artist), top_position in progressbar.progressbar(list(top_position_by_song.items())):
            lyrics_file_name = f"/tmp/chart-lyrics/{top_position};{name};{artist}"
            if os.path.exists(lyrics_file_name):
                # Already have these lyrics.
                continue
            song_link = to_az_lyrics_link(name, artist)
            lyrics = get_lyrics(song_link)
            if lyrics is None:
                # For debugging.
                missing_lyrics_file.write(f"{name};{artist};{song_link}\n")
            else:
                f = open(lyrics_file_name, "w")
                f.write(lyrics)
                f.close()

def parse_chart_file(content):
    return [(int(attrs[0]), attrs[1], attrs[2])
        for attrs in [line.split(";") for line in content.split("\n")]]

def to_az_lyrics_link(name, artist):
    return "https://www.azlyrics.com/lyrics/{}/{}.html".format(
        re.sub(INVALID_CHARS_REGEX, "", artist.lstrip("The ")).lower(),
        re.sub(INVALID_CHARS_REGEX, "", name).lower())

if __name__ == "__main__":
    main()

import os

LYRICS_DIR = "/tmp/beatles/"

def first_line(lyrics):
    return lyrics.split("\n")[0]

def main():
    lyrics_file_paths = [os.path.join(LYRICS_DIR, p) for p in os.listdir(LYRICS_DIR)]
    all_lyrics = [open(p, "r").read() for p in lyrics_file_paths]
    you_count = 0
    you_first_line_count = 0
    for lyrics in all_lyrics:
        if "you" in lyrics.lower():
            you_count += 1
        if "you" in first_line(lyrics.lower()):
            you_first_line_count += 1
    print(f"Songs: {len(all_lyrics)}")
    print(f"You percentage: {100. * you_count / len(all_lyrics)}%")
    print(f"You in first line percentage: {100. * you_first_line_count / len(all_lyrics)}%")

if __name__ == "__main__":
    main()

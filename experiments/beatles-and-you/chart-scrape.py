from bs4 import BeautifulSoup
from utils import get_page
from datetime import date, timedelta
import os
import progressbar

START_DATE = date.fromisoformat("1962-01-06")
END_DATE = date.fromisoformat("1970-01-01")

def get_top100_chart(d):
    pg = get_page(f"https://www.billboard.com/charts/hot-100/{d.isoformat()}")
    if pg is None:
        return None
    soup = BeautifulSoup(pg.content, "html.parser")
    names = [e.text for e in soup.find_all("span", class_="chart-element__information__song text--truncate color--primary")]
    artists = [e.text for e in soup.find_all("span", class_="chart-element__information__artist text--truncate color--secondary")]
    assert len(names) == 100
    assert len(artists) == 100
    return [(str(i+1), name, artist) for i, (name, artist) in enumerate(zip(names, artists))]

def main():
    os.makedirs("/tmp/music-charts/", exist_ok=True)

    dates = []
    d = START_DATE
    while d < END_DATE:
        dates.append(d)
        d += timedelta(days=7)

    for d in progressbar.progressbar(dates):
        chart_file_path = f"/tmp/music-charts/{d.isoformat()}"
        if os.path.isfile(chart_file_path):
            continue # already downloaded
        songs = get_top100_chart(d)
        if songs is None:
            print(f"Failed to download chart for year {d.isoformat()}.")
            continue
        chart_file = open(chart_file_path, "w")
        chart_file.write("\n".join(";".join(song_data) for song_data in songs))
        chart_file.close()

if __name__ == "__main__":
    main()

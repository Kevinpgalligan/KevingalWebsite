import matplotlib.pyplot as plt

BYTES_INTERQUARTILE = [117444183625, 1492513128682, 283180779936, 2814808632228, 363177891001, 70743962271]
BYTES_10_TO_90 = [188774196140, 2337263808937, 462249675219, 5528893867126, 595097490698, 257342218994]

def main():
    # plot_bar(BYTES_INTERQUARTILE)
    plot_bar(BYTES_10_TO_90)

def plot_bar(bs):
    totalsum = sum(bs)
    props = [round(100*s/totalsum, 2) for s in bs]
    colours = ["red", "#f3e31d", "purple", "#77dd77", "black", "#aec6cf"]
    names = ["html", "js", "css", "img", "font", "video"]
    attrs = list(zip(props, names, colours))
    attrs.sort(key=lambda p: p[0])
    fig, ax = plt.subplots()
    props, names, colours = zip(*attrs)
    plt.bar(
        names,
        props,
        color=colours,
        zorder=3)
    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)
    for i, v in enumerate(props):
        ax.text(
            i,
            v+1,
            str(v) + "%",
            color="black",
            ha="center")
    plt.grid(zorder=0)
    plt.ylabel("%")
    plt.show()

if __name__ == "__main__":
    main()

import matplotlib.pyplot as plt
from collections import defaultdict

def count_dict(ticks):
    count = defaultdict(int)
    for n in ticks: count[n] += 1
    return count

def plot_tick_frequency(ticks, ax, plot_format, legend_name):
    count_map = count_dict(ticks)
    unique_ticks = []
    counts = []
    for unique_tick in count_map:
        unique_ticks.append(unique_tick)
        counts.append(count_map[unique_tick])
    ax.plot(unique_ticks, counts, plot_format, label=legend_name)
    
def main():
    results_file = open("results.txt", "r")
    entrances_at_ends_ticks = [int(s.strip()) for s in results_file.readline().split(",")]
    centered_entrances_ticks = [int(s.strip()) for s in results_file.readline().split(",")]
    print(sum(entrances_at_ends_ticks) / len(entrances_at_ends_ticks))
    print(sum(centered_entrances_ticks) / len(centered_entrances_ticks))
    f, ax = plt.subplots(1)
    ax.set_xlim(xmin=0, xmax=max(max(entrances_at_ends_ticks), max(centered_entrances_ticks)))
    plot_tick_frequency(entrances_at_ends_ticks, ax, "r^", "entrances-at-ends")
    plot_tick_frequency(centered_entrances_ticks, ax, "bs", "centered-entrances")
    ax.legend()
    plt.xlabel("Time steps")
    plt.ylabel("Frequency")
    plt.show()

if __name__ == "__main__":
    main()

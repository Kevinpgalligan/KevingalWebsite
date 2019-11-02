from random import randint, shuffle
import matplotlib.pyplot as plt
import collections

def generate_rgb(N):
    remaining = 3 * N
    R = randint(max(remaining - 2 * 255, 0), min(remaining, 255))
    remaining -= R
    G = randint(max(remaining - 1 * 255, 0), min(remaining, 255))
    remaining -= G
    B = remaining
    result = [R, G, B]
    shuffle(result)
    return tuple(result)

def main():
    N = 1
    rgbs = [generate_rgb(N) for _ in range(10000)]
    print("PASS" if all(sum(rgb) // 3 == N for rgb in rgbs) else "FAIL")
    ax = plt.figure().gca()
    for colour, values in zip(["r", "g", "b"], zip(*rgbs)):
        value_counts = collections.defaultdict(int)
        for value in values:
            value_counts[value] += 1
        xs, ys = zip(*sorted(value_counts.items(), key=lambda vc: vc[0]))
        ax.plot(xs, ys, color=colour)
    plt.title("Value frequency for R, G, B")
    plt.ylabel('Frequency')
    plt.xlabel('Value')
    plt.xticks(list(range(4)))
    ax.spines['right'].set_visible(False)
    ax.spines['top'].set_visible(False)
    plt.show()

if __name__ == "__main__":
    main()

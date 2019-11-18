import random as rd
import matplotlib.pyplot as plt
import collections
from scipy.special import comb

def main():
    A = 245
    print(f"Possible RGB values: {num_rgbs_that_sum_to(3*A)}")
    rgbs = generate_rgbs(A, n=100000)
    print("PASS" if all(sum(rgb) == 3*A for rgb in rgbs) else "FAIL")
    plot_rgb_frequencies(rgbs)

def generate_rgbs(A, n=1):
    S = 3*A
    num_rgbs = num_rgbs_that_sum_to(S)
    # R can't be smaller than 3A - 2*255. Otherwise, even with G = B = 255, we will have:
    #   R + G + B = (3A - 2*255 - blah) + 255 + 255 = 3A - blah < 3A.
    # Similarly, R can't be bigger than 3A.
    # We also need to ensure that R is in the range [0, 255].
    R_possible_values = list(range(max(S - 2 * 255, 0), min(S, 255) + 1))
    R_probabilities = [num_gbs_that_sum_to(S - r) / num_rgbs for r in R_possible_values]
    return [generate_rgb(S, R_possible_values, R_probabilities)
            for _ in range(n)]

def num_rgbs_that_sum_to(S):
    """Outputs number of RGB tuples that sum to 'S'."""
    K = 3 # R, G, B
    return (comb(S + K - 1, K - 1)
        # Remove cases where R, G or B are >255, since
        # these are not allowed.
        - 3 * comb(S + K - 1 - 256, K - 1)
        # Re-add the case where >1 of R, G and B are >255, since
        # they were subtracted twice in the previous bit.
        + 3 * comb(S + K - 1 - 2 * 256, K - 1))

def num_gbs_that_sum_to(S):
    return min(S, 255) - max(S - 255, 0) + 1

def generate_rgb(S, R_possible_values, R_probabilities):
    """Generates R,G,B that sum to S given pre-computed probabilities
    for possible R values."""
    R, = rd.choices(population=R_possible_values, weights=R_probabilities)
    S -= R
    G = rd.randint(max(S - 255, 0), min(S, 255))
    S -= G
    B = S
    return (R, G, B)

def plot_rgb_frequencies(rgbs):
    ax = plt.figure().gca()
    rgb_counts = collections.defaultdict(int)
    for rgb in rgbs:
        rgb_counts[rgb] += 1
    print(f"{len(rgb_counts)} unique RGB values generated.")
    print(f"Most frequent RGB value: {max(rgb_counts.items(), key=lambda vc: vc[1])}")
    plt.scatter(
        list(range(len(rgb_counts))),
        list(rgb_counts.values()))
    plt.title("RGB value distribution")
    plt.ylabel('Count')
    plt.xlabel('RGB value')
    plt.ylim(bottom=0)
    ax.spines['right'].set_visible(False)
    ax.spines['top'].set_visible(False)
    plt.show()

if __name__ == "__main__":
    main()

import random as rd
import matplotlib.pyplot as plt
import collections
from scipy.special import comb
import numpy as np

def main():
    A = 1
    generator = RGBGenerator(A)
    rgbs = [generator.generate() for _ in range(10000)]
    print("PASS" if all(sum(rgb) // 3 == A for rgb in rgbs) else "FAIL")
    plot_value_frequencies(rgbs)
    # plot_rgb_frequencies(rgbs)

class RGBGenerator:
    def __init__(self, A):
        self.A = A
        remaining = 3*A
        rgbs = num_rgbs_that_sum_to(remaining) 
        # R can't be smaller than 3A - 2*255. Otherwise, even with G = B = 255, we will have:
        #   R + G + B = (3A - 2*255 - k) + 255 + 255 = 3A - k < 3A. R+G+B=3A.
        # Likewise, R can't be bigger than 3A.
        # We also need to ensure that R is in the range [0, 255].
        self.R_possible_values = list(range(max(remaining - 2 * 255, 0), min(remaining, 255) + 1))
        # Probability distribution of all of the possible R values. To calculate the probability
        # of a given value of R, we consider the number of pairs of G & B that sum up to 3A - R,
        # which varies by the value of R.
        self.R_probabilities = [num_gbs_that_sum_to(remaining - r) / rgbs for r in self.R_possible_values]
        
    def generate(self):
        # The goal is to generate RGB so that R+G+B=3A, so that the average (R+G+B)/3 = A.
        remaining = 3 * self.A
        R, = rd.choices(population=self.R_possible_values, weights=self.R_probabilities)
        remaining -= R
        # As we did above for R, need to set bounds on possible values for G so that it's in the range [0, 255].
        G = rd.randint(max(remaining - 255, 0), min(remaining, 255))
        remaining -= G
        B = remaining
        return (R, G, B)

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

def plot_value_frequencies(rgbs):
    ax = plt.figure().gca()
    for colour, values in zip(["r", "g", "b"], zip(*rgbs)):
        value_counts = collections.defaultdict(int)
        for value in values:
            value_counts[value] += 1
        xs, ys = zip(*sorted(value_counts.items(), key=lambda vc: vc[0]))
        ax.plot(xs, ys, color=colour)
    plt.title("Value distribution for R, G, B")
    plt.ylabel('Count')
    plt.xlabel('Value')
    plt.ylim(bottom=0)
    plt.xticks([0, 1, 2, 3]) # assumes that we only plot this for A=1.
    ax.spines['right'].set_visible(False)
    ax.spines['top'].set_visible(False)
    plt.show()

def plot_rgb_frequencies(rgbs):
    ax = plt.figure().gca()
    rgb_counts = collections.defaultdict(int)
    for rgb in rgbs:
        rgb_counts[rgb] += 1
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

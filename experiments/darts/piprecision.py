from scipy.stats import norm
import matplotlib.pyplot as plt
import math

TARGET_SUCCESS_RATE = 0.95

def plot(eps):
    ns = []
    ys = []
    n = 100
    p = math.pi/4
    while True:
        # Normal approximation of the binomial distribution
        # of darts landing on the dartboard.
        distr = norm(n * p, math.sqrt(n * p * (1 - p)))
        y = 1 - 2*distr.cdf(n * (math.pi - eps) / 4)
        ns.append(n)
        ys.append(y)
        if y >= TARGET_SUCCESS_RATE:
            break
        n *= 1.2
    plt.plot(ns, ys, label="ε={}".format(eps), marker="o")

def main():
    for eps in [0.1, 0.01, 0.001, 0.0001]:
        plot(eps)
    plt.xscale("log")
    plt.legend()
    plt.ylim(bottom=0., top=1.)
    plt.axhline(y=TARGET_SUCCESS_RATE, color="red", linestyle="--")
    plt.grid(linestyle="--")
    plt.xlabel("n")
    plt.ylabel("P(E < ε)")
    plt.show()

if __name__ == "__main__":
    main()

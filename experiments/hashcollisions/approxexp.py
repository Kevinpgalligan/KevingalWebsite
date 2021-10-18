import matplotlib.pyplot as plt
import math

def main():
    start_pow, end_pow = 1, -10
    steps = 100
    xs = [10**(i*(end_pow-start_pow)/steps) for i in range(steps+1)]

    ys = []
    for x in xs:
        y1 = math.exp(-x)
        y2 = 1-x
        ys.append(abs(y1-y2)/y1)
    plt.plot(xs, ys, marker="o", markerfacecolor="None", linestyle="--")

    plt.grid(linestyle="--")
    plt.xlabel("x")
    plt.ylabel("relative error")
    plt.xscale("log")
    plt.yscale("log")
    plt.show()

if __name__ == "__main__":
    main()

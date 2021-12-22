import matplotlib.pyplot as plt
import math
import argparse

def product(xs):
    result = 1
    for x in xs:
        result *= x
    return result

def bday(k, N):
    return 1 - product((N-i)/N for i in range(k))

def exp_bday(k, N):
    return 1 - math.exp(-k*(k-1)/(2*N))

def simple_bday(k, N):
    return (k*(k-1))/(2*N)

def simple_square_bday(k, N):
    return (k**2)/(2*N)

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("N", type=int)
    parser.add_argument("startk", type=int)
    args = parser.parse_args()

    methods = [
        #("full", bday),
        ("exp", exp_bday),
        ("simple", simple_bday),
        ("simple-square", simple_square_bday)
    ]
    
    ks = [math.floor(k) for k in exp_range(args.startk, args.N, 10)]
    full_bday_probs = [bday(k, args.N) for k in ks]
    for name, f in methods:
        rel_errs = [abs(fbp-f(k, args.N))/fbp for k, fbp in zip(ks, full_bday_probs)]
        plt.plot(ks, rel_errs, marker="o",
                 markerfacecolor="white", linestyle="--",
                 label=name)

    plt.grid(linestyle="--")
    plt.xlabel("k")
    plt.ylabel("relative error")
    plt.xscale("log")
    plt.yscale("log")
    plt.legend()
    plt.show()

def exp_range(start, end, steps):
    for e in float_range(math.log10(start), math.log10(end), steps):
        yield 10**e

def float_range(start, end, steps):
    i = 0
    while i <= steps:
        if steps == 0:
            x = start
        else:
            x = start + i*(end-start)/steps
        yield x
        i += 1

if __name__ == "__main__":
    main()

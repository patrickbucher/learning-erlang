#!/usr/bin/env python3

import matplotlib.pyplot as plt
import numpy as np

if __name__ == '__main__':
    data = np.genfromtxt('statistics.csv', delimiter=',', skip_header=1)
    data = np.array(data)
    fig = plt.figure()
    plt.xlabel('processes')
    plt.ylabel('seconds')
    plt.title('spawning Erlang processes')
    plt.plot(data[:, 0], data[:, 1], label='CPU Time')
    plt.plot(data[:, 0], data[:, 2], label='Wall Time')
    plt.legend()
    fig.savefig('statistics.png')

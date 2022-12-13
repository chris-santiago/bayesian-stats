import numpy as np


def exact_method(alpha, n):
	i = np.arange(1, n+1, 1)
	return np.sum(alpha / (alpha + i - 1))



def approx_method(alpha, n):
	return alpha * np.log((n + alpha - 1) / alpha)


a = .35499
# a = 2
n = 100
print(approx_method(a, n))

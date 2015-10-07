#!/usr/bin/env python3

from math import factorial

def combination(n, m):
    return factorial(n) / (factorial(m) * factorial(n - m))

def permutation(n, m):
    return factorial(n) / factorial(n - m)

def powerset(n):
    return 2 ** n

def num_formulas(variables):
    return powerset(3 ** variables - 1)

for i in range(1, 6):
    print(str(i) + "->" + str(num_formulas(i)))

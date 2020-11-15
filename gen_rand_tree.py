# cost parent
# number of lines n 

import sys
import random

n = int(sys.argv[1])

#random.seed(42)

def rand_cost(limit=1000):
    return random.randint(1, limit)

def rand_node():
    return random.randint(1, n)


# Tree repr: list with index tree node and value list of children
i, j = 1, 1
parent = dict()
parent[1] = 0 # root
while i < n:
    child_num = random.randint(1,6)
    for k in range(j+1, j+child_num+1):
        parent[k] = i
    i += 1
    j += child_num

n = len(parent)
print(n)

# function that maps every node 1..n to another node (rename nodes)

vals = list(range(1, n+1))
random.shuffle(vals)
vals.insert(0, 0)
inv_vals = vals.copy()

for i, val in enumerate(vals):
    inv_vals[val] = i
for i in range(1, n+1):
    cost = rand_cost()
    print('{} {}'.format(cost, vals[parent[inv_vals[i]]]))

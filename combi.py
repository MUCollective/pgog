from itertools import chain, combinations

def powerset(iterable):
    """
    powerset([1,2,3]) --> () (1,) (2,) (3,) (1,2) (1,3) (2,3) (1,2,3)
    """
    xs = list(iterable)
    # note we return an iterator rather than a list
    return chain.from_iterable(combinations(xs,n) for n in range(len(xs)+1))


aesthetics_var = ["width = P(A)", "height = P(B)", "x = A", "y = B"]
geoms = ["geom_icon", "geom_block"]

for e in list(powerset(aesthetics_var))[1:]:
    for i in e:
    	print(i, end="; ")
    print()


# second thoughts

aes = ["w", "h", "c"]
coord_aes = ["x", "y"]
prob_variables = [ "P(A)", "P(B)", "P(A|B)", "P(B|A)"]
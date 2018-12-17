from itertools import chain, combinations, product

def powerset(iterable):
	"""
	powerset([1,2,3]) --> () (1,) (2,) (3,) (1,2) (1,3) (2,3) (1,2,3)
	"""
	xs = list(iterable)
	# note we return an iterator rather than a list
	return chain.from_iterable(combinations(xs,n) for n in range(len(xs)+1))


aesthetics_var = ["width = P(A)", "height = P(B)", "x = A", "y = B"]
geoms = ["geom_icon", "geom_block"]


def first_Try():
	for e in list(powerset(aesthetics_var))[1:]:
		for i in e:
			print(i, end="; ")
		print()


# second thoughts

aes = ["w", "h", "c"]
coord_aes = ["x", "y"]
variables = ["A", "B"]
prob_variables = [ "P(A)", "P(B)", "P(A|B)", "P(B|A)"]


def insert_equal(x):
	mapping = x[0] + '=' + x[1]
	return mapping

# coord_mappings = list(powerset(map(insert_equal, [("x", "A"), ("y", "B")])))
coord_mappings = [[], \
				["x=A"], \
				["x=A", "y=B"]]

print(coord_mappings)
prob_mappings = list(powerset(map(insert_equal, list(product(aes, prob_variables)))))
print(len(prob_mappings))




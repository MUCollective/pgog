rv(x).
rv(y).


marginal([]).

marginal([H|T]):-
	rv(H),
	marginal(T).

pmf(Marginal, Conditional) :-
	marginal(Marginal),
	marginal(Conditional).

merge(pmf(M1,C1), pmf(M2, C2)) :-
	
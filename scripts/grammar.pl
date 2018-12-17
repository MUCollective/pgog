probAes(w).
probAes(h).
probAes(c).

probVar(p_A).
probVar(p_A_given_B).


probMapping(Aes, Var) :-
	probAes(Aes),
	probVar(Var).




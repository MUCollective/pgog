%% aesthetics
probAes(width).
probAes(height).

coordAes(x).
coordAes(y).
coordAes(color).

%% geom arguments
geomArg(position).

%% Variables
probVar('P(A)').
probVar('P(A|B)').
coordVar('A').
coordVar('A|B').


%% Aesthetics mappings
probMapping(Aes, Var) :-
	probAes(Aes),
	probVar(Var).

coordMapping(Aes, Var) :-
	coordAes(Aes),
	coordVar(Var).

%% mapping(L):- 
%% 	L = [H]
%% 	[probMapping(Aes, Var)].

	



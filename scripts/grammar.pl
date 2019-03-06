%% aesthetics
probAes(width).
probAes(height).

coordAes(x).
coordAes(y).
%% coordAes(color).

%% geom arguments
geomArg(position).

%% Variables
%% probVar('P(A)').
%% probVar('P(B)').
%% probVar('P(A|B)').
%% probVar('P(B|A)').

coordVar(a).
coordVar(b).

probVar(X) :- coordVar(X).
probVar(X, Y) :- coordVar(X), coordVar(Y), \+ ((X = Y)). 
%% probVar(a, b).
%% probVar(b, a).

%% coordVar('A|B').


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

%% mapping(A, B)	



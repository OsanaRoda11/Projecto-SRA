:- initialization(main).

main :-
    read(Input),
    eval(Input, R),
    write(R), nl,
    halt.

% Definição das operações lógicas
eval(true, true).
eval(false, false).
eval(var(X), V) :- valor(X, V).  % Busca o valor da variável no ambiente
eval(not(X), V) :- eval(X, VX), negacao(VX, V).
eval(and(X, Y), V) :- eval(X, VX), eval(Y, VY), conjuncao(VX, VY, V).
eval(or(X, Y), V) :- eval(X, VX), eval(Y, VY), disjuncao(VX, VY, V).
eval(implies(X, Y), V) :- eval(X, VX), eval(Y, VY), implicacao(VX, VY, V).
eval(iff(X, Y), V) :- eval(X, VX), eval(Y, VY), equivalencia(VX, VY, V).

% Definição das operações booleanas
negacao(true, false).
negacao(false, true).

conjuncao(true, true, true).
conjuncao(_, _, false).

disjuncao(false, false, false).
disjuncao(_, _, true).

implicacao(true, false, false).
implicacao(_, _, true).

equivalencia(X, Y, true) :- X = Y.
equivalencia(_, _, false).


% Gera todas as combinações de valores para as variáveis
todas_as_asignacoes([], []).
todas_as_asignacoes([Var | Resto], [[Var, true] | Asig]) :-
    todas_as_asignacoes(Resto, Asig).
todas_as_asignacoes([Var | Resto], [[Var, false] | Asig]) :-
    todas_as_asignacoes(Resto, Asig).

% Verifica se uma proposição é uma tautologia
tautologia(Expr) :-
    findall(Var, variavel(Expr, Var), VariaveisSemDuplicatas),
    todas_as_asignacoes(VariaveisSemDuplicatas, Asignacoes),
    forall(member(Asignacao, Asignacoes), (maplist(assertz, Asignacao), eval(Expr, true), retractall(valor(_, _)))).


% Verifica se um conjunto de premissas implica a conclusão
inferencia(Premissas, Conclusao) :-
    maplist(assertz, Premissas),
    eval(not(Conclusao), false),
    retractall(valor(_, _)).

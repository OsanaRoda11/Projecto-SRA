% Operações lógicas básicas
avaliar_e(true, true, true).
avaliar_e(true, false, false).
avaliar_e(false, true, false).
avaliar_e(false, false, false).

avaliar_ou(true, _, true).
avaliar_ou(_, true, true).
avaliar_ou(false, false, false).

avaliar_nao(true, false).
avaliar_nao(false, true).

avaliar_implica(true, false, false).
avaliar_implica(_, true, true).
avaliar_implica(false, _, true).

avaliar_iff(X, X, true).
avaliar_iff(_, _, false).

% Predicado principal para avaliar expressões
avaliar(and(X, Y), R) :- avaliar_e(X, Y, R).
avaliar(or(X, Y), R) :- avaliar_ou(X, Y, R).
avaliar(not(X), R) :- avaliar_nao(X, R).
avaliar(implies(X, Y), R) :- avaliar_implica(X, Y, R).
avaliar(iff(X, Y), R) :- avaliar_iff(X, Y, R).
avaliar(X, X).
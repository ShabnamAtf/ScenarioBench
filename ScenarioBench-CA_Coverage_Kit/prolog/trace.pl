:- module(trace, [trace_reset/0, fire/1, trace_get/1]).
:- dynamic rules_fired/1.

trace_reset :-
    retractall(rules_fired(_)),
    asserta(rules_fired([])).

to_string(X, S) :-
    ( string(X) -> S = X
    ; atom(X)   -> atom_string(X, S)
    ; term_string(X, S)
    ).

fire(Id) :-
    to_string(Id, S),
    (   retract(rules_fired(L0)) -> true ; L0 = [] ),
    append(L0, [S], L1),
    asserta(rules_fired(L1)).

trace_get(L) :-
    ( rules_fired(L) -> true ; L = [] ).

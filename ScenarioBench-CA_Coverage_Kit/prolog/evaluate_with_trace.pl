:- module(evaluate_with_trace, [evaluate/3]).
:- use_module(prolog/trace).
:- use_module('rules/decision_bridge').

evaluate(SId, Decision, Trace) :-
    decision_bridge:final_decide(SId, Decision, Trace).

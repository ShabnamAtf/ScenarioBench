:- module(decision_bridge, [final_decide/3]).
:- use_module(rules/finding_violations).
:- use_module(rules/decision_classifier).
:- use_module(prolog/trace).

final_decide(SId, Decision, Trace) :-
    trace_reset,
    violations_for(SId, Vs),
    decision_classifier:classify_decision(Vs, Decision, Fixes),
    ( Fixes \= [] -> forall(member(F, Fixes), fire(F)) ; true ),
    trace_get(Trace).

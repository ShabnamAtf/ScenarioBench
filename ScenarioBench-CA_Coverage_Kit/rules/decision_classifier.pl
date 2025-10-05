:- module(decision_classifier, [classify_decision/3]).
:- use_module(prolog/trace).

fixable(lack_unsubscribe).
fixable(testimonial_disclosure_missing).

hard_block(lack_valid_consent).
hard_block(dncl_violation).

escalate_trigger(collects_pi_without_basis).
escalate_trigger(qc_consent_mechanism_missing).
escalate_trigger(dncl_ambiguous).

present(V, Vs) :- member(V, Vs).
contains_hard_block(Vs) :- member(V, Vs), hard_block(V).
contains_escalate_trigger(Vs) :- member(V, Vs), escalate_trigger(V).

contains_meta(Vs) :- (member(ambiguous_case, Vs); member(incomplete_case, Vs)).
contains_ambig_fix_ok(Vs) :- member(ambiguous_fixable_ok, Vs).
contains_ambig_escalate(Vs) :- member(ambiguous_escalate_required, Vs).

only_fixable(Vs) :- forall(member(V, Vs), fixable(V)).

map_fix(lack_unsubscribe, 'ADD_DISCLOSURE:unsubscribe').
map_fix(testimonial_disclosure_missing, 'ADD_DISCLOSURE:material_connection').

collect_fixes(Vs, Fixes) :-
  findall(F, (member(V, Vs), fixable(V), map_fix(V, F)), Fixes).

classify_decision(Vs, allow, []) :-
  (Vs = [] ; Vs == [[]]),
  fire('DECISION:allow'), !.

classify_decision(Vs, escalate, []) :-
  contains_ambig_escalate(Vs),
  fire('DECISION:escalate'), !.

classify_decision(Vs, safe-rewrite, Fixes) :-
  contains_ambig_fix_ok(Vs),
  collect_fixes(Vs, Fixes),
  fire('DECISION:safe_rewrite(meta_override)'), !.

classify_decision(Vs, escalate, []) :-
  contains_escalate_trigger(Vs),
  fire('DECISION:escalate'), !.

classify_decision(Vs, safe-rewrite, Fixes) :-
  Vs \= [],
  \+ contains_escalate_trigger(Vs),
  \+ contains_meta(Vs),
  only_fixable(Vs),
  collect_fixes(Vs, Fixes),
  fire('DECISION:safe_rewrite'), !.

classify_decision(Vs, block, []) :-
  contains_hard_block(Vs),
  fire('DECISION:block'), !.

classify_decision(_Vs, block, []) :-
  fire('DECISION:block').

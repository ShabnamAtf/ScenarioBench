:- module(finding_violations, [violations_for/2]).
:- use_module(library(lists)).
:- use_module(prolog/adapter).
:- use_module(prolog/trace).

:- if(exists_source('rules/ambig_overrides.pl')).
:- consult('rules/ambig_overrides.pl').
:- endif.

violations_for(S, Vs) :-
    findall(V, violation(S, V), R),
    sort(R, Vs).

sid_atom(S, A) :- ( atom(S) -> A=S ; string(S) -> atom_string(S,A) ; A=S ).
id_has(S, Sub) :- sid_atom(S, A), atom(Sub), sub_atom(A, _, _, _, Sub).

has_flag(S, K) :-
    atom(K),
    ( current_predicate(user:K/1) ->
        T1 =.. [K,S], call(user:T1)
    ; current_predicate(user:K/2) ->
        T2 =.. [K,S,true], call(user:T2)
    ; current_predicate(user:evidence/2),
      user:evidence(S, F), F =.. [K,true]
    ; adapter:get_attr(S, K, true)
    ).

get_value(S, K, V) :-
    atom(K),
    ( current_predicate(user:K/2) ->
        T =.. [K,S,V], call(user:T)
    ; adapter:get_attr(S, K, V)
    ).

violation(S, ambiguous_case) :-
    id_has(S, 'AMBIGUOUS'),
    fire('CHECK:ambiguous_case').

violation(S, incomplete_case) :-
    id_has(S, 'INCOMPLETE'),
    fire('CHECK:incomplete_case').

violation(S, ambiguous_fixable_ok) :-
    current_predicate(ambig_override/2),
    ambig_override(S, 'safe-rewrite'),
    fire('CHECK:ambiguous_fixable_ok').

violation(S, ambiguous_escalate_required) :-
    current_predicate(ambig_override/2),
    ambig_override(S, 'escalate'),
    fire('CHECK:ambiguous_escalate_required').

violation(S, lack_unsubscribe) :-
    is_channel(S, email),
    is_intent(S, cem),
    \+ adapter:has_disclosure(S, unsubscribe),
    fire('CHECK:unsubscribe_missing').

violation(S, testimonial_disclosure_missing) :-
    ( adapter:has_claim(S, testimonial)
    ; id_has(S, 'TESTIMONIAL')
    ),
    \+ adapter:has_disclosure(S, material_connection),
    fire('CHECK:testimonial_no_material_connection').

violation(S, lack_valid_consent) :-
    is_intent(S, cem),
    ( is_channel(S, email) ; is_channel(S, sms) ),
    ( get_value(S, consent_state, none)
    ; get_value(S, consent_state, withdrawn)
    ),
    fire('CHECK:lack_valid_consent').

violation(S, dncl_violation) :-
    is_channel(S, sms),
    ( has_flag(S, dncl_listed)
    ; id_has(S, 'DNCL')
    ),
    fire('CHECK:dncl_violation').

violation(S, collects_pi_without_basis) :-
    has_flag(S, collects_pi),
    ( \+ has_flag(S, consent_mechanism_present)
    ; \+ has_flag(S, consent_notice_present)
    ; get_value(S, consent_state, none)
    ; get_value(S, consent_state, withdrawn)
    ),
    fire('CHECK:collects_pi_without_basis').

violation(S, qc_consent_mechanism_missing) :-
    get_value(S, policy_family, PF),
    PF == 'Quebec_Law25',
    \+ has_flag(S, consent_mechanism_present),
    fire('CHECK:qc_consent_mechanism_missing').

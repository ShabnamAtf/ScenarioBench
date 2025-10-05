% reference_rules.pl — minimal reference rules for ScenarioBench-CA

:- dynamic scenario/1.
:- dynamic channel/2, message_intent/2, consent_state/2, relationship/2, audience/2, region/2, pi_types/2, substantiation/2.
:- dynamic has_disclosure/2, claim_type/2.
:- dynamic has_identity/1, has_unsubscribe/1, transaction_reference_included/1, dncl_listed/1.
:- dynamic consent_mechanism_present/1, collects_sensitive_pi/1, collects_pi/1.
:- dynamic is_cem/1, is_transactional/1.

% ---------- helpers ----------
valid_consent(ID) :-
    consent_state(ID, express);
    (consent_state(ID, implied), \+ consent_state(ID, withdrawn)).

unsubscribe_present(ID) :-
    has_unsubscribe(ID).

missing_ad_label(ID) :-
    \+ has_disclosure(ID, ad_label).

% ---------- rules producing decision + trace (list of strings) ----------
casl_section6_block(ID, Trace) :-
    is_cem(ID),
    \+ valid_consent(ID),
    \+ transactional_exception(ID),
    Trace = ['is_cem(content)=true','has_valid_consent(context)=false','FORBID fires: CASL:section6'].

transactional_exception(ID) :-
    is_transactional(ID),
    transaction_reference_included(ID),
    has_identity(ID),
    has_unsubscribe(ID).

casl_unsubscribe_fix(ID, Trace) :-
    is_cem(ID),
    \+ unsubscribe_present(ID),
    Trace = ['is_cem(content)=true','unsubscribe_present(content)=false','REQUIRE unmet: CASL:unsubscribe'].

crtc_dncl_block(ID, Trace) :-
    channel(ID, sms),
    dncl_listed(ID),
    Trace = ['channel=sms','dncl_listed(context)=true','FORBID fires: CRTC:DNCL'].

pipeda_4_3_escalate(ID, Trace) :-
    (collects_sensitive_pi(ID); pi_types(ID, sensitive)),
    \+ consent_mechanism_present(ID),
    Trace = ['collects_sensitive_pi(content)=true','consent_mechanism_present(context)=false','REQUIRE unmet → escalate'].

law25_consent_escalate(ID, Trace) :-
    region(ID, qc),
    collects_pi(ID),
    \+ consent_mechanism_present(ID),
    Trace = ['region=QC','collects_pi(content)=true','consent_mechanism_present(context)=false','REQUIRE unmet: QL25:Consent'].

adstand_testimonial_fix(ID, Trace) :-
    claim_type(ID, testimonial),
    (substantiation(ID, none); substantiation(ID, weak); missing_ad_label(ID)),
    Trace = ['contains_claim(testimonial)=true','substantiation_level∈{none|weak} or missing_ad_label=true','REQUIRE unmet: AS:Testimonials'].

% precedence: PERMIT defeats FORBID
casl_transactional_permit(ID, Trace) :-
    transactional_exception(ID),
    Trace = ['is_transactional(content)=true','identity_and_unsubscribe_present(content)=true','PERMIT defeats FORBID via precedence'].

% ---------- top-level decision ----------
decide(ID, block, Trace) :- crtc_dncl_block(ID, Trace), !.
decide(ID, allow, Trace) :- casl_transactional_permit(ID, Trace), !.
decide(ID, block, Trace) :- casl_section6_block(ID, Trace), !.
decide(ID, safe_rewrite, Trace) :- casl_unsubscribe_fix(ID, Trace), !.
decide(ID, escalate, Trace) :- pipeda_4_3_escalate(ID, Trace), !.
decide(ID, escalate, Trace) :- law25_consent_escalate(ID, Trace), !.
decide(ID, safe_rewrite, Trace) :- adstand_testimonial_fix(ID, Trace), !.
decide(_, allow, ['default_allow']).

% ---------- evaluation printer ----------
print_result(ID, Decision, Trace) :-
    write('RESULT|'), write(ID), write('|'), write(Decision), write('|'), write(Trace), nl.

evaluate_all :-
    forall(scenario(ID),
        (decide(ID, D, T), print_result(ID, D, T))
    ).

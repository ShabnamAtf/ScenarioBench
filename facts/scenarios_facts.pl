:- module(scenarios_facts, [
    scenario/4,
    gold_decision/2,
    gold_trace/2,
    gold_sql/2,
    gold_topk/2
]).

:- discontiguous scenario/4.
:- discontiguous gold_decision/2.
:- discontiguous gold_trace/2.
:- discontiguous gold_sql/2.
:- discontiguous gold_topk/2.

:- dynamic      scenario/4.
:- dynamic      gold_decision/2.
:- dynamic      gold_trace/2.
:- dynamic      gold_sql/2.
:- dynamic      gold_topk/2.

scenario('CASL-EMAIL-UNSUB-003', '{"jurisdiction": "CA", "channel": "email", "language": "en", "consent_state": "none"}', '"Subject: Upgrade today!\nBody: Exclusive offer for you... limited time.\nFooter: (no unsubscribe link)\n"', 'scenarios/CASL-EMAIL-UNSUB-003.yml').
gold_decision('CASL-EMAIL-UNSUB-003', escalate).

% stub to satisfy export when no gold_sql/2 facts exist
gold_sql(_, _) :- fail.

% stub to satisfy export when no gold_topk/2 facts exist
gold_topk(_, _) :- fail.


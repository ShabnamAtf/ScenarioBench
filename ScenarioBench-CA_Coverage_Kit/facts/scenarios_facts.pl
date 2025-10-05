:- module(scenarios_facts, [
    scenario/4,        % scenario_id, ContextJSON, ContentJSON, SourceFile
    gold_decision/2,   % scenario_id, Decision(atom: allow|block|safe_rewrite|escalate)
    gold_trace/2,      % scenario_id, ClauseId (e.g., 'CLAUSE_XYZ')
    gold_sql/2,        % scenario_id, SQLText
    gold_topk/2        % scenario_id, DocIdOrClauseId
]).

:- discontiguous gold_trace/2.
:- dynamic      gold_trace/2.

% NOTE:
% - Decision را با اَتم‌های allow | block | safe_rewrite | escalate بنویس.
% - clause_id دقیقاً با شناسه‌های Policy-DB یکی باشد (یک‌به‌یک).
% - ContextJSON و ContentJSON را به صورت رشتهٔ JSON ذخیره می‌کنیم.

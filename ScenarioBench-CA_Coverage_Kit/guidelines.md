
### 4) `ScenarioBench-CA_Coverage_Kit/guidelines.md`
```md
# ScenarioBench-CA Labeling Guidelines (Stage 4 — Coverage & Taxonomy)

Each scenario MUST include:
```yaml
gold_standard:
  decision: <allow|block|safe-rewrite|escalate>
  rationale: "<plain why>"
  policy_refs: ["<clause ids>"]
  expected_trace:
    - "<predicate>=<value>"
    - "RULE fires: <clause id>"

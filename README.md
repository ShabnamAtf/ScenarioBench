# ScenarioBench (Private)

Trace-grounded compliance benchmark for **Text-to-SQL** and **RAG**. Systems must justify decisions with **clause IDs** they actually retrieved from the same policy canon; SQL is scored by **result-set equivalence** over `clause_id`. Includes difficulty/latency-aware indices (**SDI / SDI-R**).

> Status: private WIP. Seed scenarios, schemas, and evaluator scripts are included; more docs incoming.

## Quickstart
```bash
# create & activate venv (Windows)
python -m venv .venv && .\.venv\Scripts\activate
# install deps (placeholder — add/adjust as needed)
pip install -r requirements.txt  # if present
# run your minimal demo script (adjust when ready)
python stage16_run_all.bat       # or: python scripts/quick_log.py
## Features
- Clause-level **grounding** (trace ⊆ retrieved@k), no external peek
- NLQ-to-SQL judged by **result-set equivalence** (on `clause_id`)
- Metrics: decision Acc/M-F1, trace completeness/correctness/order, Recall@k/MRR/nDCG, SQL acc, coverage, latency, hallucination
- **SDI / SDI-R**: difficulty & budget-aware aggregation
## Repo Structure (abridged)
prolog/                     # policy clauses (deterministic rules)
scenarios/                  # YAML/JSON scenarios
schema/                     # JSON Schemas for scenarios/logs
spec/                       # log schema & docs
stage16_run_all.bat         # starter scripts (Windows)
stage16_run_sut.bat

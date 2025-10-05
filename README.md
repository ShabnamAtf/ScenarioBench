# ScenarioBench (Private)

Trace-grounded compliance benchmark for **Text-to-SQL** and **RAG**: systems must justify decisions with **clause IDs** they retrieved; SQL is scored by **result-set equivalence** on `clause_id`. Includes difficulty/latency-aware indices (**SDI / SDI-R**).

**Status:** private WIP. Code and seed scenarios will be added shortly.

## Quickstart (placeholder)
```bash
Features

Clause-level grounding (trace ⊆ retrieved@k)

No-peek gold package (evaluator-only)

NLQ-to-SQL scored by result-set equality

Metrics: decision acc/F1, trace C/K/O, Recall@k/MRR/nDCG, SQL acc, coverage, latency, hallucination

SDI / SDI-R for difficulty & budget

Roadmap

Seed scenarios (N≈16), SQLite Policy_DB, BM25 + Hybrid

Logs (JSONL/CSV) + simple CLI for metrics

Repro scripts and manifest snapshots
# after cloning:
python -m venv .venv && .\.venv\Scripts\activate
pip install -r requirements.txt
python scripts/quick_log.py   # minimal end-to-end demo (coming soon)

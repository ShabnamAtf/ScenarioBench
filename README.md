# ScenarioBench (Private)

## License
MIT © 2025 Zahra Atf  
![License](https://img.shields.io/badge/license-MIT-informational)

**[📄 Preprint (PDF)](docs/scenariobench-preprint.pdf)**

Trace-grounded compliance benchmark for **Text-to-SQL** and **RAG**. Systems must justify decisions with **clause IDs** they actually retrieved from the same policy canon; SQL is scored by **result-set equivalence** on `clause_id`. Includes difficulty/latency-aware indices (**SDI / SDI-R**).

---

## Features
- Clause-level **grounding**: trace ⊆ retrieved@k (no external peek)
- NLQ-to-SQL judged by **result-set equality** (on `clause_id`)
- Metrics: decision Acc/M-F1, trace completeness/correctness/order, Recall@k/MRR/nDCG, SQL acc, coverage, latency, hallucination
- **SDI / SDI-R**: difficulty & time-budget–aware aggregation
- Reproducible logs (JSONL/CSV)

---

## Quickstart
```bash
# create & activate venv (Windows)
python -m venv .venv && .\.venv\Scripts\activate

# install deps (placeholder — adjust when requirements are added)
pip install -r requirements.txt  # if present

# run a minimal demo (adjust when your scripts are ready)
python stage16_run_all.bat
# or:
# python scripts/quick_log.py

```


## Repo Structure (abridged)
```text
prolog/                     # policy clauses (deterministic rules)
scenarios/                  # YAML/JSON scenarios
schema/                     # JSON Schemas for scenarios/logs
spec/                       # log schema & docs
stage16_run_all.bat         # starter scripts (Windows)
stage16_run_sut.bat


Metrics (at a glance)
```
Decision: Accuracy, Macro-F1

Trace: Completeness, Correctness, Order (Kendall-τ)

Retrieval: Recall@k, MRR, nDCG

SQL: result-set equivalence on clause_id

Grounding/Hallucination: unsupported clause citations

Latency: wall-clock per scenario

SDI / SDI-R: difficulty and budgeted cost–benefit

Data & Privacy

Synthetic scenarios and policy canon; no PII. Logs contain only scenario IDs, clause IDs, config hashes, and timing. Large binaries/databases are intentionally not versioned.

Roadmap

Seed expansion (≈50–150 scenarios, bilingual variants)

CLI for metrics & ablations; manifest snapshots (DB/index/rules)

Lightweight rerank + rule-only trace critic under latency budgets

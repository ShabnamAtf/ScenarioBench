# ScenarioBench (Private)

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

> Tested on Python 3.10–3.12 (Windows). Adjust paths as needed.

```bash
# create & activate venv (Windows)
python -m venv .venv && .\.venv\Scripts\activate

# install deps (placeholder — adjust when requirements are added)
pip install -r requirements.txt  # if present

# run a minimal demo (adjust when your scripts are ready)
# Example placeholders:
python stage16_run_all.bat
# or:
# python scripts/quick_log.py

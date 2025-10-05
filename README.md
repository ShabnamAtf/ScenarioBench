# ScenarioBench (Private)

Trace-grounded compliance benchmark for **Text-to-SQL** and **RAG**: systems must justify decisions with **clause IDs** they retrieved; SQL is scored by **result-set equivalence** on `clause_id`. Includes difficulty/latency-aware indices (**SDI / SDI-R**).

**Status:** private WIP. Code and seed scenarios will be added shortly.

## Quickstart (placeholder)
```bash
# after cloning:
python -m venv .venv && .\.venv\Scripts\activate
pip install -r requirements.txt
python scripts/quick_log.py   # minimal end-to-end demo (coming soon)

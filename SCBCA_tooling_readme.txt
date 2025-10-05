ScenarioBench-CA — Authoring & Tooling Recommendations (v0)
===========================================================

• Use Visual Studio Code + Git for day-to-day authoring:
  - Pros: first-class YAML/JSON editing, linting, search/replace, Git versioning, pre-commit hooks, easy repo structure.
  - Best for: policy_canon.yml/.csv, scenarios/*.yaml, Prolog rules, python scripts, docs/*.md.

• Use Google Colab for shareable experiments:
  - Pros: zero-setup Python, easy to run evaluation notebooks and share links.
  - Best for: prototyping metrics, running the evaluation harness on subsets of scenarios, quick visualizations.

• Hybrid workflow (recommended):
  1) Author and version files locally in VS Code (repo: /canon, /scenarios, /rules, /scripts, /docs).
  2) Push to GitHub (private or public).
  3) Open a Colab notebook that `git clone`s the repo (or mounts Drive) and runs evaluation scripts.
  4) Export baseline reports (CSV/Markdown) back into the repo (/reports).

• Suggested repo layout:
  /canon/policy_canon_sources.csv
  /canon/policy_canon_glossary.csv
  /canon/policy_canon.yml         # later
  /scenarios/*.yaml
  /rules/*.pl
  /scripts/canon2prolog.py
  /scripts/eval_scenarios.py
  /reports/baseline_report.md
  /docs/operational_notes_domain_distinctions.csv
  /README.md

• Quick rule of thumb:
  - Editing/curation -> VS Code
  - Experiments/notebooks/demos -> Colab

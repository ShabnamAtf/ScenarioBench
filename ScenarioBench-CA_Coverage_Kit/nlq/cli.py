# nlq/cli.py
import sys
import json
import re
from jsonschema import validate, ValidationError
from pathlib import Path
from .intents import NLQParsed, IntentName
from .normalize import normalize_slots
from .sql_contract import compile_sql
from .exec_sql import run_sql, run_count, connect

SCHEMA = json.loads(Path(__file__).with_name("schema.json").read_text())

HELP = """Usage:
  python -m nlq.cli sample
  python -m nlq.cli validate <jsonl-file>      # each line: {"intent": "...", "slots": {...}}
  python -m nlq.cli compile  <jsonl-file>      # outputs {"sql": "...", "params": {...}} per line
  python -m nlq.cli run <db-path> <jsonl-file> # executes SQL against SQLite DB; prints rows/count per line
"""

def _validate_line(obj: dict, lineno: int):
    slots = obj.get("slots", {})
    slots = normalize_slots(slots).model_dump()
    obj = {"intent": obj["intent"], "slots": slots}
    validate(instance=obj, schema=SCHEMA)
    return NLQParsed(**obj).model_dump()

def cmd_validate(path: Path):
    ok = 0
    for i, line in enumerate(path.read_text().splitlines(), start=1):
        if not line.strip():
            continue
        try:
            obj = json.loads(line)
            obj = _validate_line(obj, i)
            print(json.dumps(obj, ensure_ascii=False))
            ok += 1
        except ValidationError as e:
            raise SystemExit(f"Validation error on line {i}: {e.message}")
        except Exception as e:
            raise SystemExit(f"Error on line {i}: {e}")
    print(f"\nValidated {ok} objects.", file=sys.stderr)

def cmd_compile(path: Path):
    for i, line in enumerate(path.read_text().splitlines(), start=1):
        if not line.strip():
            continue
        try:
            obj = json.loads(line)
            parsed = _validate_line(obj, i)
            sql, params = compile_sql(IntentName(parsed["intent"]), NLQParsed(**parsed).slots)
            print(json.dumps({"sql": sql, "params": params}, ensure_ascii=False))
        except ValidationError as e:
            raise SystemExit(f"Validation error on line {i}: {e.message}")
        except Exception as e:
            raise SystemExit(f"Error on line {i}: {e}")

def _has_column(db_path: Path, table: str, col: str) -> bool:
    with connect(str(db_path)) as con:
        cur = con.execute(f"PRAGMA table_info({table})")
        return any(str(r[1]).lower() == col.lower() for r in cur.fetchall())

def _get_clause_columns(db_path: Path) -> set[str]:
    with connect(str(db_path)) as con:
        cur = con.execute("PRAGMA table_info(policy_clauses)")
        return {str(r[1]).lower() for r in cur.fetchall()}

def _strip_severity_if_missing(db_path: Path, intent: IntentName, sql: str) -> str:
    if intent != IntentName.ListProhibitions:
        return sql
    if _has_column(db_path, "policy_clauses", "severity"):
        return sql
    sql = re.sub(r"\s+(AND|WHERE)\s+severity\s*>\s*0\s*", " ", sql, flags=re.IGNORECASE)
    sql = re.sub(r"\s{2,}", " ", sql).strip()
    sql = re.sub(r"\sWHERE\s*$", "", sql, flags=re.IGNORECASE)
    return sql

def _strip_missing_equality_preds(db_path: Path, sql: str) -> str:
    cols = _get_clause_columns(db_path)
    candidates = {"jurisdiction": "jurisdiction", "channel": "channel", "consent_type": "consent_type"}
    for param, col in candidates.items():
        if col.lower() in cols:
            continue
        pattern_and = rf"\s+AND\s+{re.escape(col)}\s*=\s*:{re.escape(param)}\s*"
        sql = re.sub(pattern_and, " ", sql, flags=re.IGNORECASE)
        pattern_where_mid = rf"\s+WHERE\s+{re.escape(col)}\s*=\s*:{re.escape(param)}\s*AND\s+"
        sql = re.sub(pattern_where_mid, " WHERE ", sql, flags=re.IGNORECASE)
        pattern_where_end = rf"\s+WHERE\s+{re.escape(col)}\s*=\s*:{re.escape(param)}\s*(?=$|\sORDER|\sLIMIT)"
        sql = re.sub(pattern_where_end, " ", sql, flags=re.IGNORECASE)
        sql = re.sub(r"\s{2,}", " ", sql).strip()
        sql = re.sub(r"\sWHERE\s*(?=$|\sORDER|\sLIMIT)", " ", sql, flags=re.IGNORECASE).strip()
    return sql

def _fix_order_by_missing_columns(db_path: Path, sql: str) -> str:
    cols = _get_clause_columns(db_path)

    # If ORDER BY uses relevance placeholder or rowid, keep as is.
    if re.search(r"ORDER\s+BY\s+/\*\s*relevance\s*\*/\s*1", sql, flags=re.IGNORECASE):
        return sql
    if re.search(r"ORDER\s+BY\s+rowid(\s+DESC|\s+ASC)?", sql, flags=re.IGNORECASE):
        return sql

    m = re.search(r"(ORDER\s+BY)\s+([^\s].*?)(?=\s+LIMIT|\s*$)", sql, flags=re.IGNORECASE)
    if not m:
        return sql

    order_kw, expr = m.group(1), m.group(2).strip()
    # take the first identifier (optionally qualified), ignore comments
    ident_match = re.search(r"([A-Za-z_][A-Za-z0-9_\.]*)", expr)
    if not ident_match:
        return sql

    ident = ident_match.group(1)
    col = ident.rsplit(".", 1)[-1].lower()

    if col in cols:
        return sql  # column exists, nothing to do

    # Prefer precedence if present; otherwise drop ORDER BY
    replacement = None
    if "precedence" in cols:
        replacement = f"{order_kw} precedence DESC"
    else:
        replacement = ""  # remove ORDER BY entirely

    start, end = m.span()
    new_sql = sql[:start] + replacement + sql[end:]
    new_sql = re.sub(r"\s{2,}", " ", new_sql).strip()
    return new_sql

def cmd_run(db_path: Path, path: Path):
    for i, line in enumerate(path.read_text().splitlines(), start=1):
        if not line.strip():
            continue
        try:
            obj = json.loads(line)
            parsed = _validate_line(obj, i)
            intent = IntentName(parsed["intent"])
            slots = NLQParsed(**parsed).slots
            sql, params = compile_sql(intent, slots)
            sql = _strip_severity_if_missing(db_path, intent, sql)
            sql = _strip_missing_equality_preds(db_path, sql)
            sql = _fix_order_by_missing_columns(db_path, sql)
            if intent == IntentName.CountClauses:
                count = run_count(str(db_path), sql, params)
                print(json.dumps({"count": count, "sql": sql, "params": params}, ensure_ascii=False))
            else:
                rows = run_sql(str(db_path), sql, params)
                print(json.dumps({"rows": rows, "sql": sql, "params": params}, ensure_ascii=False))
        except ValidationError as e:
            raise SystemExit(f"Validation error on line {i}: {e.message}")
        except Exception as e:
            raise SystemExit(f"Error on line {i}: {e}")

def cmd_sample():
    obj = {
        "intent": "TopKClauses",
        "slots": {
            "topic": "UnSubScribe",
            "channel": "Newsletter",
            "consent_type": "explicit",
            "jurisdiction": "CA",
            "k": 5,
            "sort_by": "relevance"
        }
    }
    print(json.dumps(obj, ensure_ascii=False, indent=2))

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print(HELP)
        sys.exit(1)
    cmd = sys.argv[1]
    if cmd == "sample":
        cmd_sample()
    elif cmd == "validate" and len(sys.argv) == 3:
        cmd_validate(Path(sys.argv[2]))
    elif cmd == "compile" and len(sys.argv) == 3:
        cmd_compile(Path(sys.argv[2]))
    elif cmd == "run" and len(sys.argv) == 4:
        cmd_run(Path(sys.argv[2]), Path(sys.argv[3]))
    else:
        print(HELP)
        sys.exit(1)

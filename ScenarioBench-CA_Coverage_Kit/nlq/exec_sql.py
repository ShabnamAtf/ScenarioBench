# nlq/exec_sql.py
from __future__ import annotations
import sqlite3
from typing import Iterable, Dict, Any, List, Tuple, Optional

def connect(db_path: str) -> sqlite3.Connection:
    con = sqlite3.connect(db_path)
    con.row_factory = sqlite3.Row
    con.execute("PRAGMA foreign_keys = ON")
    return con

def run_sql(db_path: str, sql: str, params: Optional[Dict[str, Any]] = None) -> List[Dict[str, Any]]:
    params = params or {}
    with connect(db_path) as con:
        cur = con.execute(sql, params)
        rows = cur.fetchall()
    return [dict(r) for r in rows]

def run_count(db_path: str, sql: str, params: Optional[Dict[str, Any]] = None) -> int:
    params = params or {}
    with connect(db_path) as con:
        cur = con.execute(sql, params)
        row = cur.fetchone()
    if row is None:
        return 0
    # support COUNT(*) AS n
    if isinstance(row, sqlite3.Row):
        if "n" in row.keys():
            return int(row["n"])
        # fallback to first column
        return int(list(row)[0])
    return int(row[0])  # type: ignore[index]

def explain_plan(db_path: str, sql: str) -> List[Tuple]:
    with connect(db_path) as con:
        cur = con.execute(f"EXPLAIN QUERY PLAN {sql}")
        return [tuple(r) for r in cur.fetchall()]

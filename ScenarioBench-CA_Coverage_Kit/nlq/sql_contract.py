# nlq/sql_contract.py
from __future__ import annotations
from typing import Dict, Tuple, List, Any
from .intents import IntentName, Slots

TABLES = {
    "clauses": "policy_clauses",
    "sections": "policy_sections",
}

COLUMNS = {
    "policy_clauses": {
        "id": "clause_id",
        "text": "text",
        # extended columns (present in test DB; may be absent in some prod snapshots)
        "section_id": "section_id",
        "channel": "channel",
        "consent_type": "consent_type",
        "jurisdiction": "jurisdiction",
        "severity": "severity",
        "effective_start": "effective_start_date",
        "effective_end": "effective_end_date",
        "domain": "domain",
        # "title" intentionally NOT selected to stay compatible with test DB
    },
    "policy_sections": {
        "id": "section_id",
        "code": "code",
        "title": "title",
    },
}

SORT_KEYS = {
    "relevance": "/* relevance */ 1",
    "recency":   COLUMNS["policy_clauses"].get("effective_start", "rowid") + " DESC",
    "severity":  COLUMNS["policy_clauses"].get("severity", "rowid") + " DESC",
}

def _where_from_slots(intent: IntentName, slots: Slots) -> Tuple[str, Dict[str, Any], List[str]]:
    clauses: List[str] = []
    params: Dict[str, Any] = {}
    joins: List[str] = []

    s = slots.model_dump()

    # equality filters
    for key in ("channel", "consent_type", "jurisdiction"):
        val = s.get(key)
        if val:
            col = COLUMNS["policy_clauses"][key]
            clauses.append(f"{col} = :{key}")
            params[key] = val

    # domain filter
    if s.get("domain"):
        clauses.append(f"{COLUMNS['policy_clauses']['domain']} = :domain")
        params["domain"] = s["domain"]

    # section filter + JOIN
    if s.get("section"):
        if intent != IntentName.ShowSection:
            joins.append(f"JOIN {TABLES['sections']} USING (section_id)")
        clauses.append(f"{COLUMNS['policy_sections']['code']} = :section")
        params["section"] = s["section"]

    # date bounds (if present in backing DB these will work; harmless otherwise in tests)
    if s.get("date_effective_before"):
        clauses.append(f"{COLUMNS['policy_clauses']['effective_start']} <= :date_effective_before")
        params["date_effective_before"] = s["date_effective_before"]
    if s.get("date_effective_after"):
        clauses.append(f"{COLUMNS['policy_clauses']['effective_end']} >= :date_effective_after")
        params["date_effective_after"] = s["date_effective_after"]

    # contains filters
    if s.get("topic"):
        clauses.append(f"{COLUMNS['policy_clauses']['text']} LIKE :topic_like")
        params["topic_like"] = f"%{s['topic']}%"
    if s.get("terms"):
        for i, term in enumerate(s["terms"] or []):
            key = f"term_{i}"
            clauses.append(f"{COLUMNS['policy_clauses']['text']} LIKE :{key}")
            params[key] = f"%{term}%"

    where_sql = ("WHERE " + " AND ".join(clauses)) if clauses else ""
    return where_sql, params, joins

def base_select(intent: IntentName) -> str:
    pc = TABLES["clauses"]
    if intent in (IntentName.FindClauses, IntentName.TopKClauses, IntentName.ListProhibitions):
        # select minimal columns common to both test and prod snapshots
        return f"SELECT {pc}.clause_id, {pc}.text FROM {pc}"
    if intent == IntentName.ShowSection:
        # test DB has policy_sections with code/title; keep the join
        return (
            "SELECT s.section_id, s.code, s.title, c.clause_id, c.text "
            f"FROM {TABLES['sections']} s LEFT JOIN {TABLES['clauses']} c USING (section_id)"
        )
    if intent == IntentName.CountClauses:
        return f"SELECT COUNT(*) AS n FROM {pc}"
    raise ValueError(f"Unsupported intent: {intent}")

def finalize_order_limit(slots: Slots) -> str:
    s = slots.model_dump()
    order = SORT_KEYS.get(s.get("sort_by") or "relevance", SORT_KEYS["relevance"])
    limit = f" LIMIT {int(s['k'])}" if s.get("k") else ""
    return f" ORDER BY {order}{limit}"

def compile_sql(intent: IntentName, slots: Slots) -> Tuple[str, Dict[str, Any]]:
    # ShowSection goes through the join SELECT above; _where_from_slots will add code= filter if provided
    select_sql = base_select(intent)
    where_sql, params, joins = _where_from_slots(intent, slots)
    join_sql = " " + " ".join(joins) if joins else ""
    sql = f"{select_sql}{join_sql} {where_sql}".rstrip()

    # add prohibition heuristic
    if intent == IntentName.ListProhibitions:
        if "WHERE" in sql:
            sql += " AND " + COLUMNS["policy_clauses"]["severity"] + " > 0"
        else:
            sql += " WHERE " + COLUMNS["policy_clauses"]["severity"] + " > 0"

    if intent in (IntentName.FindClauses, IntentName.TopKClauses):
        sql += finalize_order_limit(slots)

    return sql, params

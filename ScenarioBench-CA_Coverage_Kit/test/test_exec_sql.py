import sqlite3
from pathlib import Path

from nlq.intents import IntentName, Slots
from nlq.sql_contract import compile_sql
from nlq.exec_sql import run_sql, run_count

def _seed_db(db_path: Path):
    con = sqlite3.connect(str(db_path))
    con.execute("PRAGMA foreign_keys = ON")
    con.executescript(
        """
        CREATE TABLE policy_sections (
            section_id INTEGER PRIMARY KEY,
            code TEXT NOT NULL,
            title TEXT
        );

        CREATE TABLE policy_clauses (
            clause_id INTEGER PRIMARY KEY,
            section_id INTEGER NOT NULL,
            text TEXT NOT NULL,
            channel TEXT,
            consent_type TEXT,
            jurisdiction TEXT,
            severity INTEGER DEFAULT 0,
            effective_start_date TEXT,
            effective_end_date TEXT,
            FOREIGN KEY (section_id) REFERENCES policy_sections(section_id)
        );
        """
    )
    con.execute(
        "INSERT INTO policy_sections(section_id, code, title) VALUES (1, 'CASL-3.1', 'Email requirements')"
    )
    con.execute(
        """INSERT INTO policy_clauses
           (clause_id, section_id, text, channel, consent_type, jurisdiction, severity, effective_start_date, effective_end_date)
           VALUES (101, 1, 'Email unsubscribe link required; express consent rules apply.', 'email', 'express', 'CA', 2, '2020-01-01', '2099-12-31')"""
    )
    con.execute(
        """INSERT INTO policy_clauses
           (clause_id, section_id, text, channel, consent_type, jurisdiction, severity, effective_start_date, effective_end_date)
           VALUES (102, 1, 'No robocalls without consent; children protection applies.', 'phone', 'implied', 'CA', 1, '2020-01-01', '2099-12-31')"""
    )
    con.execute(
        """INSERT INTO policy_clauses
           (clause_id, section_id, text, channel, consent_type, jurisdiction, severity, effective_start_date, effective_end_date)
           VALUES (103, 1, 'Contest posts on social media must include terms; children guidance may apply.', 'social', 'implied', 'CA', 0, '2020-01-01', '2099-12-31')"""
    )
    con.commit()
    con.close()

def test_run_topk_unsubscribe(tmp_path):
    db = tmp_path / "policy.db"
    _seed_db(db)

    slots = Slots(topic="unsubscribe", channel="email", consent_type="express", jurisdiction="CA", k=5)
    sql, params = compile_sql(IntentName.TopKClauses, slots)
    rows = run_sql(str(db), sql, params)
    assert len(rows) == 1
    assert rows[0]["clause_id"] == 101

def test_run_count_children_on_social(tmp_path):
    db = tmp_path / "policy.db"
    _seed_db(db)

    slots = Slots(topic="children", channel="social", jurisdiction="CA")
    sql, params = compile_sql(IntentName.CountClauses, slots)
    n = run_count(str(db), sql, params)
    assert n == 1

def test_show_section_join_no_duplicate_join(tmp_path):
    db = tmp_path / "policy.db"
    _seed_db(db)

    slots = Slots(section="CASL-3.1", jurisdiction="CA")
    sql, params = compile_sql(IntentName.ShowSection, slots)
    rows = run_sql(str(db), sql, params)
    assert any(r["code"] == "CASL-3.1" for r in rows)

def test_list_prohibitions_uses_severity(tmp_path):
    db = tmp_path / "policy.db"
    _seed_db(db)

    slots = Slots(channel="phone", jurisdiction="CA")
    sql, params = compile_sql(IntentName.ListProhibitions, slots)
    rows = run_sql(str(db), sql, params)
    # row 102 has severity 1 → should be returned
    assert any(r["clause_id"] == 102 for r in rows)

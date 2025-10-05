from nlq.intents import IntentName, Slots
from nlq.sql_contract import compile_sql

def test_compile_topk_basic():
    slots = Slots(
        topic="unsubscribe",
        channel="email",
        consent_type="express",
        jurisdiction="CA",
        k=5,
        sort_by="relevance",
    )
    sql, params = compile_sql(IntentName.TopKClauses, slots)
    assert "SELECT policy_clauses.clause_id" in sql
    assert "FROM policy_clauses" in sql
    assert "WHERE" in sql
    assert "ORDER BY" in sql and "LIMIT 5" in sql
    assert params["channel"] == "email"
    assert params["consent_type"] == "express"
    assert params["jurisdiction"] == "CA"
    assert params["topic_like"] == "%unsubscribe%"

def test_compile_show_section():
    slots = Slots(section="CASL-3.1", jurisdiction="CA")
    sql, params = compile_sql(IntentName.ShowSection, slots)
    assert "FROM policy_sections s LEFT JOIN policy_clauses c USING (section_id)" in sql
    assert "code = :section" in sql
    assert params["section"] == "CASL-3.1"

def test_compile_list_prohibitions_adds_severity():
    slots = Slots(channel="phone", jurisdiction="CA")
    sql, params = compile_sql(IntentName.ListProhibitions, slots)
    assert "severity > 0" in sql
    assert params["channel"] == "phone"

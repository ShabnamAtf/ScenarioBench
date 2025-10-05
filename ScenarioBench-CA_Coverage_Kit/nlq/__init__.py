"""
Public API for the NLQ layer (Step 12).
"""

from .intents import IntentName, Slots, NLQParsed
from .normalize import normalize_slots
from .sql_contract import compile_sql
from .exec_sql import run_sql, run_count, explain_plan

__all__ = [
    "IntentName",
    "Slots",
    "NLQParsed",
    "normalize_slots",
    "compile_sql",
    "run_sql",
    "run_count",
    "explain_plan",
]

__version__ = "0.1.0"

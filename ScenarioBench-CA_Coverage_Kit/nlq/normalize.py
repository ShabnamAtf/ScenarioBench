import re, json, yaml
from pathlib import Path
from typing import Optional
from .intents import Slots

_LEX = yaml.safe_load(Path(__file__).with_name("lexicon.yaml").read_text())["synonyms"]

def _canon_from_syn(category: str, value: str) -> Optional[str]:
    if value is None:
        return None
    v = re.sub(r"\s+", " ", str(value).lower().strip())
    # direct canonical key match
    if category in _LEX and isinstance(_LEX[category], dict) and v in _LEX[category].keys():
        return v
    # synonym lists -> canonical key
    if category in _LEX and isinstance(_LEX[category], dict):
        for canon, syns in _LEX[category].items():
            if v == canon or v in [s.lower() for s in syns]:
                return canon
    return v

def normalize_slots(slots: dict) -> Slots:
    slots = dict(slots or {})
    slots["channel"] = _canon_from_syn("channel", slots.get("channel"))
    slots["topic"] = _canon_from_syn("topic", slots.get("topic"))
    slots["consent_type"] = _canon_from_syn("consent_type", slots.get("consent_type"))
    slots["domain"] = _canon_from_syn("domain", slots.get("domain"))
    # unify date formats like 2025/09/12 -> 2025-09-12
    for key in ("date_effective_before","date_effective_after"):
        if slots.get(key):
            s = re.sub(r"[/.]", "-", str(slots[key]).strip())
            slots[key] = s
    return Slots(**slots)

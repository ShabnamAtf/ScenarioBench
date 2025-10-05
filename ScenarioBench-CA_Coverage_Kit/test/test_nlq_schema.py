import json, pathlib
from nlq.cli import _validate_line

def test_sample_validates():
    obj = {
      "intent":"TopKClauses",
      "slots":{"topic":"opt out","channel":"txt","consent_type":"opt in","k":3}
    }
    validated = _validate_line(obj, 1)
    assert validated["slots"]["channel"] == "sms"
    assert validated["slots"]["consent_type"] == "opt-in"
    assert validated["intent"] == "TopKClauses"
def test_normalizes_dates_and_robocall():
    obj = {
      "intent":"FindClauses",
      "slots":{"channel":"robocall","date_effective_before":"2025/12/31"}
    }
    from nlq.cli import _validate_line
    v = _validate_line(obj, 1)
    assert v["slots"]["channel"] == "phone"
    assert v["slots"]["date_effective_before"] == "2025-12-31"

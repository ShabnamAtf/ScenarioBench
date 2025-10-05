# nlq/intents.py
from enum import Enum
from typing import List, Optional, Literal
from pydantic import BaseModel, Field

class IntentName(str, Enum):
    FindClauses = "FindClauses"
    TopKClauses = "TopKClauses"
    CountClauses = "CountClauses"
    ShowSection = "ShowSection"
    ListProhibitions = "ListProhibitions"

Channel = Literal["email","sms","social","phone","in-app","web","postal"]
ConsentType = Literal["opt-in","opt-out","implied","express"]
Jurisdiction = Literal["CA","ON","QC","EU","US"]
SortBy = Literal["relevance","recency","severity"]
Domain = Literal["CASL","PIPEDA","ADV","ADSTAND","CRTC","QL25","GEN"]

class Slots(BaseModel):
    topic: Optional[str] = None
    channel: Optional[Channel] = None
    consent_type: Optional[ConsentType] = None
    purpose: Optional[Literal["marketing","research","service","third-party-sharing"]] = None
    sensitive_category: Optional[Literal["health","financial","children","biometric","location","none"]] = None
    audience_age: Optional[int] = Field(default=None, ge=0)
    jurisdiction: Optional[Jurisdiction] = "CA"
    section: Optional[str] = None
    domain: Optional[Domain] = None
    terms: Optional[List[str]] = None
    sort_by: Optional[SortBy] = "relevance"
    k: Optional[int] = Field(default=None, ge=1, le=50)
    date_effective_before: Optional[str] = None   # YYYY-MM-DD
    date_effective_after: Optional[str] = None

class NLQParsed(BaseModel):
    intent: IntentName
    slots: Slots

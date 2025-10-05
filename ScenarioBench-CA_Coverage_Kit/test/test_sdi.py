# tests/test_sdi.py
import pytest
from scripts.metrics import SDIConfig, sdi, sdi_r


def _norm(wD, wT, wR):
    s = wD + wT + wR
    return wD / s, wT / s, wR / s


def test_sdi_perfect_retrieval():
    cfg = SDIConfig()
    wD, wT, wR = _norm(cfg.w_D, cfg.w_T, cfg.w_R)
    val = sdi(1.0, 1.0, 1.0, 1.0, 1.0, cfg)  # R=0
    expected = wD * 1.0 + wT * 1.0 + wR * 0.0
    assert val == pytest.approx(expected, rel=1e-9, abs=1e-9)


def test_sdi_worst_retrieval_caps_at_one():
    cfg = SDIConfig()
    val = sdi(1.0, 1.0, 1.0, 1.0, 0.0, cfg)  # R=1
    assert val == pytest.approx(1.0)


def test_sdi_clips_inputs_to_01():
    cfg = SDIConfig()
    # After clipping: D=1, T_c=0, T_k=1, T_o=0.5 => T=0.5; ndcg=0 => R=1
    val = sdi(1.7, -0.2, 2.0, 0.5, -3.0, cfg)
    wD, wT, wR = _norm(cfg.w_D, cfg.w_T, cfg.w_R)
    expected = wD * 1.0 + wT * 0.5 + wR * 1.0
    # Defensive bound:
    expected = max(0.0, min(1.0, expected))
    assert val == pytest.approx(expected, rel=1e-9)


def test_sdi_r_penalizes_latency_overhead():
    cfg = SDIConfig(lam=0.3)
    base, obs = 2.0, 3.0  # rho = 0.5
    s0 = sdi(0.8, 0.6, 0.6, 0.6, 0.7, cfg)
    val = sdi_r(0.8, 0.6, 0.6, 0.6, 0.7, base, obs, cfg)
    expected = s0 * (1 - cfg.lam * 0.5)
    assert val == pytest.approx(expected, rel=1e-9)


def test_sdi_r_no_penalty_when_latency_missing():
    cfg = SDIConfig()
    s0 = sdi(0.9, 0.8, 0.7, 0.6, 0.4, cfg)
    assert sdi_r(0.9, 0.8, 0.7, 0.6, 0.4, None, 2.0, cfg) == pytest.approx(s0)
    assert sdi_r(0.9, 0.8, 0.7, 0.6, 0.4, 2.0, None, cfg) == pytest.approx(s0)


def test_sdi_r_raises_on_nonpositive_base():
    cfg = SDIConfig()
    with pytest.raises(ValueError):
        sdi_r(0.5, 0.5, 0.5, 0.5, 0.5, 0.0, 1.0, cfg)


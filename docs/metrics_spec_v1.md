# ScenarioBench-CA: SDI & SDI-R (v1)

## Signals (normalized to [0,1])
- Decision quality: \(D = \mathrm{F1}_{\text{decision}}\).
- Trace quality: completeness \(T_c\), correctness \(T_k\), order \(T_o\);
  \[
  T=\frac{T_c+T_k+T_o}{3}.
  \]
- Retrieval difficulty: let \(N@k=\mathrm{nDCG}@k\) for gold clauses (default \(k=10\));
  \[
  R = 1 - N@k.
  \]

## Weights (sum to 1)
\[
w_D=0.5,\quad w_T=0.3,\quad w_R=0.2.
\]

## SDI
\[
\mathrm{SDI}=w_D\cdot D+w_T\cdot T+w_R\cdot R.
\]

## SDI-R (reflect-time penalty)
Let \(L_{\text{base}}\) be latency without reflection and \(L\) the observed latency with reflection.
\[
\rho=\min\!\Big(1,\;\frac{L-L_{\text{base}}}{L_{\text{base}}}\Big),\qquad \lambda=0.3.
\]
\[
\mathrm{SDI\!-\!R}=\mathrm{SDI}\cdot(1-\lambda\cdot \rho).
\]

**Defaults:** \(k=10\), \(\lambda=0.3\). All inputs are clipped to \([0,1]\).

# IV Replication: Angrist & Krueger (1991)

Replication of core results from **"Does Compulsory School Attendance Affect Schooling and Earnings?"** (Angrist & Krueger, 1991, *Quarterly Journal of Economics*) using R.

## Research Question

What is the **causal return to an additional year of education on wages?** OLS estimates are likely upward-biased — people with higher ability tend to get more schooling *and* earn more, making it hard to isolate the effect of education itself.

## Identification Strategy

AK91 exploit **quarter of birth (QOB)** as an instrumental variable.

- U.S. compulsory schooling laws require students to stay in school until they turn 16 (or 17/18, depending on state).
- Students born earlier in the year (Q1: Jan–Mar) reach the legal dropout age having completed *less* schooling than students born later in the year.
- QOB is plausibly **exogenous** (parents don't strategically time births to affect their child's education) and affects wages **only through** its effect on schooling — satisfying the exclusion restriction.

**Instrument:** `qob1` — indicator for being born in Q1 (January–March)  
**Endogenous variable:** `s` — years of education  
**Outcome:** `lnw` — log weekly wage  

## Data

`ak91.csv` — 1980 U.S. Census microdata, men born 1930–1939.

| Variable | Description |
|----------|-------------|
| `lnw`    | Log weekly wage |
| `s`      | Years of completed education |
| `qob`    | Quarter of birth (1–4) |
| `qob1`   | Constructed indicator: 1 if born in Q1, 0 otherwise |

## Script Overview (`ak91_iv_replication.R`)

| Section | Description |
|---------|-------------|
| 1. Setup | Package installation and loading |
| 2. Data | Load data, construct `qob1` instrument |
| 3. Wald Estimates | Replication of AK91 Table III, Panel B |
| 4. First Stage | OLS: `s ~ qob1` — instrument relevance check |
| 5. Reduced Form | OLS: `lnw ~ qob1` — ITT effect of QOB on wages |
| 6. IV / 2SLS | `ivreg(lnw ~ s | qob1)` with weak instrument diagnostics |
| 7. Comparison | Stargazer table: OLS vs. IV side by side |

## Key Results

The IV estimate of the return to education is larger than the OLS estimate, which is consistent with **downward ability bias** (or LATE effects — the IV identifies returns specifically for compliers, i.e., individuals whose schooling was constrained by compulsory attendance laws).

## Packages

```r
tidyverse, AER, sandwich, lmtest, stargazer, knitr
```

## Reference

Angrist, J. D., & Krueger, A. B. (1991). Does compulsory school attendance affect schooling and earnings? *Quarterly Journal of Economics*, 106(4), 979–1014.

# Matched-FTA impact by GTAP-23 sector — four specifications

Parallel to `FTA_GTAP23/`, but the treatment is the **matched** FTA variable (a
homogeneous set of similar agreements, from `regression_data_2.rds`), with the
**other** (unmatched) FTA group kept as a **time-varying control dummy** and
no-FTA pairs as the baseline.

Three mutually-exclusive groups per pair-year: `FTA_matched=1` (reported),
`FTA_other=1` (control), both 0 (no FTA). The matched/other split is defined
separately for **agriculture** and **goods**; agri sectors (`aff, b_t, ofd,
prf`) use the agri split, all others use the goods split. HS6 is merchandise,
so services are not estimated.

## Stages (built and pushed one at a time)
| Stage | Script | Treatment | Controls / FE |
|---|---|---|---|
| **1** | `Stage 1 - … Baseline` | matched FTA dummy (+ other control) | gravity controls + tariff; HS6-level country-time FE |
| 2 | ETWFE | ETWFE on matched (other = control dummy) | gravity controls + tariff |
| 3 | Bilateral FE | matched dummy (+ other control) | pair FE `i^j` replaces gravity controls |
| 4 | ETWFE + Bilateral FE | ETWFE on matched | pair FE `i^j` replaces gravity controls |

`finalize_matched_table.R` auto-detects the completed stages and rebuilds the
publication table (`results/Table_matched_FTA_by_GTAP23.html` / `.tex`) and the
combined CSV. SEs are pair-clustered throughout.

## Status: STAGES 1–2 complete
Share of the 14 sectors with a positive & significant matched-FTA effect:

| Stage | +sig 1% | +sig 5% | +sig 10% |
|---|--:|--:|--:|
| (1) Baseline | 64.3% | 64.3% | 71.4% |
| (2) ETWFE    | 71.4% | 78.6% | 78.6% |

The **matched** FTA effects are larger and more consistently significant than
the "complete FTA" effects were — plausible for a homogeneous set of deeper
agreements — and the ETWFE strengthens them further (e.g. Transport equipment
flips from −0.19 to +0.61***).

## Data / inputs (not committed)
`regression_data_2.rds` (the matched/other FTA data) and the per-HS6
`Sigma_HS6_*.csv` trade files are kept local (git-ignored). Adjust
`project_root` in each script to point at them.

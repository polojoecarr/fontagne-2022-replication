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

## Status: STAGE 1 complete
Matched-FTA effect (baseline), share of the 14 sectors positive & significant:
**1%: 64.3% · 5%: 64.3% · 10%: 71.4%** (85.7% positive). Effects range from
−0.19 (Transport equipment) to +1.01 (Energy & extraction).

## Data / inputs (not committed)
`regression_data_2.rds` (the matched/other FTA data) and the per-HS6
`Sigma_HS6_*.csv` trade files are kept local (git-ignored). Adjust
`project_root` in each script to point at them.

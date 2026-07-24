# FTA impact by GTAP-23 sector — three methods

Work-in-progress analysis extending the Fontagné (2022) product-level machinery
to estimate the average effect of Free Trade Agreements (FTAs) on trade, one
number per GTAP-23 sector, under three estimation strategies.

FTA membership comes from the **Deep Trade Agreements (DTA)** dataset
(`Bilateral Information` sheet); HS6 products are pooled to **GTAP-23** sectors
via the GTAP concordances.

## The three runs (`scripts/`)
| Script | Method |
|--------|--------|
| `Run 1 - Base Fontagne` | PPML with a single FTA dummy (plain two-way fixed effects), HS6-level FEs + gravity controls + tariff. |
| `Run 2 - ETWFE Staggered DiD` | Replaces the FTA dummy with the heterogeneity-robust Extended TWFE / staggered diff-in-diff estimator (Nagengast & Yotov 2023; Wooldridge 2021, 2023), using a **cohort-specific pre-period baseline**, then aggregates the cohort-year effects to one number per sector. |
| `Run 3 - Bilateral Fixed Effects` | Run 1 with the four gravity controls replaced by a country-pair (`i^j`) fixed effect. |
| `Run 4 - Combine Results` | Builds the side-by-side comparison table across the three runs. |

Each script is self-contained and follows the same 5-section skeleton, so they
are easy to compare and edit. Standard errors are pair-clustered throughout.

## Status — COMPLETE (all 14 GTAP-23 goods sectors, all three runs)
- **Run 1** — `results/results_run1_base.csv`
- **Run 2** — `results/results_run2_etwfe.csv`
- **Run 3** — `results/results_run3_pairfe.csv`
- **Combined** — `results/COMBINED_comparison_by_sector.csv` and
  `results/COMBINED_significance_summary.csv`

## Headline (share of the 14 sectors positive & significant)
| Run | any positive | +sig 1% | +sig 5% | +sig 10% |
|-----|-------------:|--------:|--------:|---------:|
| Run 1 (TWFE dummy)          | 100%  | 85.7% | 92.9% | 100%  |
| Run 2 (ETWFE staggered DiD) | 85.7% | 50.0% | 50.0% | 57.1% |
| Run 3 (pair fixed effect)   | 35.7% | 14.3% | 14.3% | 14.3% |

The estimated FTA effect shrinks sharply as the identification gets stricter.
The plain TWFE dummy finds a large, almost-universally-significant positive
effect; the ETWFE removes the "forbidden comparisons" and is far more
conservative (two sectors even turn negative); and the pair fixed effect —
which identifies the effect purely from within-pair change across only six
(non-consecutive) waves — washes most of it out, leaving only `mff` and `ofd`
positive and significant. This ordering is the expected consequence of each
method's identification, and is exactly the comparison the three runs are
designed to surface.

## Inputs NOT included here
The raw source data is kept locally and is not committed:
`DTA 2.0 - Vertical Content (v2).xlsx` (Deep Trade Agreements), the GTAP↔HS6 and
GTAP-65↔GTAP-23 concordances, and the per-HS6 `Sigma_HS6_*.csv` trade+tariff
files. Set `project_root` at the top of each script to point at their location.

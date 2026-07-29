# FTA impact by GTAP-23 sector — four methods

Analysis extending the Fontagné (2022) product-level machinery to estimate the
average effect of Free Trade Agreements (FTAs) on trade, one number per GTAP-23
sector, under **four** estimation strategies.

FTA membership comes from the **Deep Trade Agreements (DTA)** dataset
(`Bilateral Information` sheet); HS6 products are pooled to **GTAP-23** sectors
via the GTAP concordances.

## The four regression runs (`scripts/`)
| Script | Treatment | Controls / FE |
|--------|-----------|---------------|
| `Run 1 - Base Fontagne` | single FTA dummy (plain TWFE) | gravity controls + tariff; HS6-level country-time FE |
| `Run 2 - ETWFE Staggered DiD` | ETWFE cohort-year effects with a **cohort-specific pre-period baseline** (Nagengast & Yotov 2023; Wooldridge 2021, 2023), aggregated to one number | gravity controls + tariff; HS6-level country-time FE |
| `Run 3 - Bilateral Fixed Effects` | single FTA dummy | **pair FE `i^j`** replaces the gravity controls |
| `Run 4 - ETWFE plus Bilateral FE` | ETWFE (as Run 2) | **pair FE `i^j`** replaces the gravity controls (Runs 2 + 3 combined — closest to Nagengast & Yotov's own specification) |

Table scripts: `Run 4 - Combine Results.R` (simple side-by-side) and
`com_table.R` (publication-style `gt` table, **Section 5** builds the four-spec
version). Standard errors are pair-clustered throughout; each script follows the
same self-contained 5-section skeleton.

## Status — COMPLETE (all 14 GTAP-23 goods sectors, all four runs)
- Per-run results: `results/results_run{1..4}_*.csv`
- Combined: `results/COMBINED_comparison_4runs.csv`
- Publication table: `results/Table_FTA_by_GTAP23_4specs.html` and `.tex`

## Headline (share of the 14 sectors positive & significant)
| Run | any positive | +sig 1% | +sig 5% | +sig 10% |
|-----|-------------:|--------:|--------:|---------:|
| (1) TWFE dummy            | 100%  | 85.7% | 92.9% | 100%  |
| (2) ETWFE                 | 85.7% | 50.0% | 50.0% | 57.1% |
| (3) Bilateral (pair) FE   | 35.7% | 14.3% | 14.3% | 14.3% |
| (4) ETWFE + Bilateral FE  | 42.9% | 14.3% | 14.3% | 21.4% |

The estimated FTA effect shrinks sharply as the identification gets stricter.
The plain TWFE dummy finds a large, almost-universally-significant positive
effect; the ETWFE removes the "forbidden comparisons" and is more conservative
(two sectors turn negative); and the specifications with a country-pair fixed
effect — which identify the effect purely from within-pair change across only
six (non-consecutive) waves — wash most of it out. Run 4, which combines the
ETWFE treatment with the pair fixed effect, is the strictest and closest to the
Nagengast & Yotov (2023) design.

## Inputs NOT included here
The raw source data is kept locally and is not committed:
`DTA 2.0 - Vertical Content (v2).xlsx` (Deep Trade Agreements), the GTAP↔HS6 and
GTAP-65↔GTAP-23 concordances, and the per-HS6 `Sigma_HS6_*.csv` trade+tariff
files. Set `project_root` at the top of each script to point at their location.

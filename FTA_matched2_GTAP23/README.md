# Matched-FTA impact by GTAP-23 sector — SECOND matched table (`regression_data_3`)

A robustness counterpart to `FTA_matched_GTAP23/`. Everything about the design is
identical; the **only** change is that the matched/other split comes from
**`regression_data_3.rds`** instead of `regression_data_2.rds`.

The `_3` matched group is roughly **half the size** of the `_2` one — 1,280 vs
2,326 treated goods pair-years and 990 vs 2,188 agri pair-years — so it is a
tighter, more homogeneous set of "similar" agreements. Within the six estimation
waves this leaves 86 treated goods pairs and 62 treated agri pairs (directed,
excluding always-treated), i.e. a matched share of roughly 0.2% of observations.

As before, three mutually-exclusive groups per pair-year: `FTA_matched = 1`
(reported), `FTA_other = 1` (time-varying control), both 0 (no FTA, the
baseline). Agri sectors (`aff, b_t, ofd, prf`) use the agriculture split; all
others use the goods split. HS6 is merchandise, so services are not estimated.

## Specifications — three only
Specification (2) (ETWFE with gravity controls) is **deliberately not run** for
this table. Column numbering is kept aligned with the first matched table so the
two can be read side by side.

| Spec | Script | Treatment | Controls / FE |
|---|---|---|---|
| **(1)** | `Stage 1 - Matched2 … Baseline` | matched FTA dummy (+ other control) | gravity controls + tariff; HS6-level country-time FE |
| **(3)** | `Stage 3 - Matched2 … Bilateral FE` | matched FTA dummy (+ other control) | pair FE `i^j` replaces gravity controls |
| **(4)** | `Stage 4 - Matched2 … ETWFE plus Bilateral FE` | ETWFE on matched | pair FE `i^j` replaces gravity controls |

`finalize_matched2_table.R` auto-detects which stages are complete and rebuilds
`results/Table_matched2_FTA_by_GTAP23.html` / `.tex` plus
`results/COMBINED_matched2_comparison.csv`. SEs are pair-clustered throughout.

## Status
| Stage | State | Runtime |
|---|---|--:|
| (1) Baseline | **complete**, 14/14 sectors | 72.4 min |
| (3) Bilateral FE | in progress | est. ~4.5 h |
| (4) ETWFE + Bilateral FE | not yet run | est. ~8.7 h in 4 chunks |

Stage 4 is chunked (`chunk_sectors` near the top of its script) so it can be run
in sittings: Chunk 1 = the 10 small/medium sectors, Chunk 2 = `ome tal`,
Chunk 3 = `mff`, Chunk 4 = `crp`. The loop skips sectors already saved.

## Data / inputs (not committed)
`regression_data_3.rds` and the per-HS6 `Sigma_HS6_*.csv` trade files are kept
local and git-ignored. Adjust `project_root` in each script to point at them.

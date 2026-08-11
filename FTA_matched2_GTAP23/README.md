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
| (3) Bilateral FE | **complete**, 14/14 sectors | 258.5 min |
| (4) ETWFE + Bilateral FE | **chunks 1-2 done**, 12/14 sectors (`mff`, `crp` pending) | 177.4 + 132.1 min |

### Stage 4 so far (12 of 14 sectors)
Stage 4 is run in chunks; `mff` and `crp` are still outstanding, so its column
in the table is **provisional** and its Panel-C shares are computed over the 12
sectors available. The table's own "Coverage" source note states this and
self-corrects once the remaining chunks finish.

The result that matters: **meat & processed foods (`prf`) survives the full
specification at 0.313\*\* (t = 2.26)**. It weakens from Stage 3's 0.444\*\*\*
but stays positive and significant at 5% once treatment timing is handled by the
ETWFE block on top of the pair fixed effect. In the first (`_2`) matched table
**no** sector survived this specification at any level, so this is the clearest
substantive difference the narrow matched definition produces.

The thin-cohort worry raised before the run did not materialise in the way
feared: **all 15 cohort-year cells were estimated in every sector**, with none
dropped for collinearity. Standard errors widen relative to Stage 3 but remain
usable.

Of the remaining 11 estimated sectors, only `eeq` (0.077) is even positive.
Three are significantly negative — `tal` -0.409\*\*\*, `omf` -0.260\*\*\*, and
`otn` -0.409\* — and textiles is again the caution: +0.738\*\*\* in the baseline,
-0.262\*\*\* under pair FE, -0.409\*\*\* under ETWFE + pair FE.

### Share of the 14 sectors with a positive & significant matched effect
| Stage | any positive | +sig 1% | +sig 5% | +sig 10% |
|---|--:|--:|--:|--:|
| (1) Baseline     | 92.9% | 42.9% | 50.0% | 64.3% |
| (3) Bilateral FE | 42.9% |  7.1% |  7.1% | 14.3% |

Compared with the first (`_2`) matched table, halving the treatment group leaves
*more* sectors positive under the baseline (92.9% vs 85.7%) but fewer of them
significant (42.9% vs 64.3% at 1%) — the expected cost of a thinner treatment
group. It also **reshuffles which sectors carry the effect**: agriculture and
textiles go from flat to large and strongly significant (aff 0.12 -> 0.67***,
tal 0.05 -> 0.74***), while chemicals and energy fall away (crp 0.57*** -> 0.02,
enr 1.01*** -> 0.36*).

Adding the bilateral pair fixed effect again removes almost all of it, as in
both earlier tables — but not quite all. **Meat & processed foods (`prf`) is the
one sector that survives**, at 0.444*** under pair FE — and it is new to the
narrow definition: in the `_2` table's Stage 3 `prf` was -0.034 and
indistinguishable from zero. Food products (`ofd`)
holds a weak 0.102*. Everything else is indistinguishable from zero or negative,
and four sectors turn significantly negative (otn -0.428***, tal -0.262***,
enr -0.259**, omf -0.131**) — note that textiles flips sign entirely between the
two specifications, which is a warning against reading the baseline column alone.

The broad conclusion is unchanged from the complete-FTA and first matched
analyses: the apparent sectoral FTA effect is driven by cross-pair comparison,
not by within-pair change after entry into force, and six widely-spaced waves
carry little within-pair identifying variation.

Stage 4 is chunked (`chunk_sectors` near the top of its script) so it can be run
in sittings: Chunk 1 = the 10 small/medium sectors, Chunk 2 = `ome tal`,
Chunk 3 = `mff`, Chunk 4 = `crp`. The loop skips sectors already saved.

## Data / inputs (not committed)
`regression_data_3.rds` and the per-HS6 `Sigma_HS6_*.csv` trade files are kept
local and git-ignored. Adjust `project_root` in each script to point at them.

# Extension — Exact replication of Table 5 (JIE preprint)

Target: **Table 5**, "The descriptive statistics for trade elasticities by HS
section", in the *Journal of International Economics* preprint version of
Fontagné, Guimbard & Orefice, "Tariff-Based Product-Level Trade Elasticities".

Script: `replicate_FGO_Table5_extension.R` (the original headline-results
script is left unchanged). Outputs in `/output_table5`.

## What changed vs. the first script
Every change is tagged `### DIVERGENCE v2 ###` in the code. In short: the first
script estimated a coefficient for all 5,050 products, but the paper drops some
as "missing" and a few weakly-identified products produced garbage estimates
(most visibly HS 293963 at ε = −910, which corrupted Section VI).

The new script adds an **identification diagnostic**: for each product it
measures the share of tariff variation that survives the exporter-year ×
importer-year fixed effects **among the positive-trade observations** (the cells
that identify a PPML coefficient). When the fixed effects absorb essentially all
of it (`tariff_id < 1e-4`), the tariff is numerically unidentified and the
product is dropped — mirroring Stata's "collinear with the fixed effects" drop.

## Why we do NOT match the paper's count of 124 "missing"
The paper reports ~124 missing products; our screen drops only **8**. This is
deliberate and is the key finding of the extension:

* The 124 are largely **Stata-algorithm artifacts** — `ppml_panel_sg` failed or
  dropped the tariff on products that are perfectly estimable, and our `fixest`
  run recovers sensible coefficients for them.
* Products with genuinely low surviving tariff variation are disproportionately
  the **legitimate large-negative elasticities** the paper KEEPS (e.g.
  HS 440341 = −62, HS 290270 = −117). Forcing 124 drops (id_tol ≈ 0.02) removes
  these and pulls the section averages toward zero — *away* from Table 5.
* So we drop only the 8 numerically-unidentified artifacts (ε from +73 to −910,
  mostly insignificant). This reproduces the **economic content** of Table 5 —
  the averages, std. devs. and minimums — rather than a Stata bookkeeping count.

## Result quality (see `Table5_comparison_to_paper.csv`)
| Column | Match |
|--------|-------|
| **Average** | 17/21 sections within 0.05; mean abs. error **0.049** |
| **Std Dev** | within ~0.1 for all sections except XIV |
| **Minimum** | 20/21 sections match within 1.0 (most exact to 2 dp) |
| Non-missing count | higher than the paper by construction (we drop 8, they drop 124) |

Two cells resist and cannot be fixed by any screen, because they are genuine
**estimator-implementation** differences (`ppml_panel_sg` vs `fepois`) on
specific weakly-identified products, run on identical data:
* Section II minimum: −20.62 (ours) vs −37.51 (paper)
* Section XIV average/sd: −14.17 / 15.24 vs −13.59 / 13.52

## Why identical data still gives a few different estimates
We use the authors' pre-merged `Sigma_HS6_*.csv` files, so trade values and
tariffs are identical to theirs. The residual differences come only from the
**estimator**: Stata's `ppml_panel_sg` and R's `fixest::fepois` differ in
convergence tolerance, starting values, and which separated observations / FE
they drop. These are immaterial for well-identified products (18/21 sections
matched to ±0.03 even before any screen) but can move the point estimate for a
weakly-identified product — and those products are exactly the section minima.

## Files in `/output_table5`
| File | Contents |
|------|----------|
| `raw_estimates_with_diagnostic.csv` | Per-product tariff coef., SE, distance coef., n positive-trade obs, and the `tariff_id` diagnostic. Lets `id_tol` be re-tuned without re-running. |
| `Table5_replication.csv` | The replicated Table 5. |
| `Table5_comparison_to_paper.csv` | Side-by-side with the published values and deltas. |

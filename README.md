# Replication — Fontagné, Guimbard & Orefice (2022)
## "Product-Level Trade Elasticities: Worth Weighting For"

This folder reproduces the **headline results** of the paper: the product-level
(HS 6-digit) trade elasticities, their distribution (Figure 1), and the
descriptive statistics by HS section (Table 4). Robustness checks and the
welfare exercise are intentionally not reproduced.

## How it works
`replicate_FGO_elasticities.R` estimates the paper's **Equation 5** by PPML,
once for each of the **5,050** HS6 products, using `fixest::fepois` with
exporter-year and importer-year fixed effects and robust standard errors. This
is a direct R/tidyverse translation of the authors' Stata file
`../Stata/baseline_v4.do` (which used `ppml_panel_sg`). The ~5,050 independent
regressions are run in parallel with **furrr** (see Section 4 of the script).

The trade elasticity is recovered as `epsilon_k = 1 + beta_k`, where `beta_k`
is the coefficient on `ln(1 + tariff)`.

## Data
The regressions read the authors' pre-merged per-product panels in
`../Replic_FGO/Replic_FGO/Sigma_HS6_<code>.csv`. Each file already combines
BACI trade values, MAcMap-HS6 applied tariffs, and CEPII gravity variables, so
the raw BACI / Gravity folders are not used directly.

## Outputs (`/output`)
| File | Contents |
|------|----------|
| `elasticity_estimates.csv` | **The regression dataset** — one row per HS6 product: tariff coefficient, std. error, t-stat, significance flag, distance coefficient, and trade elasticity (point estimate and zero-replaced). |
| `Figure1_trade_elasticity_distribution.png` | Replication of the paper's Figure 1 (distribution of `epsilon_k`, centred around -5). |
| `headline_results.txt` | Headline summary statistics vs. the paper's reported figures. |
| `table4_by_HS_section.csv` | Replication of the paper's Table 4 (mean / sd / min elasticity by HS section). |

## How close is the replication?
| Statistic | Paper | This replication |
|-----------|-------|------------------|
| Trade elasticity, with zeros — mean / median | -5.5 / -4.0 | -5.41 / -3.39 |
| Trade elasticity, without zeros — mean / median | -9.8 / -7.3 | -10.35 / -7.56 |
| Tariff coef. significant at 1% | 61% | 57.2% |
| Median t-statistic | 3.2 | 3.06 |
| Mean distance elasticity | ≈ -1 | -1.08 |

Small differences are expected: this replication uses a current BACI vintage in
the **HS96** nomenclature, whereas the paper used an earlier release built on
the **HS07** classification.

## To re-run
Open `replicate_FGO_elasticities.R` in RStudio and source it. The only path you
may need to edit is `project_root` near the top. Required packages: `fixest`,
`data.table`, `furrr`, `future`, and the tidyverse (`dplyr`, `tidyr`, `readr`,
`stringr`, `ggplot2`). Full run ≈ 5 minutes on 14 cores.

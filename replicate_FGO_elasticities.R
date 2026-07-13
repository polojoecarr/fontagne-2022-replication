###############################################################################
##                                                                           ##
##   REPLICATION OF THE HEADLINE RESULTS IN:                                 ##
##                                                                           ##
##   Fontagne, Guimbard & Orefice (2022)                                     ##
##   "Product-Level Trade Elasticities: Worth Weighting For"                 ##
##   Journal of International Economics / CEPII Working Paper 2019-17         ##
##                                                                           ##
##   What this script reproduces                                            ##
##   ---------------------------------------------------------------------- ##
##   * The product-level (HS 6-digit) trade-elasticity estimates obtained   ##
##     by estimating the structural-gravity equation (the paper's           ##
##     Equation 5) once for EACH of the ~5,050 HS6 product categories.      ##
##   * The headline distribution of trade elasticities (the paper's         ##
##     Figure 1) and its summary statistics (centred around -5).            ##
##   * The descriptive statistics of the trade elasticity by HS section     ##
##     (the paper's Table 4).                                               ##
##                                                                           ##
##   This is a faithful R/tidyverse/fixest translation of the authors'      ##
##   original Stata do-file `baseline_v4.do` (in the /Stata folder), which  ##
##   uses `ppml_panel_sg` (Larch et al. structural-gravity PPML) to run     ##
##   one regression per product.                                           ##
##                                                                           ##
##   Tooling                                                                ##
##   ---------------------------------------------------------------------- ##
##   * fixest::fepois()  -> the Poisson Pseudo-Maximum-Likelihood (PPML)    ##
##                          estimator the paper relies on (Santos-Silva &   ##
##                          Tenreyro 2006), with high-dimensional fixed     ##
##                          effects absorbed natively.                      ##
##   * data.table::fread -> fast reading of the 5,052 per-product CSVs.     ##
##   * furrr / future    -> runs the ~5,050 independent regressions in      ##
##                          PARALLEL across CPU cores (see SECTION 4).      ##
##   * tidyverse (dplyr/ggplot2/readr/stringr) -> post-processing, the      ##
##                          Figure 1 plot and the output tables.           ##
##                                                                           ##
###############################################################################


## ===========================================================================
## SECTION 1 -- ENVIRONMENT, PACKAGES AND FILE PATHS
## ===========================================================================
## The packages were installed into the user library. We make sure that
## library is on the search path before loading anything.
user_lib <- Sys.getenv("R_LIBS_USER")
if (nzchar(user_lib)) .libPaths(c(user_lib, .libPaths()))

suppressMessages({
  library(data.table)   # fast CSV reading (fread)
  library(fixest)       # PPML estimator with fixed effects (fepois)
  library(future)       # parallel back-end
  library(furrr)        # parallel purrr:: map (future_map) -- SEE SECTION 4
  library(dplyr)        # data wrangling (tidyverse)
  library(tidyr)        # data wrangling (tidyverse)
  library(readr)        # write_csv (tidyverse)
  library(stringr)      # string helpers (tidyverse)
  library(ggplot2)      # Figure 1 (tidyverse)
})

## ----- Folder layout --------------------------------------------------------
## `project_root` is the only path you may need to change if you move the data.
project_root <- "C:/Claude Code Project Folder/Fontange 2022"

## The per-product panels prepared by the authors. Each file
## `Sigma_HS6_<code>.csv` is ONE HS6 product and already contains, for every
## importer (j) x exporter (i) x year cell:
##   v            = bilateral import value, FOB (the dependent variable X_ijk,t)
##   ADV          = applied ad-valorem (equivalent) bilateral tariff  (tau_ijk,t)
##   DISTW        = population-weighted bilateral distance            (d_ij)
##   COLONY       = common-coloniser dummy            ) the gravity controls
##   CONTIG       = common-border dummy               ) collected in the
##   COMLANG_OFF  = common-official-language dummy     ) vector Z_ij of Eq. 5
## i.e. the BACI trade data, the MAcMap-HS6 tariffs and the CEPII gravity
## variables have ALREADY been merged together by the authors, one file per
## product. (That is why the raw BACI / Gravity folders are not needed here.)
data_dir <- file.path(project_root, "Replic_FGO", "Replic_FGO")

## Where we will write the deliverables.
out_dir <- file.path(project_root, "R_Replication", "output")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)


## ===========================================================================
## SECTION 2 -- THE LIST OF PRODUCTS TO ESTIMATE
## ===========================================================================
## One regression is run per HS6 product. We take the product list directly
## from the files that are present on disk.
all_files <- list.files(data_dir, pattern = "^Sigma_HS6_.*\\.csv$", full.names = FALSE)
hs6_codes <- str_match(all_files, "^Sigma_HS6_(.*)\\.csv$")[, 2]

## The paper (Section 2.2, footnote 30) disregards two HS6 positions because of
## missing trade information:
##   710820 = Monetary gold
##   711890 = Coins of legal tender
## Dropping them takes the universe from 5,052 to the paper's 5,050 products.
hs6_codes <- setdiff(hs6_codes, c("710820", "711890"))
cat("Number of HS6 products to estimate:", length(hs6_codes), "\n")


## ===========================================================================
## SECTION 3 -- THE PER-PRODUCT ESTIMATOR  (the paper's Equation 5)
## ===========================================================================
## For each HS6 product k the paper estimates, by PPML:
##
##   X_ijk,t = exp[ a_ik,t + a_jk,t
##                  + beta_k * ln(1 + tau_ijk,t)      <- TARIFF elasticity
##                  + gamma_k * ln(d_ij)              <- distance
##                  + delta_k * Z_ij ] * e_ijk,t      <- colony/contig/language
##
##   a_ik,t  = exporter-year fixed effect  (i^year)   } absorb the multilateral
##   a_jk,t  = importer-year fixed effect  (j^year)   } resistance terms
##
## The coefficient of interest is beta_k = the elasticity of trade VALUE to the
## (1+tariff) term. Under the CES interpretation beta_k = -sigma_k, and the
## structural TRADE elasticity is recovered as  epsilon_k = 1 + beta_k
## (paper, Section 2.2: "epsilon_k = 1 + beta_k").
##
## This function mirrors, line for line, the body of the Stata loop in
## `baseline_v4.do` (the PPML block).
estimate_product <- function(hs6) {

  f <- file.path(data_dir, paste0("Sigma_HS6_", hs6, ".csv"))

  ## --- read the product panel (semicolon-delimited) ----------------------
  dt <- tryCatch(fread(f, sep = ";"), error = function(e) NULL)
  if (is.null(dt) || nrow(dt) == 0)
    return(data.table(hs6 = hs6, tariff_coef = NA_real_, std_err = NA_real_,
                      dist_coef = NA_real_, nobs = 0L, status = "read_error"))

  ## --- cleaning, identical to the Stata do-file --------------------------
  ## Stata: "replace v=0 if v==." -- a missing trade cell means a zero flow,
  ## which PPML can use directly (it is robust to zeros: Santos-Silva &
  ## Tenreyro 2006).
  dt[is.na(v), v := 0]

  ## Stata: "g ln_tariff = ln(adv+1)"  and  "g l_distw = ln(distw)"
  dt[, ln_tariff := log(ADV + 1)]
  dt[, l_distw   := log(DISTW)]

  ## Stata: drop exporters i that NEVER export this product over the panel
  ## ("bys i: egen flag=total(v)" then "drop if flag==0"). Such exporters are
  ## perfectly predicted by the exporter-year fixed effect and carry no
  ## identifying information (paper, Section 2.5 and Appendix B).
  dt[, flag := sum(v), by = i]
  dt <- dt[flag != 0]

  if (nrow(dt) == 0 || length(unique(dt$ln_tariff)) < 2)
    return(data.table(hs6 = hs6, tariff_coef = NA_real_, std_err = NA_real_,
                      dist_coef = NA_real_, nobs = 0L, status = "no_variation"))

  ## --- PPML estimation of Equation 5 -------------------------------------
  ## `i^year + j^year` create the exporter-year and importer-year fixed
  ## effects (equivalent to Stata's `ppml_panel_sg ..., exporter(i)
  ## importer(j) year(year) nopair`). `vcov = "hetero"` = robust SE.
  m <- tryCatch(
    fepois(v ~ ln_tariff + l_distw + COLONY + CONTIG + COMLANG_OFF |
             i^year + j^year,
           data = dt, vcov = "hetero", warn = FALSE, notes = FALSE),
    error = function(e) NULL)

  ## The tariff term can be dropped by the estimator when it is collinear with
  ## the fixed effects (the paper's "missing" products). We flag these.
  if (is.null(m) || !("ln_tariff" %in% names(coef(m))))
    return(data.table(hs6 = hs6, tariff_coef = NA_real_, std_err = NA_real_,
                      dist_coef = NA_real_, nobs = nrow(dt), status = "dropped"))

  ct <- summary(m)$coeftable
  data.table(
    hs6         = hs6,
    tariff_coef = ct["ln_tariff", "Estimate"],     # beta_k  (= -sigma_k)
    std_err     = ct["ln_tariff", "Std. Error"],
    dist_coef   = if ("l_distw" %in% rownames(ct)) ct["l_distw", "Estimate"] else NA_real_,
    nobs        = m$nobs,
    status      = "ok")
}


## ===========================================================================
## SECTION 4 -- PARALLEL EXECUTION WITH furrr   *** furrr USED HERE ***
## ===========================================================================
## The ~5,050 product regressions are completely independent of one another,
## so they are embarrassingly parallel. This is the ONE place in the whole
## script where furrr is deployed.
##
##   * future::plan(multisession, workers = N) opens N background R sessions
##     (multisession is used because Windows cannot fork processes).
##   * furrr::future_map() is the drop-in parallel version of purrr::map():
##     it distributes the product codes across those worker sessions.
##   * furrr_options(packages=) makes sure each worker has data.table and
##     fixest attached; seed=TRUE makes the run reproducible.
##
## To run this SEQUENTIALLY instead (e.g. for debugging) you would simply
## replace the two furrr lines with:  results_list <- purrr::map(hs6_codes, estimate_product)
n_workers <- max(1, parallel::detectCores() - 2)   # leave 2 cores for the OS
cat("Launching", n_workers, "parallel workers...\n")
plan(multisession, workers = n_workers)

t_start <- Sys.time()
results_list <- future_map(
  hs6_codes,
  estimate_product,
  .options  = furrr_options(packages = c("data.table", "fixest"), seed = TRUE),
  .progress = TRUE
)
plan(sequential)                                   # shut the workers down
cat("All regressions finished in",
    round(as.numeric(Sys.time() - t_start, units = "mins"), 1), "minutes.\n")

## Stack the one-row-per-product results into a single table.
estimates <- rbindlist(results_list) |> as_tibble()


## ===========================================================================
## SECTION 5 -- POST-ESTIMATION: SIGN CONVENTION, SIGNIFICANCE, ELASTICITIES
## ===========================================================================
## This reproduces the second half of `baseline_v4.do`, where the raw tariff
## coefficients are turned into the published trade elasticities.
##
## Naming follows the paper / Stata:
##   tariff_coef          = beta_k, the raw coefficient on ln(1+tariff)
##                          (Stata calls this "sigma_ppml"; it is negative)
##   epsilon_pt           = 1 + beta_k = the POINT-ESTIMATE trade elasticity
##                          (Stata "epsilon_original", no significance screen)
##   t_stat               = |beta_k / se|
##   sig_1pct             = TRUE if significant at the 1% level (|t| > 2.576)
##   epsilon_zeroed       = trade elasticity AFTER setting the tariff
##                          coefficient to 0 for insignificant products
##                          (Stata "epsilon_ppml"): these become exactly 1.
z_crit_1pct <- 2.576    # two-sided normal critical value at the 1% level

estimates <- estimates |>
  mutate(
    epsilon_pt = 1 + tariff_coef,                       # point-estimate elasticity
    t_stat     = abs(tariff_coef / std_err),
    sig_1pct   = !is.na(t_stat) & t_stat > z_crit_1pct,

    ## flags used by the paper's online dataset (baseline_v4.do, lines 185-187)
    flag_missing  = is.na(tariff_coef),                 # tariff dropped by estimator
    flag_positive = !is.na(tariff_coef) & tariff_coef > 0 & sig_1pct,  # wrong sign & sig.
    flag_zero     = !is.na(tariff_coef) & !sig_1pct,    # insignificant -> set to zero

    ## Stata: "replace sigma_ppml = 0 if t_ppml < 2.576" then epsilon = 1+sigma
    tariff_coef_zeroed = if_else(sig_1pct, tariff_coef, 0),
    epsilon_zeroed     = 1 + tariff_coef_zeroed
  )

## --- Add the HS-section classification (for Table 4) -----------------------
## Numeric HS6 -> HS section (I..XXI). The cut-offs are taken verbatim from the
## authors' `descriptive_v4.do` (lines 11-31).
estimates <- estimates |>
  mutate(
    hs6_num = as.integer(hs6),
    hs_section = case_when(
      hs6_num <= 59999                       ~ 1L,
      hs6_num >= 60000  & hs6_num <= 149999  ~ 2L,
      hs6_num >= 150000 & hs6_num <= 159999  ~ 3L,
      hs6_num >= 160000 & hs6_num <= 249999  ~ 4L,
      hs6_num >= 250000 & hs6_num <= 279999  ~ 5L,
      hs6_num >= 280000 & hs6_num <= 389999  ~ 6L,
      hs6_num >= 390000 & hs6_num <= 409999  ~ 7L,
      hs6_num >= 410000 & hs6_num <= 439999  ~ 8L,
      hs6_num >= 440000 & hs6_num <= 469999  ~ 9L,
      hs6_num >= 470000 & hs6_num <= 499999  ~ 10L,
      hs6_num >= 500000 & hs6_num <= 639999  ~ 11L,
      hs6_num >= 640000 & hs6_num <= 679999  ~ 12L,
      hs6_num >= 680000 & hs6_num <= 709999  ~ 13L,
      hs6_num >= 710000 & hs6_num <= 719999  ~ 14L,
      hs6_num >= 720000 & hs6_num <= 839999  ~ 15L,
      hs6_num >= 840000 & hs6_num <= 859999  ~ 16L,
      hs6_num >= 860000 & hs6_num <= 899999  ~ 17L,
      hs6_num >= 900000 & hs6_num <= 929999  ~ 18L,
      hs6_num >= 930000 & hs6_num <= 939999  ~ 19L,
      hs6_num >= 940000 & hs6_num <= 969999  ~ 20L,
      hs6_num >= 970000 & hs6_num <= 989999  ~ 21L,
      TRUE ~ NA_integer_)
  )

hs_section_names <- c(
  "I Live Animals and Animal Products", "II Vegetable Products",
  "III Animal or vegetable fats and oils", "IV Prepared foodstuffs, beverages, tobacco",
  "V Mineral products", "VI Products of chemical industries",
  "VII Plastic and articles thereof", "VIII Raw hides, skins, leather",
  "IX Wood/Cork and articles thereof", "X Pulp of wood, cellulosic materials",
  "XI Textile and textile articles", "XII Footwear, headgear, umbrellas",
  "XIII Articles of stone, plaster, ceramic, glass", "XIV Pearls, precious stones, metals",
  "XV Base metals and articles thereof", "XVI Machinery and electrical equipment",
  "XVII Vehicles, aircraft, transport equipment", "XVIII Optical, precision, medical instruments",
  "XIX Arms and ammunitions", "XX Miscellaneous", "XXI Works of art")


## ===========================================================================
## SECTION 6 -- HEADLINE RESULTS
## ===========================================================================

## --- (a) The regression dataset (one row per HS6 product) ------------------
## This is the core deliverable: the estimated tariff coefficient, its
## standard error, and the recovered trade elasticity for every product.
regression_dataset <- estimates |>
  transmute(
    hs6, hs_section,
    tariff_coef, std_err, t_stat, sig_1pct,
    dist_coef,
    epsilon_pt,                 # 1 + beta_k (point estimate)
    epsilon_zeroed,             # 1 + beta_k, insignificant set to 1
    flag_zero, flag_positive, flag_missing,
    nobs, status
  ) |>
  arrange(hs6)

write_csv(regression_dataset, file.path(out_dir, "elasticity_estimates.csv"))

## --- (b) Headline summary statistics ---------------------------------------
## The paper reports the distribution of trade elasticities under two
## conventions (Section 3.1):
##   * "WITH zeros": insignificant tariff coefficients set to 0 (epsilon=1),
##     dropping products whose tariff coefficient is POSITIVE & significant.
##     Paper: average -5.5, median -4.
##   * "WITHOUT zeros": keep only the significant, NEGATIVE products.
##     Paper: average -9.8, median -7.3.
with_zeros <- estimates |>
  filter(!flag_missing, !flag_positive) |>     # drop missing & positive-significant
  pull(epsilon_zeroed)                          # insignificant already = 1

without_zeros <- estimates |>
  filter(sig_1pct, tariff_coef < 0) |>          # significant & correct (negative) sign
  pull(epsilon_pt)

share_sig <- estimates |>
  summarise(
    p10 = mean(abs(tariff_coef / std_err) > 1.645, na.rm = TRUE),
    p05 = mean(abs(tariff_coef / std_err) > 1.960, na.rm = TRUE),
    p01 = mean(abs(tariff_coef / std_err) > 2.576, na.rm = TRUE))

sink(file.path(out_dir, "headline_results.txt"))
cat("==============================================================\n")
cat(" REPLICATION OF FONTAGNE, GUIMBARD & OREFICE (2022)\n")
cat(" Headline product-level trade-elasticity results\n")
cat("==============================================================\n\n")
cat("Products attempted              :", nrow(estimates), "\n")
cat("Tariff coef. successfully estim.:", sum(estimates$status == "ok"), "\n")
cat("Dropped (collinear w/ FE)       :", sum(estimates$flag_missing), "\n\n")

cat("Share of tariff coefficients statistically significant\n")
cat(sprintf("  at 10%% level : %4.1f%%  (paper: 78%%)\n", 100 * share_sig$p10))
cat(sprintf("  at  5%% level : %4.1f%%  (paper: 72%%)\n", 100 * share_sig$p05))
cat(sprintf("  at  1%% level : %4.1f%%  (paper: 61%%)\n\n", 100 * share_sig$p01))

cat("Median t-statistic of tariff coef.:",
    round(median(estimates$t_stat, na.rm = TRUE), 2), " (paper: 3.2)\n\n")

cat("Mean distance elasticity (gamma_k):",
    round(mean(estimates$dist_coef, na.rm = TRUE), 3),
    " (paper: distributed around -1)\n\n")

cat("--- TRADE ELASTICITY epsilon_k = 1 + beta_k ------------------\n\n")
cat("(A) WITH zeros  (insignificant set to 1; positives & missing dropped)\n")
cat("    Paper: mean -5.5, median -4\n")
cat(sprintf("    Replication: mean %6.2f, median %6.2f, N = %d\n\n",
            mean(with_zeros), median(with_zeros), length(with_zeros)))
cat("(B) WITHOUT zeros  (significant & negative products only)\n")
cat("    Paper: mean -9.8, median -7.3\n")
cat(sprintf("    Replication: mean %6.2f, median %6.2f, N = %d\n\n",
            mean(without_zeros), median(without_zeros), length(without_zeros)))
sink()

## --- (c) Figure 1: empirical distribution of trade elasticities ------------
## The paper's Figure 1 plots the density of the trade elasticities for HS6
## products with epsilon_k < 0, cutting the left tail at -25 for readability
## (footnote 46). We reproduce that here from the significant, negative
## point estimates.
fig_data <- tibble(epsilon = without_zeros) |>
  filter(epsilon < 0, epsilon > -25)

p <- ggplot(fig_data, aes(x = epsilon)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 0.5,
                 fill = "grey80", colour = "white") +
  geom_density(colour = "firebrick", linewidth = 0.9) +
  geom_vline(xintercept = median(without_zeros), linetype = "dashed") +
  scale_x_continuous(breaks = seq(-25, 0, 5)) +
  labs(
    title = "Figure 1 (replication): empirical distribution of trade elasticities",
    subtitle = "HS6 products with epsilon_k < 0, PPML estimates, left tail cut at -25",
    x = expression("Trade elasticity  " * epsilon[k]),
    y = "Density") +
  theme_minimal(base_size = 12)

ggsave(file.path(out_dir, "Figure1_trade_elasticity_distribution.png"),
       p, width = 8, height = 5, dpi = 150)

## --- (d) Table 4: descriptive statistics by HS section ---------------------
## Reproduces the paper's Table 4. Following `descriptive_v4.do`, the
## statistics use the POINT estimates (epsilon_pt) and are computed only over
## products with a negative trade elasticity (tariff coef < -1, i.e. epsilon<0).
table4 <- estimates |>
  filter(tariff_coef < -1, !is.na(epsilon_pt)) |>
  group_by(hs_section) |>
  summarise(
    avg_epsilon = mean(epsilon_pt),
    sd_epsilon  = sd(epsilon_pt),
    min_epsilon = min(epsilon_pt),
    n_negative  = n(),
    .groups = "drop") |>
  left_join(
    estimates |> group_by(hs_section) |> summarise(n_total = n(), .groups = "drop"),
    by = "hs_section") |>
  filter(!is.na(hs_section)) |>
  mutate(section = hs_section_names[hs_section]) |>
  arrange(hs_section) |>
  transmute(section, avg_epsilon = round(avg_epsilon, 2),
            sd_epsilon = round(sd_epsilon, 2), min_epsilon = round(min_epsilon, 2),
            n_total, n_negative)

write_csv(table4, file.path(out_dir, "table4_by_HS_section.csv"))

cat("\nDONE. Outputs written to:\n  ", out_dir, "\n")
print(table4, n = 21)

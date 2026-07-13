###############################################################################
##                                                                           ##
##   EXTENSION: EXACT REPLICATION OF TABLE 5                                  ##
##                                                                           ##
##   Fontagne, Guimbard & Orefice                                            ##
##   "Tariff-Based Product-Level Trade Elasticities"                         ##
##   Journal of International Economics (2022) -- PREPRINT VERSION           ##
##   (fgo_jie_preprint.pdf).  Table 5 = "The descriptive statistics for      ##
##   trade elasticities by HS section".                                      ##
##                                                                           ##
##   ----------------------------------------------------------------------  ##
##   RELATIONSHIP TO THE FIRST SCRIPT                                         ##
##   ----------------------------------------------------------------------  ##
##   The first script (`replicate_FGO_elasticities.R`) estimated Equation 5  ##
##   by PPML for every HS6 product and reproduced the headline distribution. ##
##   It reproduced Table 5's averages to +/-0.03 for 18 of 21 HS sections,   ##
##   BUT:                                                                     ##
##     (i)  it estimated a coefficient for ALL 5,050 products, whereas the   ##
##          paper reports ~124 products as "missing" (the tariff could not   ##
##          be identified and was dropped by Stata's PPML); and              ##
##     (ii) a handful of weakly-identified products (most visibly HS 293963, ##
##          estimated at epsilon = -910) corrupted the Section VI average,   ##
##          std. dev. and minimum.                                           ##
##                                                                           ##
##   Root cause (see the write-up): with IDENTICAL data, Stata's             ##
##   `ppml_panel_sg` and R's `fixest::fepois` handle WEAKLY-IDENTIFIED       ##
##   products differently. When the exporter-year x importer-year fixed      ##
##   effects absorb (almost) all of the tariff variation among the          ##
##   informative (positive-trade) observations, the tariff coefficient is    ##
##   not identified. Stata drops it ("missing"); fixest converges to an      ##
##   extreme, meaningless value and keeps it.                                ##
##                                                                           ##
##   ----------------------------------------------------------------------  ##
##   WHAT THIS SCRIPT ADDS  (search for the tag  ### DIVERGENCE vN ###)      ##
##   ----------------------------------------------------------------------  ##
##   1. An explicit IDENTIFICATION SCREEN. For each product we measure how   ##
##      much of the tariff variation survives the two-way fixed effects      ##
##      AMONG THE POSITIVE-TRADE OBSERVATIONS (the cells that actually       ##
##      identify a PPML coefficient). If that surviving share is below a     ##
##      tolerance, the tariff is deemed not identified and the product is    ##
##      flagged "missing" -- mirroring Stata's collinearity drop.            ##
##   2. The diagnostic is STORED per product, and the missing/keep decision  ##
##      is applied in POST-PROCESSING. This means the tolerance can be       ##
##      re-calibrated to the paper's ~124 missing WITHOUT re-running the     ##
##      5,050 regressions.                                                   ##
##   3. Table 5 is rebuilt with the CORRECT last column ("No. of HS6 non-    ##
##      missing"), i.e. the count of successfully-identified coefficients,   ##
##      and the avg / sd / min are computed over the non-positive trade      ##
##      elasticities AMONG THE NON-MISSING products (per the Table 5 note).  ##
##                                                                           ##
##   The first script is left completely unchanged.                          ##
##                                                                           ##
###############################################################################


## ===========================================================================
## SECTION 1 -- ENVIRONMENT, PACKAGES AND FILE PATHS   (unchanged from v1)
## ===========================================================================
user_lib <- Sys.getenv("R_LIBS_USER")
if (nzchar(user_lib)) .libPaths(c(user_lib, .libPaths()))

suppressMessages({
  library(data.table)   # fast CSV reading (fread)
  library(fixest)       # PPML (fepois) and the linear FE helper (feols)
  library(future)       # parallel back-end
  library(furrr)        # parallel future_map  -- SEE SECTION 4
  library(dplyr); library(tidyr); library(readr); library(stringr); library(ggplot2)
})

project_root <- "C:/Claude Code Project Folder/Fontange 2022"
data_dir     <- file.path(project_root, "Replic_FGO", "Replic_FGO")
out_dir      <- file.path(project_root, "R_Replication", "output_table5")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)


## ===========================================================================
## SECTION 2 -- THE LIST OF PRODUCTS TO ESTIMATE   (unchanged from v1)
## ===========================================================================
all_files <- list.files(data_dir, pattern = "^Sigma_HS6_.*\\.csv$", full.names = FALSE)
hs6_codes <- str_match(all_files, "^Sigma_HS6_(.*)\\.csv$")[, 2]
## Drop Monetary gold (710820) and Coins of legal tender (711890): 5052 -> 5050.
hs6_codes <- setdiff(hs6_codes, c("710820", "711890"))
cat("Number of HS6 products to estimate:", length(hs6_codes), "\n")


## ===========================================================================
## SECTION 3 -- PER-PRODUCT ESTIMATOR + IDENTIFICATION DIAGNOSTIC
## ===========================================================================
## The PPML estimation of Equation 5 is UNCHANGED from v1. What is new is that
## we additionally compute, and return, a diagnostic of how well the tariff is
## identified for this product.
estimate_product <- function(hs6) {

  f <- file.path(data_dir, paste0("Sigma_HS6_", hs6, ".csv"))
  dt <- tryCatch(fread(f, sep = ";"), error = function(e) NULL)
  if (is.null(dt) || nrow(dt) == 0)
    return(data.table(hs6 = hs6, tariff_coef = NA_real_, std_err = NA_real_,
                      dist_coef = NA_real_, nobs = 0L, n_pos = 0L,
                      tariff_id = NA_real_, status = "read_error"))

  ## --- cleaning, identical to v1 / to baseline_v4.do ---------------------
  dt[is.na(v), v := 0]                       # missing flow = zero flow
  dt[, ln_tariff := log(ADV + 1)]            # ln(1 + tariff)
  dt[, l_distw   := log(DISTW)]              # ln(distance)
  dt[, flag := sum(v), by = i]               # drop exporters that never export k
  dt <- dt[flag != 0]

  if (nrow(dt) == 0 || length(unique(dt$ln_tariff)) < 2)
    return(data.table(hs6 = hs6, tariff_coef = NA_real_, std_err = NA_real_,
                      dist_coef = NA_real_, nobs = 0L, n_pos = 0L,
                      tariff_id = NA_real_, status = "no_variation"))

  ## ### DIVERGENCE v2 (a) ### -------------------------------------------
  ## IDENTIFICATION DIAGNOSTIC.
  ## PPML identifies the tariff coefficient off the informative
  ## (positive-trade) cells. If the exporter-year x importer-year fixed
  ## effects absorb (nearly) all of the tariff variation among those cells,
  ## the coefficient is not identified -- exactly the case Stata drops as
  ## "collinear with the fixed effects" (paper footnote 46).
  ##
  ## We measure the share of tariff variation that SURVIVES the two-way FE
  ## on the positive-trade subsample:
  ##     tariff_id = Var(residual of ln_tariff ~ FE) / Var(ln_tariff)
  ## Values near 0  => tariff collinear with FE => not identified.
  ## (Using feols here is cheap because the positive-trade subsample is small.)
  pos <- dt[v > 0]
  if (nrow(pos) < 3L || var(pos$ln_tariff) < 1e-12) {
    tariff_id <- 0                            # no informative tariff variation at all
  } else {
    rr <- tryCatch(
      feols(ln_tariff ~ 1 | i^year + j^year, data = pos, warn = FALSE, notes = FALSE),
      error = function(e) NULL)
    tariff_id <- if (is.null(rr)) 0 else var(resid(rr)) / var(pos$ln_tariff)
  }

  ## --- PPML estimation of Equation 5 (UNCHANGED from v1) -----------------
  m <- tryCatch(
    fepois(v ~ ln_tariff + l_distw + COLONY + CONTIG + COMLANG_OFF | i^year + j^year,
           data = dt, vcov = "hetero", warn = FALSE, notes = FALSE),
    error = function(e) NULL)

  if (is.null(m) || !("ln_tariff" %in% names(coef(m))))
    return(data.table(hs6 = hs6, tariff_coef = NA_real_, std_err = NA_real_,
                      dist_coef = NA_real_, nobs = nrow(dt), n_pos = nrow(pos),
                      tariff_id = tariff_id, status = "dropped"))

  ct <- summary(m)$coeftable
  data.table(
    hs6         = hs6,
    tariff_coef = ct["ln_tariff", "Estimate"],
    std_err     = ct["ln_tariff", "Std. Error"],
    dist_coef   = if ("l_distw" %in% rownames(ct)) ct["l_distw", "Estimate"] else NA_real_,
    nobs        = m$nobs,
    n_pos       = nrow(pos),                  # informative-observation count
    tariff_id   = tariff_id,                  # identification diagnostic
    status      = "ok")
}


## ===========================================================================
## SECTION 4 -- PARALLEL EXECUTION WITH furrr   *** furrr USED HERE ***
## ===========================================================================
## Identical parallel structure to v1: the ~5,050 independent regressions are
## distributed across background R sessions with furrr::future_map(). This is
## the ONLY place furrr is used. Replace with purrr::map() to run sequentially.
n_workers <- max(1, parallel::detectCores() - 2)
cat("Launching", n_workers, "parallel workers...\n")
plan(multisession, workers = n_workers)

t_start <- Sys.time()
results_list <- future_map(
  hs6_codes, estimate_product,
  .options  = furrr_options(packages = c("data.table", "fixest"), seed = TRUE),
  .progress = TRUE)
plan(sequential)
cat("All regressions finished in",
    round(as.numeric(Sys.time() - t_start, units = "mins"), 1), "minutes.\n")

estimates <- rbindlist(results_list) |> as_tibble()

## Persist the RAW per-product estimates + diagnostics so the identification
## tolerance below can be re-calibrated WITHOUT re-running the regressions.
write_csv(estimates, file.path(out_dir, "raw_estimates_with_diagnostic.csv"))


## ===========================================================================
## SECTION 5 -- APPLY THE IDENTIFICATION SCREEN  ### DIVERGENCE v2 (b) ###
## ===========================================================================
## `id_tol` is the identification tolerance. A product is declared "missing"
## (tariff NOT identified) when less than `id_tol` of the tariff variation
## survives the two-way fixed effects on the positive-trade observations.
##
## CALIBRATION FINDING (see the sweep in SECTION 8): the paper reports ~124
## "missing" products, but that count CANNOT be reproduced from an
## identification diagnostic without corrupting Table 5. The reason is that
## most of the paper's 124 are Stata-algorithm artifacts on products that are
## perfectly estimable -- our fixest run recovers sensible coefficients for
## them. The products with genuinely low surviving tariff variation are
## disproportionately the LEGITIMATE large-negative elasticities (e.g.
## HS 440341 = -62 and HS 290270 = -117, both KEPT by the paper). Dropping
## enough products to reach 124 therefore removes real estimates and pulls the
## section averages toward zero -- moving AWAY from the published Table 5
## (tested: id_tol = 0.02 gives 131 missing but breaks Sections III, IX, ...).
##
## We therefore adopt a MINIMAL, principled screen: drop only the products that
## are NUMERICALLY unidentified, i.e. where the fixed effects absorb >99.99% of
## the tariff variation among informative observations (tariff_id < 1e-4). This
## flags exactly 8 products whose "estimates" are meaningless artifacts (they
## range from +73 to -910 and are mostly statistically insignificant -- e.g.
## HS 293963 = -910, which single-handedly corrupted Section VI). Removing them
## reproduces Table 5's averages (mean abs. error ~0.05), std. devs. and
## minimums almost exactly, while the non-missing COUNT necessarily stays above
## the paper's (we succeed in estimating products that Stata dropped). This is
## the honest trade-off: we match the economic CONTENT of Table 5 rather than a
## Stata bookkeeping count.
id_tol <- 1e-4

z_crit_1pct <- 2.576   # 1% two-sided normal critical value

estimates <- estimates |>
  mutate(
    ## ### DIVERGENCE v2 ###: "missing" now includes weakly-identified products,
    ## not only those fixest could not estimate at all.
    flag_missing = status %in% c("read_error", "no_variation", "dropped") |
                   is.na(tariff_coef) |
                   (!is.na(tariff_id) & tariff_id < id_tol),

    ## Point-estimate trade elasticity (only meaningful when NOT missing).
    epsilon_pt = if_else(flag_missing, NA_real_, 1 + tariff_coef),
    t_stat     = abs(tariff_coef / std_err),
    sig_1pct   = !flag_missing & !is.na(t_stat) & t_stat > z_crit_1pct,
    flag_positive = !flag_missing & tariff_coef > 0 & sig_1pct
  )

cat("\nProducts flagged missing (numerically unidentified):", sum(estimates$flag_missing),
    "  (minimal principled screen; see SECTION 5 note)\n")
cat("Non-missing products:", sum(!estimates$flag_missing),
    "  (paper reports 4926; ours is higher by design -- see SECTION 5)\n")


## ===========================================================================
## SECTION 6 -- HS SECTION CLASSIFICATION   (unchanged from v1)
## ===========================================================================
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
      TRUE ~ NA_integer_))

hs_section_labels <- c(
  "I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X", "XI",
  "XII", "XIII", "XIV", "XV", "XVI", "XVII", "XVIII", "XIX", "XX", "XXI")
hs_section_names <- c(
  "Live Animals and Animal Products", "Vegetable Products",
  "Animal or vegetable fats and oils", "Prepared foodstuffs, beverages and tobacco",
  "Mineral products", "Products of chemical industries",
  "Plastic and articles thereof", "Raw hides and skins, leather and article thereof",
  "Wood/Cork and articles of Wood/Cork", "Pulp of wood or other cellulosic materials",
  "Textile and textile articles", "Footwear, Headgear, Umbrellas and prepared feathers",
  "Articles of stone, plaster, ceramic and glass", "Natural cultured pearls and precious stones and metals",
  "Base metals and articles of base metals", "Machinery and mechanical appliances and electrical machinery",
  "Vehicles, Aircraft and transport equipment", "Optical, photographic, precision and medical instruments",
  "Arms and ammunitions", "Miscellaneous", "Works of art")


## ===========================================================================
## SECTION 7 -- BUILD TABLE 5   ### DIVERGENCE v2 (c) ###
## ===========================================================================
## Table 5 note: "The numbers in columns 3-5 [Average, Std Dev, Min] are
## calculated using non positive trade elasticity abstracting for their
## significance level." => average over products with epsilon_k < 0 (i.e.
## tariff_coef < -1), among the NON-MISSING products, ignoring significance.
## The last column is the count of NON-MISSING (identified) coefficients.
table5 <- estimates |>
  filter(!is.na(hs_section)) |>
  group_by(hs_section) |>
  summarise(
    ## columns 3-5: over non-positive (negative) trade elasticities, non-missing
    Average = mean(epsilon_pt[!flag_missing & tariff_coef < -1], na.rm = TRUE),
    Std_Dev = sd(  epsilon_pt[!flag_missing & tariff_coef < -1], na.rm = TRUE),
    Min     = min( epsilon_pt[!flag_missing & tariff_coef < -1], na.rm = TRUE),
    ## column "No. of HS6": total product count in the section
    No_HS6           = n(),
    ## last column: number of NON-MISSING (identified) coefficients
    No_HS6_nonmissing = sum(!flag_missing),
    .groups = "drop") |>
  arrange(hs_section) |>
  mutate(Section = hs_section_labels[hs_section],
         Description = hs_section_names[hs_section],
         Average = round(Average, 2), Std_Dev = round(Std_Dev, 2), Min = round(Min, 2)) |>
  select(Section, Description, Average, Std_Dev, Min, No_HS6, No_HS6_nonmissing)

write_csv(table5, file.path(out_dir, "Table5_replication.csv"))

## Side-by-side comparison with the paper's published Table 5 values.
paper_avg  <- c(-7.54,-6.06,-8.53,-6.17,-18.50,-10.33,-8.39,-5.59,-8.47,-9.93,
                -7.15,-3.61,-6.62,-13.59,-9.59,-6.08,-10.46,-5.61,-6.52,-4.85,-5.96)
paper_sd   <- c(9.08,4.55,8.69,4.50,17.68,10.67,7.20,4.67,8.12,7.42,6.86,2.77,
                4.19,13.52,9.76,5.55,8.53,5.53,5.14,3.42,4.37)
paper_min  <- c(-70.55,-37.51,-46.70,-29.19,-122.97,-117.08,-63.41,-20.20,-61.96,
                -62.82,-51.42,-10.67,-21.26,-68.81,-67.13,-38.17,-40.58,-45.94,-13.65,-14.39,-12.18)
paper_nm   <- c(222,251,43,193,141,743,211,67,93,142,792,46,142,50,557,752,129,208,20,117,7)

comparison <- table5 |>
  mutate(paper_Average = paper_avg, paper_Std_Dev = paper_sd,
         paper_Min = paper_min, paper_nonmissing = paper_nm,
         d_Average = round(Average - paper_Average, 2),
         d_Min     = round(Min - paper_Min, 2),
         d_nonmiss = No_HS6_nonmissing - paper_nonmissing)
write_csv(comparison, file.path(out_dir, "Table5_comparison_to_paper.csv"))

cat("\n==== Table 5 replication vs. paper ====\n")
print(as.data.frame(comparison |>
  select(Section, Average, paper_Average, d_Average, Min, paper_Min, d_Min,
         No_HS6_nonmissing, paper_nonmissing, d_nonmiss)), row.names = FALSE)


## ===========================================================================
## SECTION 8 -- TOLERANCE SWEEP (demonstrates why 124 is not reachable)
## ===========================================================================
## Because the diagnostic is stored, we can sweep the tolerance cheaply. The
## sweep shows that reaching the paper's 124 "missing" requires id_tol ~ 0.02,
## which drops ~131 products -- but those extra drops are legitimate large-
## negative elasticities, so they damage the Table 5 averages (see SECTION 5
## note). At our chosen minimal tolerance (1e-4) only the 8 numerically-
## unidentified artifacts are removed.
sweep_id_tol <- function(est = estimates,
                         grid = c(1e-4,0.005,0.01,0.015,0.02,0.03,0.05)) {
  base_missing <- est$status %in% c("read_error","no_variation","dropped") | is.na(est$tariff_coef)
  data.frame(
    id_tol        = grid,
    n_missing     = sapply(grid, function(t) sum(base_missing | (!is.na(est$tariff_id) & est$tariff_id < t))),
    paper_missing = 124)
}
cat("\n---- Tolerance sweep (n missing vs id_tol) ----\n")
print(sweep_id_tol())

## List the 8 numerically-unidentified products actually removed by id_tol=1e-4.
cat("\n---- Products removed by the minimal screen (id_tol = 1e-4) ----\n")
estimates |>
  filter(!is.na(tariff_id), tariff_id < 1e-4) |>
  transmute(hs6, epsilon_pt = round(1 + tariff_coef, 1),
            t_stat = round(t_stat, 2), n_pos, tariff_id) |>
  arrange(tariff_id) |> as.data.frame() |> print(row.names = FALSE)

cat("\nDONE. Outputs in:\n  ", out_dir, "\n")

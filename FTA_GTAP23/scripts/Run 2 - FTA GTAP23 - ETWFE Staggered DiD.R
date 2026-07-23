## ============================================================================
## RUN 2 -- AVERAGE FTA IMPACT BY GTAP-23 SECTOR  (ETWFE / staggered DiD)
## ============================================================================
##
## WHAT THIS DOES (and how it differs from Run 1)
##   Same Fontagne pooling and fixed-effect structure as Run 1, but the single
##   FTA dummy is replaced by the heterogeneity-robust ESTIMATOR from
##   Nagengast & Yotov (2023) -- the Extended Two-Way Fixed Effects (ETWFE) /
##   staggered difference-in-differences estimator of Wooldridge (2021, 2023).
##
##   Instead of one average FTA effect, we estimate a full set of
##   cohort-by-year treatment effects (delta_gs), where a "cohort" g is the
##   wave in which a country pair first gets an FTA, and s indexes the years at
##   or after treatment. We then aggregate those delta_gs back into a SINGLE
##   average FTA effect per sector using the paper's weights (Equation 7),
##   together with the correct (delta-method) standard error.
##
##   Why bother: the plain TWFE dummy in Run 1 can be biased by "forbidden
##   comparisons" (using already-treated pairs as controls for later-treated
##   pairs). Saturating the treated observations with cohort-year terms removes
##   those bad comparisons.
##
## ESTIMATING EQUATION (per GTAP-23 sector, pooling its HS6 products):
##   v_ijk,t = exp[ SUM_{g} SUM_{s>=g} delta_gs * D_gs,ij,t     <- cohort-year effects
##                + rho * ln(1+tariff) + gravity controls
##                + exporter x year x HS6 FE + importer x year x HS6 FE ] * error
##   Aggregate to one number:  delta_bar = SUM (N_gs / N_D) * delta_gs.
## ============================================================================


## ============================================================================
## SECTION 1 -- ENVIRONMENT, PACKAGES AND FILE PATHS   (same as Run 1)
## ============================================================================
user_lib <- Sys.getenv("R_LIBS_USER"); if (nzchar(user_lib)) .libPaths(c(user_lib, .libPaths()))
library(data.table); library(fixest); library(readxl)
library(dplyr); library(tidyr); library(readr); library(stringr)
setFixest_nthreads(parallel::detectCores())

project_root <- "C:/Claude Code Project Folder/Fontange 2022"
sigma_dir    <- file.path(project_root, "Replic_FGO", "Replic_FGO")
etwfe_dir    <- file.path(project_root, "R_Replication", "ETWFE")
out_dir      <- file.path(etwfe_dir, "FTA_GTAP23_Outputs", "run2_etwfe")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
waves <- c(2001, 2004, 2007, 2010, 2013, 2016)


## ============================================================================
## SECTION 2 -- INPUTS: CONCORDANCE, FTA DUMMY, AND (NEW) TREATMENT COHORTS
## ============================================================================

## ---- 2a. HS6 -> GTAP23 (same as Run 1) ------------------------------------
hs6_to_g65 <- read_csv(file.path(etwfe_dir, "GTAP to HS6 (1).csv"), show_col_types = FALSE) |>
  transmute(hs6 = str_pad(as.character(Code), 6, pad = "0"),
            gtap65 = tolower(trimws(GSEC3_rev_lower_case)))
g65_to_g23 <- read_csv(file.path(etwfe_dir, "GTAP 65 to GTAP 23 Concordence (1).csv"), show_col_types = FALSE) |>
  transmute(gtap65 = tolower(trimws(`Full Disaggregation`)), gtap23 = tolower(trimws(GTAP23))) |>
  filter(!is.na(gtap65))
hs6_gtap23 <- hs6_to_g65 |> left_join(g65_to_g23, by = "gtap65") |>
  filter(!is.na(gtap23)) |> distinct(hs6, gtap23)

gtap_sectors <- hs6_gtap23 |> count(gtap23, name = "n") |> arrange(n) |> pull(gtap23)
gtap_groups  <- split(hs6_gtap23$hs6, hs6_gtap23$gtap23)

## ---- 2b. FTA data + TREATMENT COHORTS -------------------------------------
fta_types <- c("FTA", "FTA & EIA", "CU", "CU & EIA")   # EDIT to change the FTA definition
bilateral <- read_excel(file.path(etwfe_dir, "DTA 2.0 - Vertical Content (v2).xlsx"),
                        sheet = "Bilateral Information") |>
  filter(type %in% fta_types) |>
  transmute(i = toupper(trimws(iso1)), j = toupper(trimws(iso2)), year = as.integer(year))
fta_active <- bind_rows(bilateral, rename(bilateral, i = j, j = i)) |>
  distinct(i, j, year) |> filter(year %in% waves)

## COHORT = the first wave in which a pair has an active FTA.
## Pairs never observed with an FTA get no cohort here (they are the
## never-treated control group, handled in Section 3).
fta_cohort <- fta_active |> group_by(i, j) |>
  summarise(cohort = min(year), .groups = "drop") |> as.data.table()


## ============================================================================
## SECTION 3 -- THE PER-SECTOR ETWFE ESTIMATOR
## ============================================================================
estimate_sector <- function(sector) {

  ## --- read & pool the sector's HS6 files (same as Run 1) ------------------
  hs6_list <- gtap_groups[[sector]]
  read_one <- function(h) {
    f <- file.path(sigma_dir, paste0("Sigma_HS6_", h, ".csv"))
    d <- tryCatch(fread(f, sep = ";"), error = function(e) NULL)
    if (is.null(d) || nrow(d) == 0L) return(NULL)
    d[, hs6 := h]; d
  }
  dt <- rbindlist(lapply(hs6_list, read_one), use.names = TRUE, fill = TRUE)
  if (is.null(dt) || nrow(dt) == 0L)
    return(data.table(gtap23 = sector, FTA_coef = NA_real_, FTA_SE = NA_real_,
                      tariff_coef = NA_real_, n_hs6 = 0L, nobs = 0L, n_cells = 0L, status = "read_error"))

  ## --- cleaning (same as Run 1) -------------------------------------------
  dt[is.na(v), v := 0]
  dt[, ln_tariff := log(ADV + 1)]
  dt[, l_distw   := log(DISTW)]
  dt[, flag := sum(v), by = .(i, hs6)]; dt <- dt[flag != 0]

  ## ========================================================================
  ## *** COHORT-SPECIFIC PRE-PERIOD BASELINE -- THE HEART OF THE ETWFE ***
  ## ========================================================================
  ## We attach each pair's treatment cohort, then build ONE categorical
  ## variable, `treat_cohort_year`, that encodes the staggered design:
  ##
  ##   * A treated pair in a POST-treatment wave (year >= its cohort) gets its
  ##     own label "g<cohort>_y<year>"  -> one treatment coefficient per cell.
  ##   * EVERYTHING ELSE is put in the single reference level "0_baseline":
  ##         - never-treated pairs, and
  ##         - treated pairs in their OWN PRE-treatment waves (year < cohort).
  ##
  ## Because a treated cohort's pre-treatment observations sit in the baseline
  ## (rather than being compared against other, already-treated cohorts), each
  ## cohort is effectively differenced against ITS OWN pre-period -- i.e. a
  ## cohort-specific pre-period baseline. This is what avoids the "forbidden
  ## comparisons" of the plain TWFE dummy in Run 1.
  ##
  ## We also DROP "always-treated" pairs (first treated in the very first wave,
  ## 2001): they have no observable pre-period, so there is no baseline for
  ## them.
  ## ------------------------------------------------------------------------
  dt <- merge(dt, fta_cohort, by = c("i", "j"), all.x = TRUE)   # cohort = NA means never-treated
  dt <- dt[is.na(cohort) | cohort != waves[1]]                  # drop always-treated (cohort == 2001)

  dt[, treat_cohort_year := "0_baseline"]                                   # default = reference
  dt[!is.na(cohort) & year >= cohort,                                       # treated & post-treatment
     treat_cohort_year := paste0("g", cohort, "_y", year)]                  #   -> own cohort-year cell
  dt[, treat_cohort_year := relevel(factor(treat_cohort_year), ref = "0_baseline")]
  ## ========================================================================

  ## need at least one treated cohort-year cell to estimate anything
  if (nrow(dt) == 0L || nlevels(dt$treat_cohort_year) < 2)
    return(data.table(gtap23 = sector, FTA_coef = NA_real_, FTA_SE = NA_real_,
                      tariff_coef = NA_real_, n_hs6 = uniqueN(dt$hs6), nobs = nrow(dt), n_cells = 0L,
                      status = "no_variation"))

  ## --- the saturated ETWFE regression -------------------------------------
  ## i(treat_cohort_year, ref="0_baseline") expands into one dummy per
  ## cohort-year cell. Everything else (FEs, tariff, gravity controls) is
  ## exactly as in Run 1, so the two runs differ ONLY in the treatment term.
  m <- tryCatch(
    fepois(v ~ i(treat_cohort_year, ref = "0_baseline") +
             ln_tariff + l_distw + COLONY + CONTIG + COMLANG_OFF |
             i^year^hs6 + j^year^hs6,
           data = dt, cluster = ~ i^j, warn = FALSE, notes = FALSE),
    error = function(e) NULL)

  if (is.null(m))
    return(data.table(gtap23 = sector, FTA_coef = NA_real_, FTA_SE = NA_real_,
                      tariff_coef = NA_real_, n_hs6 = uniqueN(dt$hs6), nobs = nrow(dt), n_cells = 0L,
                      status = "dropped"))

  ## --- AGGREGATE the cohort-year effects into one FTA effect (Equation 7) --
  ## Weight each cohort-year effect by N_gs / N_D, i.e. that cell's share of
  ## all treated observations. The standard error of the weighted sum is the
  ## delta-method combination  sqrt( w' V w )  using the treatment-block of the
  ## variance-covariance matrix.
  cf <- coef(m); V <- vcov(m)
  cells <- grep("^treat_cohort_year::", names(cf), value = TRUE)
  cells <- cells[!grepl("0_baseline", cells)]
  cells <- cells[!is.na(cf[cells])]                         # drop any collinear-dropped cell
  if (length(cells) == 0L)
    return(data.table(gtap23 = sector, FTA_coef = NA_real_, FTA_SE = NA_real_,
                      tariff_coef = if ("ln_tariff" %in% names(cf)) cf["ln_tariff"] else NA_real_,
                      n_hs6 = uniqueN(dt$hs6), nobs = m$nobs, n_cells = 0L, status = "no_cells"))

  levs <- sub("^treat_cohort_year::", "", cells)
  N_gs <- vapply(levs, function(L) sum(dt$treat_cohort_year == L), numeric(1))  # treated obs per cell
  w    <- N_gs / sum(N_gs)                                                       # weights (sum to 1)
  delta_bar <- sum(w * cf[cells])                                                # aggregated FTA effect
  se_bar    <- sqrt(as.numeric(t(w) %*% V[cells, cells] %*% w))                  # delta-method SE

  data.table(
    gtap23      = sector,
    FTA_coef    = delta_bar,                 # <- aggregated (ETWFE) average FTA effect
    FTA_SE      = se_bar,
    tariff_coef = if ("ln_tariff" %in% names(cf)) unname(cf["ln_tariff"]) else NA_real_,
    n_hs6       = uniqueN(dt$hs6),
    nobs        = m$nobs,
    n_cells     = length(cells),             # how many cohort-year effects were aggregated
    status      = "ok")
}


## ============================================================================
## SECTION 4 -- RUN ALL SECTORS SEQUENTIALLY   (same pattern as Run 1)
## ============================================================================
t_start <- Sys.time()
for (sec in gtap_sectors) {
  target <- file.path(out_dir, paste0("run_", sec, ".rds"))
  if (file.exists(target)) next
  out <- estimate_sector(sec)
  saveRDS(out, target)
  cat(format(Sys.time(), "%H:%M"), "- finished", sec, "->", out$status,
      "| ETWFE FTA effect:", round(out$FTA_coef, 3), "\n"); flush.console()
  gc()
}
cat("\nAll sectors done in",
    round(as.numeric(Sys.time() - t_start, units = "mins"), 1), "minutes.\n")

results <- rbindlist(lapply(list.files(out_dir, "^run_.*\\.rds$", full.names = TRUE), readRDS))


## ============================================================================
## SECTION 5 -- RESULTS TABLE + SIGNIFICANCE SUMMARY   (same logic as Run 1)
## ============================================================================
z_1 <- 2.576; z_5 <- 1.960; z_10 <- 1.645

results_table <- results |> as_tibble() |>
  mutate(
    t_stat        = abs(FTA_coef / FTA_SE),
    pct_trade_chg = exp(FTA_coef) - 1,
    positive      = FTA_coef > 0,
    sig_1pct      = !is.na(t_stat) & t_stat > z_1,
    sig_5pct      = !is.na(t_stat) & t_stat > z_5,
    sig_10pct     = !is.na(t_stat) & t_stat > z_10,
    pos_sig_1pct  = positive & sig_1pct,
    pos_sig_5pct  = positive & sig_5pct,
    pos_sig_10pct = positive & sig_10pct) |>
  arrange(gtap23) |>
  select(gtap23, n_hs6, n_cells, FTA_coef, FTA_SE, t_stat, pct_trade_chg,
         positive, sig_1pct, sig_5pct, sig_10pct,
         pos_sig_1pct, pos_sig_5pct, pos_sig_10pct, nobs, status)

write_csv(results_table, file.path(out_dir, "results_run2_etwfe.csv"))

summary_run2 <- tibble(
  run = "Run 2: ETWFE staggered DiD",
  sectors_estimated = sum(results_table$status == "ok"),
  pct_positive      = round(100 * mean(results_table$positive, na.rm = TRUE), 1),
  pct_pos_sig_1pct  = round(100 * mean(results_table$pos_sig_1pct, na.rm = TRUE), 1),
  pct_pos_sig_5pct  = round(100 * mean(results_table$pos_sig_5pct, na.rm = TRUE), 1),
  pct_pos_sig_10pct = round(100 * mean(results_table$pos_sig_10pct, na.rm = TRUE), 1))
write_csv(summary_run2, file.path(out_dir, "summary_run2_etwfe.csv"))

cat("\n================= RUN 2 (ETWFE) RESULTS =================\n")
print(as.data.frame(results_table |>
        select(gtap23, FTA_coef, FTA_SE, t_stat, sig_1pct, sig_5pct, sig_10pct)), row.names = FALSE)
cat("\nSectors with a POSITIVE & SIGNIFICANT FTA effect:\n")
cat(sprintf("  at 1%%  : %.1f%%\n  at 5%%  : %.1f%%\n  at 10%% : %.1f%%\n",
            summary_run2$pct_pos_sig_1pct, summary_run2$pct_pos_sig_5pct, summary_run2$pct_pos_sig_10pct))

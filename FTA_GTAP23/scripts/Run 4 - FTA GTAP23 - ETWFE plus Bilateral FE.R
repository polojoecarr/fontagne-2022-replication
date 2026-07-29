## ============================================================================
## RUN 4 -- AVERAGE FTA IMPACT BY GTAP-23 SECTOR  (ETWFE  +  bilateral FE)
## ============================================================================
##
## WHAT THIS DOES
##   This run COMBINES the two departures from the baseline:
##     * the treatment is the heterogeneity-robust ETWFE / staggered DiD
##       estimator (as in Run 2), AND
##     * the four gravity controls are replaced by a bilateral (country-pair)
##       fixed effect i^j (as in Run 3).
##
##   It is therefore the specification closest to Nagengast & Yotov (2023),
##   whose main equation carries exporter-time, importer-time AND pair fixed
##   effects together with the cohort-year ETWFE treatment terms. Here the
##   fixed effects are additionally kept at the HS6 level, in the Fontagne
##   style used throughout this project.
##
## ESTIMATING EQUATION (per GTAP-23 sector, pooling its HS6 products):
##   v_ijk,t = exp[ SUM_g SUM_{s>=g} delta_gs * D_gs,ij,t     <- cohort-year effects (Run 2)
##                + rho * ln(1+tariff_ijk,t)
##                + exporter x year x HS6 FE
##                + importer x year x HS6 FE
##                + PAIR FE (i^j) ] * error                    <- replaces gravity controls (Run 3)
##   Aggregate to one number:  delta_bar = SUM (N_gs / N_D) * delta_gs.
##
## Only TWO lines differ from Run 2, both flagged with  ### CHANGE vs RUN 2 ###:
##   (1) the fixed-effect block gains "+ i^j" and loses the gravity controls;
##   (2) nothing else -- the cohort-specific baseline and the aggregation are
##       identical to Run 2.
## ============================================================================


## ============================================================================
## SECTION 1 -- ENVIRONMENT, PACKAGES AND FILE PATHS   (same as Run 2/3)
## ============================================================================
user_lib <- Sys.getenv("R_LIBS_USER"); if (nzchar(user_lib)) .libPaths(c(user_lib, .libPaths()))
library(data.table); library(fixest); library(readxl)
library(dplyr); library(tidyr); library(readr); library(stringr)
setFixest_nthreads(parallel::detectCores())

project_root <- "C:/Claude Code Project Folder/Fontange 2022"
sigma_dir    <- file.path(project_root, "Replic_FGO", "Replic_FGO")
etwfe_dir    <- file.path(project_root, "R_Replication", "ETWFE")
out_dir      <- file.path(etwfe_dir, "FTA_GTAP23_Outputs", "run4_etwfe_pairfe")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
waves <- c(2001, 2004, 2007, 2010, 2013, 2016)


## ============================================================================
## SECTION 2 -- INPUTS: CONCORDANCE, FTA DUMMY, TREATMENT COHORTS  (same as Run 2)
## ============================================================================
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

fta_types <- c("FTA", "FTA & EIA", "CU", "CU & EIA")   # EDIT to change the FTA definition
bilateral <- read_excel(file.path(etwfe_dir, "DTA 2.0 - Vertical Content (v2).xlsx"),
                        sheet = "Bilateral Information") |>
  filter(type %in% fta_types) |>
  transmute(i = toupper(trimws(iso1)), j = toupper(trimws(iso2)), year = as.integer(year))
fta_active <- bind_rows(bilateral, rename(bilateral, i = j, j = i)) |>
  distinct(i, j, year) |> filter(year %in% waves)
fta_cohort <- fta_active |> group_by(i, j) |>
  summarise(cohort = min(year), .groups = "drop") |> as.data.table()


## ============================================================================
## SECTION 3 -- THE PER-SECTOR ESTIMATOR  (ETWFE + pair FE)
## ============================================================================
estimate_sector <- function(sector) {

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

  dt[is.na(v), v := 0]
  dt[, ln_tariff := log(ADV + 1)]
  ## (no gravity controls needed here -- the pair FE absorbs them)
  dt[, flag := sum(v), by = .(i, hs6)]; dt <- dt[flag != 0]

  ## --- cohort-specific pre-period baseline (IDENTICAL to Run 2) ------------
  dt <- merge(dt, fta_cohort, by = c("i", "j"), all.x = TRUE)     # cohort = NA -> never-treated
  dt <- dt[is.na(cohort) | cohort != waves[1]]                    # drop always-treated
  dt[, treat_cohort_year := "0_baseline"]                                   # reference bucket
  dt[!is.na(cohort) & year >= cohort,                                       # treated & post
     treat_cohort_year := paste0("g", cohort, "_y", year)]                  #   -> own cell
  dt[, treat_cohort_year := relevel(factor(treat_cohort_year), ref = "0_baseline")]

  if (nrow(dt) == 0L || nlevels(dt$treat_cohort_year) < 2)
    return(data.table(gtap23 = sector, FTA_coef = NA_real_, FTA_SE = NA_real_,
                      tariff_coef = NA_real_, n_hs6 = uniqueN(dt$hs6), nobs = nrow(dt), n_cells = 0L,
                      status = "no_variation"))

  ## --- the regression ------------------------------------------------------
  ## ### CHANGE vs RUN 2 ### : the fixed-effect block adds the pair FE "+ i^j"
  ## and the gravity controls (l_distw, COLONY, CONTIG, COMLANG_OFF) are gone.
  ## Everything else is exactly Run 2.
  m <- tryCatch(
    fepois(v ~ i(treat_cohort_year, ref = "0_baseline") + ln_tariff |
             i^year^hs6 + j^year^hs6 + i^j,
           data = dt, cluster = ~ i^j, warn = FALSE, notes = FALSE),
    error = function(e) NULL)

  if (is.null(m))
    return(data.table(gtap23 = sector, FTA_coef = NA_real_, FTA_SE = NA_real_,
                      tariff_coef = NA_real_, n_hs6 = uniqueN(dt$hs6), nobs = nrow(dt), n_cells = 0L,
                      status = "dropped"))

  ## --- aggregate the cohort-year effects (IDENTICAL to Run 2) --------------
  cf <- coef(m); V <- vcov(m)
  cells <- grep("^treat_cohort_year::", names(cf), value = TRUE)
  cells <- cells[!grepl("0_baseline", cells)]
  cells <- cells[!is.na(cf[cells])]
  if (length(cells) == 0L)
    return(data.table(gtap23 = sector, FTA_coef = NA_real_, FTA_SE = NA_real_,
                      tariff_coef = if ("ln_tariff" %in% names(cf)) cf["ln_tariff"] else NA_real_,
                      n_hs6 = uniqueN(dt$hs6), nobs = m$nobs, n_cells = 0L, status = "no_cells"))

  levs <- sub("^treat_cohort_year::", "", cells)
  N_gs <- vapply(levs, function(L) sum(dt$treat_cohort_year == L), numeric(1))
  w    <- N_gs / sum(N_gs)
  delta_bar <- sum(w * cf[cells])
  se_bar    <- sqrt(as.numeric(t(w) %*% V[cells, cells] %*% w))

  data.table(
    gtap23      = sector,
    FTA_coef    = delta_bar,
    FTA_SE      = se_bar,
    tariff_coef = if ("ln_tariff" %in% names(cf)) unname(cf["ln_tariff"]) else NA_real_,
    n_hs6       = uniqueN(dt$hs6),
    nobs        = m$nobs,
    n_cells     = length(cells),
    status      = "ok")
}


## ============================================================================
## SECTION 4 -- RUN ALL SECTORS SEQUENTIALLY   (same pattern as Run 2/3)
## ============================================================================
t_start <- Sys.time()
for (sec in gtap_sectors) {
  target <- file.path(out_dir, paste0("run_", sec, ".rds"))
  if (file.exists(target)) next
  out <- estimate_sector(sec)
  saveRDS(out, target)
  cat(format(Sys.time(), "%H:%M"), "- finished", sec, "->", out$status,
      "| ETWFE+pairFE FTA effect:", round(out$FTA_coef, 3), "\n"); flush.console()
  gc()
}
cat("\nAll sectors done in",
    round(as.numeric(Sys.time() - t_start, units = "mins"), 1), "minutes.\n")

results <- rbindlist(lapply(list.files(out_dir, "^run_.*\\.rds$", full.names = TRUE), readRDS))


## ============================================================================
## SECTION 5 -- RESULTS TABLE + SIGNIFICANCE SUMMARY   (same logic as Run 2/3)
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

write_csv(results_table, file.path(out_dir, "results_run4_etwfe_pairfe.csv"))

summary_run4 <- tibble(
  run = "Run 4: ETWFE + bilateral FE",
  sectors_estimated = sum(results_table$status == "ok"),
  pct_positive      = round(100 * mean(results_table$positive, na.rm = TRUE), 1),
  pct_pos_sig_1pct  = round(100 * mean(results_table$pos_sig_1pct, na.rm = TRUE), 1),
  pct_pos_sig_5pct  = round(100 * mean(results_table$pos_sig_5pct, na.rm = TRUE), 1),
  pct_pos_sig_10pct = round(100 * mean(results_table$pos_sig_10pct, na.rm = TRUE), 1))
write_csv(summary_run4, file.path(out_dir, "summary_run4_etwfe_pairfe.csv"))

cat("\n================= RUN 4 (ETWFE + pair FE) RESULTS =================\n")
print(as.data.frame(results_table |>
        select(gtap23, FTA_coef, FTA_SE, t_stat, sig_1pct, sig_5pct, sig_10pct)), row.names = FALSE)
cat("\nSectors with a POSITIVE & SIGNIFICANT FTA effect:\n")
cat(sprintf("  at 1%%  : %.1f%%\n  at 5%%  : %.1f%%\n  at 10%% : %.1f%%\n",
            summary_run4$pct_pos_sig_1pct, summary_run4$pct_pos_sig_5pct, summary_run4$pct_pos_sig_10pct))

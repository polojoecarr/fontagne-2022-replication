## ============================================================================
## RUN 3 -- AVERAGE FTA IMPACT BY GTAP-23 SECTOR  (bilateral fixed effect)
## ============================================================================
##
## WHAT THIS DOES (and how it differs from Run 1)
##   Identical to Run 1 -- the base Fontagne FTA regression -- EXCEPT that we
##   REPLACE the four time-invariant gravity control variables (distance,
##   common colony, contiguity, common language) with a single BILATERAL
##   (country-pair) FIXED EFFECT, i^j.
##
##   Why: distance, colony, contiguity and language are all fixed properties of
##   a country pair. A pair fixed effect absorbs ALL of them at once -- plus any
##   other time-invariant bilateral trade cost we did not measure. Identification
##   of the FTA effect then comes purely from CHANGES WITHIN a pair over time
##   (a pair signing an FTA between waves), which is the Baier & Bergstrand
##   (2007) approach and is generally seen as the cleaner identification.
##
##   Trade-off: with only six (non-consecutive) waves there is less within-pair
##   variation, so some sectors may be less precisely estimated than in Run 1.
##
## ESTIMATING EQUATION (per GTAP-23 sector, pooling its HS6 products):
##   v_ijk,t = exp[ beta * FTA_ij,t + rho * ln(1+tariff_ijk,t)
##                + exporter x year x HS6 FE
##                + importer x year x HS6 FE
##                + PAIR FE (i^j)  <-- replaces the gravity controls ] * error
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
out_dir      <- file.path(etwfe_dir, "FTA_GTAP23_Outputs", "run3_pairfe")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
waves <- c(2001, 2004, 2007, 2010, 2013, 2016)


## ============================================================================
## SECTION 2 -- INPUTS: CONCORDANCE AND FTA DUMMY   (same as Run 1)
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
fta_lookup <- bind_rows(bilateral, rename(bilateral, i = j, j = i)) |>
  distinct(i, j, year) |> filter(year %in% waves) |> mutate(FTA_ = 1L) |> as.data.table()


## ============================================================================
## SECTION 3 -- THE PER-SECTOR ESTIMATOR
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
                      tariff_coef = NA_real_, n_hs6 = 0L, nobs = 0L, status = "read_error"))

  dt <- merge(dt, fta_lookup, by = c("i", "j", "year"), all.x = TRUE)
  dt[is.na(FTA_), FTA_ := 0L]

  dt[is.na(v), v := 0]
  dt[, ln_tariff := log(ADV + 1)]
  ## NOTE: we no longer need l_distw / the gravity controls -- the pair FE
  ## absorbs them -- but computing ln_tariff is still required.
  dt[, flag := sum(v), by = .(i, hs6)]; dt <- dt[flag != 0]

  if (nrow(dt) == 0L || uniqueN(dt$FTA_) < 2)
    return(data.table(gtap23 = sector, FTA_coef = NA_real_, FTA_SE = NA_real_,
                      tariff_coef = NA_real_, n_hs6 = uniqueN(dt$hs6), nobs = 0L, status = "no_variation"))

  ## --- the regression -----------------------------------------------------
  ## *** THE ONLY CHANGE FROM RUN 1 ***
  ## Run 1 FE + controls:  v ~ FTA_ + ln_tariff + l_distw + COLONY + CONTIG +
  ##                             COMLANG_OFF | i^year^hs6 + j^year^hs6
  ## Run 3 drops the four gravity controls and adds the pair fixed effect i^j:
  m <- tryCatch(
    fepois(v ~ FTA_ + ln_tariff | i^year^hs6 + j^year^hs6 + i^j,
           data = dt, cluster = ~ i^j, warn = FALSE, notes = FALSE),
    error = function(e) NULL)

  if (is.null(m) || !("FTA_" %in% names(coef(m))))
    return(data.table(gtap23 = sector, FTA_coef = NA_real_, FTA_SE = NA_real_,
                      tariff_coef = NA_real_, n_hs6 = uniqueN(dt$hs6), nobs = nrow(dt), status = "dropped"))

  ct <- summary(m)$coeftable
  data.table(
    gtap23      = sector,
    FTA_coef    = ct["FTA_", "Estimate"],
    FTA_SE      = ct["FTA_", "Std. Error"],
    tariff_coef = if ("ln_tariff" %in% rownames(ct)) ct["ln_tariff", "Estimate"] else NA_real_,
    n_hs6       = uniqueN(dt$hs6),
    nobs        = m$nobs,
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
      "| FTA coef:", round(out$FTA_coef, 3), "\n"); flush.console()
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
  select(gtap23, n_hs6, FTA_coef, FTA_SE, t_stat, pct_trade_chg,
         positive, sig_1pct, sig_5pct, sig_10pct,
         pos_sig_1pct, pos_sig_5pct, pos_sig_10pct, nobs, status)

write_csv(results_table, file.path(out_dir, "results_run3_pairfe.csv"))

summary_run3 <- tibble(
  run = "Run 3: Bilateral (pair) fixed effect",
  sectors_estimated = sum(results_table$status == "ok"),
  pct_positive      = round(100 * mean(results_table$positive, na.rm = TRUE), 1),
  pct_pos_sig_1pct  = round(100 * mean(results_table$pos_sig_1pct, na.rm = TRUE), 1),
  pct_pos_sig_5pct  = round(100 * mean(results_table$pos_sig_5pct, na.rm = TRUE), 1),
  pct_pos_sig_10pct = round(100 * mean(results_table$pos_sig_10pct, na.rm = TRUE), 1))
write_csv(summary_run3, file.path(out_dir, "summary_run3_pairfe.csv"))

cat("\n================= RUN 3 (pair FE) RESULTS =================\n")
print(as.data.frame(results_table |>
        select(gtap23, FTA_coef, FTA_SE, t_stat, sig_1pct, sig_5pct, sig_10pct)), row.names = FALSE)
cat("\nSectors with a POSITIVE & SIGNIFICANT FTA effect:\n")
cat(sprintf("  at 1%%  : %.1f%%\n  at 5%%  : %.1f%%\n  at 10%% : %.1f%%\n",
            summary_run3$pct_pos_sig_1pct, summary_run3$pct_pos_sig_5pct, summary_run3$pct_pos_sig_10pct))

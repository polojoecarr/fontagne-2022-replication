## ============================================================================
## RUN 1 -- AVERAGE FTA IMPACT BY GTAP-23 SECTOR  (base Fontagne method)
## ============================================================================
##
## WHAT THIS DOES
##   For each GTAP-23 sector we pool all of its HS6 products and estimate ONE
##   average FTA effect, using the Fontagne (2022) product-level machinery that
##   the rest of this project is built on:
##       * PPML (fepois) on trade values, robust to zeros
##       * fixed effects kept at the HS6 level (exporter x year x HS6 and
##         importer x year x HS6)
##       * the applied tariff and the gravity controls as covariates
##   The one new ingredient is a Free Trade Agreement dummy (FTA_) taken from
##   the Deep Trade Agreements (DTA) dataset.
##
##   This is the plain "two-way fixed effects" (TWFE) treatment of the FTA: a
##   single 0/1 dummy. Run 2 replaces it with the heterogeneity-robust ETWFE
##   estimator; Run 3 swaps the gravity controls for a bilateral fixed effect.
##
## ESTIMATING EQUATION (per GTAP-23 sector g, pooling its HS6 products k):
##   v_ijk,t = exp[ beta * FTA_ij,t                <- average FTA effect (what we want)
##                + rho  * ln(1+tariff_ijk,t)
##                + gravity controls (dist, colony, contiguity, language)
##                + a_ik,t (exporter x year x HS6 FE)
##                + a_jk,t (importer x year x HS6 FE) ] * error
##
## The three runs share the SAME skeleton (Sections 1-5). Only the regression
## in Section 3 changes between them, so they are easy to compare side by side.
## ============================================================================


## ============================================================================
## SECTION 1 -- ENVIRONMENT, PACKAGES AND FILE PATHS
## ============================================================================
user_lib <- Sys.getenv("R_LIBS_USER"); if (nzchar(user_lib)) .libPaths(c(user_lib, .libPaths()))

library(data.table)   # fast CSV reading (fread) and in-memory pooling
library(fixest)       # PPML with high-dimensional fixed effects (fepois)
library(readxl)       # read the DTA workbook
library(dplyr); library(tidyr); library(readr); library(stringr)

## Let fepois use every CPU core INTERNALLY. We then run the sectors one at a
## time (Section 4) so that only ONE big sector sits in memory at once.
setFixest_nthreads(parallel::detectCores())

## ---- Folder layout (edit `project_root` if you move the project) ----------
project_root <- "C:/Claude Code Project Folder/Fontange 2022"
sigma_dir    <- file.path(project_root, "Replic_FGO", "Replic_FGO")   # per-HS6 trade+tariff files
etwfe_dir    <- file.path(project_root, "R_Replication", "ETWFE")     # concordances + DTA live here
out_dir      <- file.path(etwfe_dir, "FTA_GTAP23_Outputs", "run1_base")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

waves <- c(2001, 2004, 2007, 2010, 2013, 2016)   # the six years present in the Sigma files


## ============================================================================
## SECTION 2 -- INPUTS: HS6 -> GTAP23 CONCORDANCE, AND THE FTA DATA
## ============================================================================

## ---- 2a. HS6 -> GTAP23 (via HS6 -> GTAP65 -> GTAP23) -----------------------
hs6_to_g65 <- read_csv(file.path(etwfe_dir, "GTAP to HS6 (1).csv"), show_col_types = FALSE) |>
  transmute(hs6 = str_pad(as.character(Code), 6, pad = "0"),
            gtap65 = tolower(trimws(GSEC3_rev_lower_case)))
g65_to_g23 <- read_csv(file.path(etwfe_dir, "GTAP 65 to GTAP 23 Concordence (1).csv"), show_col_types = FALSE) |>
  transmute(gtap65 = tolower(trimws(`Full Disaggregation`)),
            gtap23 = tolower(trimws(GTAP23))) |>
  filter(!is.na(gtap65))

hs6_gtap23 <- hs6_to_g65 |>
  left_join(g65_to_g23, by = "gtap65") |>
  filter(!is.na(gtap23)) |>
  distinct(hs6, gtap23)

## Split the HS6 codes into one vector per GTAP23 sector, smallest sector first
## (so that if memory is tight the big sectors fail last, after most are done).
sector_size  <- hs6_gtap23 |> count(gtap23, name = "n")
gtap_sectors <- sector_size |> arrange(n) |> pull(gtap23)
gtap_groups  <- split(hs6_gtap23$hs6, hs6_gtap23$gtap23)

## ---- 2b. FTA dummy from the Deep Trade Agreements dataset ------------------
## Which agreement types count as an "FTA" (reciprocal goods liberalisation).
## EDIT this vector to broaden/narrow the definition.
fta_types <- c("FTA", "FTA & EIA", "CU", "CU & EIA")

bilateral <- read_excel(file.path(etwfe_dir, "DTA 2.0 - Vertical Content (v2).xlsx"),
                        sheet = "Bilateral Information") |>
  filter(type %in% fta_types) |>
  transmute(i = toupper(trimws(iso1)), j = toupper(trimws(iso2)), year = as.integer(year))

## Make the pair symmetric (an FTA between i and j applies in both directions)
## and mark it active at each wave-year. We call the column FTA_ (trailing
## underscore) so it does NOT clash with the Sigma files' own 'FTA' column.
fta_lookup <- bind_rows(bilateral, rename(bilateral, i = j, j = i)) |>
  distinct(i, j, year) |>
  filter(year %in% waves) |>
  mutate(FTA_ = 1L) |>
  as.data.table()


## ============================================================================
## SECTION 3 -- THE PER-SECTOR ESTIMATOR   (this is the part that differs
##              between Run 1, Run 2 and Run 3)
## ============================================================================
estimate_sector <- function(sector) {

  ## --- read & pool every HS6 file in this GTAP23 sector --------------------
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

  ## --- attach the FTA dummy (0 where no agreement) -------------------------
  dt <- merge(dt, fta_lookup, by = c("i", "j", "year"), all.x = TRUE)
  dt[is.na(FTA_), FTA_ := 0L]

  ## --- cleaning, identical to the Fontagne pipeline ------------------------
  dt[is.na(v), v := 0]                    # missing trade = zero trade (PPML uses zeros)
  dt[, ln_tariff := log(ADV + 1)]         # ln(1 + tariff)
  dt[, l_distw   := log(DISTW)]           # ln(distance)
  dt[, flag := sum(v), by = .(i, hs6)]    # drop exporters that never ship a given HS6
  dt <- dt[flag != 0]

  ## need variation in both the tariff and the FTA dummy to identify anything
  if (nrow(dt) == 0L || uniqueN(dt$FTA_) < 2)
    return(data.table(gtap23 = sector, FTA_coef = NA_real_, FTA_SE = NA_real_,
                      tariff_coef = NA_real_, n_hs6 = uniqueN(dt$hs6), nobs = 0L, status = "no_variation"))

  ## --- the regression (the paper's Equation 5, plus the FTA dummy) ---------
  ## FE kept at HS6 level; standard errors clustered by country-pair (i^j),
  ## which is the standard choice for bilateral trade panels.
  m <- tryCatch(
    fepois(v ~ FTA_ + ln_tariff + l_distw + COLONY + CONTIG + COMLANG_OFF |
             i^year^hs6 + j^year^hs6,
           data = dt, cluster = ~ i^j, warn = FALSE, notes = FALSE),
    error = function(e) NULL)

  if (is.null(m) || !("FTA_" %in% names(coef(m))))
    return(data.table(gtap23 = sector, FTA_coef = NA_real_, FTA_SE = NA_real_,
                      tariff_coef = NA_real_, n_hs6 = uniqueN(dt$hs6), nobs = nrow(dt), status = "dropped"))

  ct <- summary(m)$coeftable
  data.table(
    gtap23      = sector,
    FTA_coef    = ct["FTA_", "Estimate"],       # <- the average FTA effect for this sector
    FTA_SE      = ct["FTA_", "Std. Error"],
    tariff_coef = if ("ln_tariff" %in% rownames(ct)) ct["ln_tariff", "Estimate"] else NA_real_,
    n_hs6       = uniqueN(dt$hs6),
    nobs        = m$nobs,
    status      = "ok")
}


## ============================================================================
## SECTION 4 -- RUN ALL SECTORS SEQUENTIALLY (fepois multithreaded internally)
## ============================================================================
t_start <- Sys.time()
for (sec in gtap_sectors) {
  target <- file.path(out_dir, paste0("run_", sec, ".rds"))
  if (file.exists(target)) next                       # resume-friendly: skip finished sectors
  out <- estimate_sector(sec)
  saveRDS(out, target)
  cat(format(Sys.time(), "%H:%M"), "- finished", sec, "->", out$status,
      "| FTA coef:", round(out$FTA_coef, 3), "\n"); flush.console()
  gc()
}
cat("\nAll sectors done in",
    round(as.numeric(Sys.time() - t_start, units = "mins"), 1), "minutes.\n")

## stitch the per-sector files together
results <- rbindlist(lapply(list.files(out_dir, "^run_.*\\.rds$", full.names = TRUE), readRDS))


## ============================================================================
## SECTION 5 -- RESULTS TABLE + SIGNIFICANCE SUMMARY
## ============================================================================
z_1 <- 2.576; z_5 <- 1.960; z_10 <- 1.645   # two-sided normal critical values

results_table <- results |> as_tibble() |>
  mutate(
    t_stat        = abs(FTA_coef / FTA_SE),
    pct_trade_chg = exp(FTA_coef) - 1,                 # FTA effect as a % change in trade
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

write_csv(results_table, file.path(out_dir, "results_run1_base.csv"))

## Headline: share of sectors with a POSITIVE and SIGNIFICANT FTA effect
n_est <- sum(results_table$status == "ok")
summary_run1 <- tibble(
  run = "Run 1: Base Fontagne (TWFE FTA dummy)",
  sectors_estimated = n_est,
  pct_positive          = round(100 * mean(results_table$positive, na.rm = TRUE), 1),
  pct_pos_sig_1pct      = round(100 * mean(results_table$pos_sig_1pct, na.rm = TRUE), 1),
  pct_pos_sig_5pct      = round(100 * mean(results_table$pos_sig_5pct, na.rm = TRUE), 1),
  pct_pos_sig_10pct     = round(100 * mean(results_table$pos_sig_10pct, na.rm = TRUE), 1))
write_csv(summary_run1, file.path(out_dir, "summary_run1_base.csv"))

cat("\n================= RUN 1 RESULTS =================\n")
print(as.data.frame(results_table |>
        select(gtap23, FTA_coef, FTA_SE, t_stat, sig_1pct, sig_5pct, sig_10pct)), row.names = FALSE)
cat("\nSectors with a POSITIVE & SIGNIFICANT FTA effect:\n")
cat(sprintf("  at 1%%  : %.1f%%\n  at 5%%  : %.1f%%\n  at 10%% : %.1f%%\n",
            summary_run1$pct_pos_sig_1pct, summary_run1$pct_pos_sig_5pct, summary_run1$pct_pos_sig_10pct))

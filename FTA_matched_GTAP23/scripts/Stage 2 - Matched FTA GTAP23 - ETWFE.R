## ============================================================================
## STAGE 2 -- MATCHED-FTA IMPACT BY GTAP-23 SECTOR   (ETWFE / staggered DiD)
## ============================================================================
##
## Same as Stage 1, but the MATCHED FTA is now estimated with the
## heterogeneity-robust ETWFE (Wooldridge 2021, 2023; Nagengast & Yotov 2023):
##   * cohort g = the first wave in which a pair gets a MATCHED FTA;
##   * a cohort-specific pre-period baseline (never-matched pairs -- which
##     includes both no-FTA and "other"-only pairs -- plus each cohort's own
##     pre-treatment years -- form the reference);
##   * cohort-year effects are aggregated to one number (N_gs/N_D weights,
##     delta-method SE).
##   * FTA_other stays in as a plain time-varying CONTROL DUMMY (not ETWFE'd).
##
## Only Stage-1's single matched dummy is replaced by the ETWFE block; the FE,
## controls, sample cleaning and agri/goods split are unchanged.
## ============================================================================

## ============================================================================
## SECTION 1 -- ENVIRONMENT (same as Stage 1)
## ============================================================================
user_lib <- Sys.getenv("R_LIBS_USER"); if (nzchar(user_lib)) .libPaths(c(user_lib, .libPaths()))
library(data.table); library(fixest); library(dplyr); library(tidyr); library(readr); library(stringr)
setFixest_nthreads(parallel::detectCores())

project_root <- "C:/Claude Code Project Folder/Fontange 2022"
sigma_dir    <- file.path(project_root, "Replic_FGO", "Replic_FGO")
etwfe_dir    <- file.path(project_root, "R_Replication", "ETWFE")
rd2_path     <- file.path(project_root, "R_Replication", "regression_data_2.rds")
out_dir      <- file.path(etwfe_dir, "FTA_matched_GTAP23_Outputs", "stage2_etwfe")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
waves <- c(2001, 2004, 2007, 2010, 2013, 2016)

## ============================================================================
## SECTION 2 -- INPUTS (same as Stage 1)
## ============================================================================
hs6_to_g65 <- read_csv(file.path(etwfe_dir, "GTAP to HS6 (1).csv"), show_col_types = FALSE) |>
  transmute(hs6 = str_pad(as.character(Code), 6, pad = "0"), gtap65 = tolower(trimws(GSEC3_rev_lower_case)))
g65_to_g23 <- read_csv(file.path(etwfe_dir, "GTAP 65 to GTAP 23 Concordence (1).csv"), show_col_types = FALSE) |>
  transmute(gtap65 = tolower(trimws(`Full Disaggregation`)), gtap23 = tolower(trimws(GTAP23))) |> filter(!is.na(gtap65))
hs6_gtap23 <- hs6_to_g65 |> left_join(g65_to_g23, by = "gtap65") |> filter(!is.na(gtap23)) |> distinct(hs6, gtap23)
agri_sectors <- c("aff", "b_t", "ofd", "prf")
gtap_sectors <- hs6_gtap23 |> count(gtap23, name = "n") |> arrange(n) |> pull(gtap23)
gtap_groups  <- split(hs6_gtap23$hs6, hs6_gtap23$gtap23)

rd <- as.data.table(readRDS(rd2_path)); setnames(rd, c("exporter_iso3","importer_iso3"), c("i","j"))
rd <- rd[year %in% waves, .(i = toupper(trimws(i)), j = toupper(trimws(j)), year = as.integer(year),
                            m_agri = as.integer(FTA_matched_agri), o_agri = as.integer(FTA_other_agri),
                            m_goods = as.integer(FTA_matched_goods), o_goods = as.integer(FTA_other_goods))]
fta_lookup <- unique(rbind(rd, rd[, .(i = j, j = i, year, m_agri, o_agri, m_goods, o_goods)]))

## ============================================================================
## SECTION 3 -- THE PER-SECTOR ETWFE ESTIMATOR
## ============================================================================
estimate_sector <- function(sector) {
  type <- if (sector %in% agri_sectors) "agri" else "goods"
  hs6_list <- gtap_groups[[sector]]
  read_one <- function(h) {
    f <- file.path(sigma_dir, paste0("Sigma_HS6_", h, ".csv"))
    d <- tryCatch(fread(f, sep = ";"), error = function(e) NULL)
    if (is.null(d) || nrow(d) == 0L) return(NULL); d[, hs6 := h]; d
  }
  dt <- rbindlist(lapply(hs6_list, read_one), use.names = TRUE, fill = TRUE)
  fail <- function(st, n = 0L) data.table(gtap23 = sector, type = type, matched_coef = NA_real_, matched_SE = NA_real_,
    t_stat = NA_real_, other_coef = NA_real_, other_SE = NA_real_, n_cells = 0L,
    n_hs6 = if (is.null(dt)) 0L else uniqueN(dt$hs6), nobs = n, matched_share = NA_real_, status = st)
  if (is.null(dt) || nrow(dt) == 0L) return(fail("read_error"))

  ## attach matched / other for this sector's type
  L <- fta_lookup[, .(i, j, year, FTA_matched = get(paste0("m_", type)), FTA_other = get(paste0("o_", type)))]
  dt <- merge(dt, L, by = c("i", "j", "year"), all.x = TRUE)
  dt[is.na(FTA_matched), FTA_matched := 0L]; dt[is.na(FTA_other), FTA_other := 0L]
  dt[is.na(v), v := 0]; dt[, ln_tariff := log(ADV + 1)]; dt[, l_distw := log(DISTW)]
  dt[, flag := sum(v), by = .(i, hs6)]; dt <- dt[flag != 0]
  matched_share <- mean(dt$FTA_matched)

  ## cohort-specific pre-period baseline, built on the MATCHED FTA
  dt[, cohort := { y <- year[FTA_matched == 1]; if (length(y)) min(y) else NA_integer_ }, by = .(i, j)]
  dt <- dt[is.na(cohort) | cohort != waves[1]]           # drop always-matched (matched in first wave)
  dt[, treat_cohort_year := "0_baseline"]
  dt[!is.na(cohort) & year >= cohort, treat_cohort_year := paste0("g", cohort, "_y", year)]
  dt[, treat_cohort_year := relevel(factor(treat_cohort_year), ref = "0_baseline")]
  if (nrow(dt) == 0L || nlevels(dt$treat_cohort_year) < 2) return(fail("no_variation", nrow(dt)))

  ## saturated ETWFE on matched + FTA_other control
  m <- tryCatch(
    fepois(v ~ i(treat_cohort_year, ref = "0_baseline") + FTA_other +
             ln_tariff + l_distw + COLONY + CONTIG + COMLANG_OFF | i^year^hs6 + j^year^hs6,
           data = dt, cluster = ~ i^j, warn = FALSE, notes = FALSE),
    error = function(e) NULL)
  if (is.null(m)) return(fail("dropped", nrow(dt)))

  ## aggregate cohort-year effects -> one matched effect (Equation 7 + delta SE)
  cf <- coef(m); V <- vcov(m)
  cells <- grep("^treat_cohort_year::", names(cf), value = TRUE)
  cells <- cells[!grepl("0_baseline", cells)]; cells <- cells[!is.na(cf[cells])]
  if (length(cells) == 0L) return(fail("no_cells", m$nobs))
  levs <- sub("^treat_cohort_year::", "", cells)
  N_gs <- vapply(levs, function(L) sum(dt$treat_cohort_year == L), numeric(1)); w <- N_gs / sum(N_gs)
  delta_bar <- sum(w * cf[cells]); se_bar <- sqrt(as.numeric(t(w) %*% V[cells, cells] %*% w))
  data.table(gtap23 = sector, type = type, matched_coef = delta_bar, matched_SE = se_bar,
             t_stat = abs(delta_bar / se_bar),
             other_coef = if ("FTA_other" %in% names(cf)) unname(cf["FTA_other"]) else NA_real_,
             other_SE = if ("FTA_other" %in% rownames(summary(m)$coeftable)) summary(m)$coeftable["FTA_other","Std. Error"] else NA_real_,
             n_cells = length(cells), n_hs6 = uniqueN(dt$hs6), nobs = m$nobs,
             matched_share = matched_share, status = "ok")
}

## ============================================================================
## SECTION 4 -- RUN ALL SECTORS SEQUENTIALLY
## ============================================================================
t_start <- Sys.time()
for (sec in gtap_sectors) {
  target <- file.path(out_dir, paste0("run_", sec, ".rds"))
  if (file.exists(target)) next
  out <- estimate_sector(sec); saveRDS(out, target)
  cat(format(Sys.time(), "%H:%M"), "-", sec, "(", out$type, ") ->", out$status,
      "| matched ETWFE:", round(out$matched_coef, 3), "\n"); flush.console(); gc()
}
cat("\nStage 2 done in", round(as.numeric(Sys.time() - t_start, units = "mins"), 1), "minutes.\n")
results <- rbindlist(lapply(list.files(out_dir, "^run_.*\\.rds$", full.names = TRUE), readRDS))

## ============================================================================
## SECTION 5 -- RESULTS TABLE + SIGNIFICANCE SUMMARY
## ============================================================================
z_1 <- 2.576; z_5 <- 1.960; z_10 <- 1.645
results_table <- results |> as_tibble() |>
  mutate(positive = matched_coef > 0,
         sig_1pct = !is.na(t_stat) & t_stat > z_1, sig_5pct = !is.na(t_stat) & t_stat > z_5,
         sig_10pct = !is.na(t_stat) & t_stat > z_10,
         pos_sig_1pct = positive & sig_1pct, pos_sig_5pct = positive & sig_5pct, pos_sig_10pct = positive & sig_10pct) |>
  arrange(gtap23) |>
  select(gtap23, type, matched_coef, matched_SE, t_stat, other_coef, other_SE, n_cells,
         positive, sig_1pct, sig_5pct, sig_10pct, pos_sig_1pct, pos_sig_5pct, pos_sig_10pct,
         matched_share, nobs, status)
write_csv(results_table, file.path(out_dir, "results_stage2_etwfe.csv"))

summary_stage2 <- tibble(run = "Stage 2: ETWFE (matched FTA)",
  sectors_estimated = sum(results_table$status == "ok"),
  pct_positive = round(100 * mean(results_table$positive, na.rm = TRUE), 1),
  pct_pos_sig_1pct = round(100 * mean(results_table$pos_sig_1pct, na.rm = TRUE), 1),
  pct_pos_sig_5pct = round(100 * mean(results_table$pos_sig_5pct, na.rm = TRUE), 1),
  pct_pos_sig_10pct = round(100 * mean(results_table$pos_sig_10pct, na.rm = TRUE), 1))
write_csv(summary_stage2, file.path(out_dir, "summary_stage2_etwfe.csv"))

cat("\n============ STAGE 2 (matched FTA, ETWFE) ============\n")
print(as.data.frame(results_table |> select(gtap23, type, matched_coef, matched_SE, t_stat, sig_1pct, sig_5pct, sig_10pct)), row.names = FALSE)
cat(sprintf("\nMatched ETWFE positive & significant:  1%%: %.1f%%   5%%: %.1f%%   10%%: %.1f%%\n",
            summary_stage2$pct_pos_sig_1pct, summary_stage2$pct_pos_sig_5pct, summary_stage2$pct_pos_sig_10pct))

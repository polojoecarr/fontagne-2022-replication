## ============================================================================
## STAGE 3 -- MATCHED-FTA IMPACT BY GTAP-23 SECTOR   (bilateral fixed effect)
## ============================================================================
##
## Identical to Stage 1 (matched dummy treatment, other-FTA control), EXCEPT the
## four gravity controls (distance, colony, contiguity, language) are replaced
## by a bilateral country-pair fixed effect i^j. Identification of the matched
## effect then comes from WITHIN-pair change over time (Baier & Bergstrand).
##
##   v_ijk,t = exp[ beta * FTA_matched_ij,t + gamma * FTA_other_ij,t
##                + rho * ln(1+tariff)
##                + exporter x year x HS6 FE + importer x year x HS6 FE
##                + PAIR FE (i^j) ] * error
##
## The ONLY change from Stage 1 is the fixed-effect block (### CHANGE vs S1 ###).
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
out_dir      <- file.path(etwfe_dir, "FTA_matched_GTAP23_Outputs", "stage3_pairfe")
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
## SECTION 3 -- THE PER-SECTOR ESTIMATOR
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
    t_stat = NA_real_, other_coef = NA_real_, other_SE = NA_real_,
    n_hs6 = if (is.null(dt)) 0L else uniqueN(dt$hs6), nobs = n, matched_share = NA_real_, status = st)
  if (is.null(dt) || nrow(dt) == 0L) return(fail("read_error"))

  L <- fta_lookup[, .(i, j, year, FTA_matched = get(paste0("m_", type)), FTA_other = get(paste0("o_", type)))]
  dt <- merge(dt, L, by = c("i", "j", "year"), all.x = TRUE)
  dt[is.na(FTA_matched), FTA_matched := 0L]; dt[is.na(FTA_other), FTA_other := 0L]
  dt[is.na(v), v := 0]; dt[, ln_tariff := log(ADV + 1)]
  dt[, flag := sum(v), by = .(i, hs6)]; dt <- dt[flag != 0]
  matched_share <- mean(dt$FTA_matched)
  if (nrow(dt) == 0L || uniqueN(dt$FTA_matched) < 2) return(fail("no_variation", nrow(dt)))

  ## ### CHANGE vs STAGE 1 ###: drop gravity controls, add pair FE i^j
  m <- tryCatch(
    fepois(v ~ FTA_matched + FTA_other + ln_tariff | i^year^hs6 + j^year^hs6 + i^j,
           data = dt, cluster = ~ i^j, warn = FALSE, notes = FALSE),
    error = function(e) NULL)
  if (is.null(m) || !("FTA_matched" %in% names(coef(m)))) return(fail("dropped", nrow(dt)))

  ct <- summary(m)$coeftable; g <- function(v, c) if (v %in% rownames(ct)) ct[v, c] else NA_real_
  data.table(gtap23 = sector, type = type,
    matched_coef = g("FTA_matched", "Estimate"), matched_SE = g("FTA_matched", "Std. Error"),
    t_stat = abs(g("FTA_matched", "Estimate") / g("FTA_matched", "Std. Error")),
    other_coef = g("FTA_other", "Estimate"), other_SE = g("FTA_other", "Std. Error"),
    n_hs6 = uniqueN(dt$hs6), nobs = m$nobs, matched_share = matched_share, status = "ok")
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
      "| matched (pairFE):", round(out$matched_coef, 3), "\n"); flush.console(); gc()
}
cat("\nStage 3 done in", round(as.numeric(Sys.time() - t_start, units = "mins"), 1), "minutes.\n")
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
  select(gtap23, type, matched_coef, matched_SE, t_stat, other_coef, other_SE,
         positive, sig_1pct, sig_5pct, sig_10pct, pos_sig_1pct, pos_sig_5pct, pos_sig_10pct,
         matched_share, nobs, status)
write_csv(results_table, file.path(out_dir, "results_stage3_pairfe.csv"))

summary_stage3 <- tibble(run = "Stage 3: Bilateral FE (matched FTA)",
  sectors_estimated = sum(results_table$status == "ok"),
  pct_positive = round(100 * mean(results_table$positive, na.rm = TRUE), 1),
  pct_pos_sig_1pct = round(100 * mean(results_table$pos_sig_1pct, na.rm = TRUE), 1),
  pct_pos_sig_5pct = round(100 * mean(results_table$pos_sig_5pct, na.rm = TRUE), 1),
  pct_pos_sig_10pct = round(100 * mean(results_table$pos_sig_10pct, na.rm = TRUE), 1))
write_csv(summary_stage3, file.path(out_dir, "summary_stage3_pairfe.csv"))

cat("\n============ STAGE 3 (matched FTA, bilateral FE) ============\n")
print(as.data.frame(results_table |> select(gtap23, type, matched_coef, matched_SE, t_stat, sig_1pct, sig_5pct, sig_10pct)), row.names = FALSE)
cat(sprintf("\nMatched pair-FE positive & significant:  1%%: %.1f%%   5%%: %.1f%%   10%%: %.1f%%\n",
            summary_stage3$pct_pos_sig_1pct, summary_stage3$pct_pos_sig_5pct, summary_stage3$pct_pos_sig_10pct))

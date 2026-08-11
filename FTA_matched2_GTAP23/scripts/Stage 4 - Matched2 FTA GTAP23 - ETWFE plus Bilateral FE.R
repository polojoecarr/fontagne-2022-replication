## ============================================================================
## STAGE 4 -- MATCHED-FTA IMPACT BY GTAP-23 SECTOR   (ETWFE + bilateral FE)
## ============================================================================
##
## *** SECOND MATCHED TABLE (the regression_data_3 table) ***
##   Identical in every respect to the first matched table in
##   FTA_matched_GTAP23/, EXCEPT that the matched/other split is taken from
##   regression_data_3.rds instead of regression_data_2.rds. The _3 matched
##   group is roughly half the size (goods 1,280 vs 2,326 treated pair-years;
##   agri 990 vs 2,188), i.e. a tighter, more homogeneous treatment group.
##
##   Only THREE specifications are estimated for this table:
##     (1) Baseline Fontagne, (3) Bilateral FE, (4) ETWFE + Bilateral FE.
##   Specification (2) (ETWFE with gravity controls) is deliberately NOT run.
##
##   Working outputs -> ETWFE/FTA_matched2_GTAP23_Outputs/
##   Published table -> FTA_matched2_GTAP23/
##
## Combines Stage 2 and Stage 3: the ETWFE staggered-DiD treatment on the
## MATCHED FTA (Stage 2), with the gravity controls replaced by a bilateral
## pair fixed effect i^j (Stage 3). FTA_other stays as a time-varying control.
## Closest to the Nagengast & Yotov (2023) specification.
##
##   v_ijk,t = exp[ SUM_g SUM_{s>=g} delta_gs * D_gs(matched)   <- ETWFE (matched)
##                + gamma * FTA_other_ij,t                       <- control dummy
##                + rho * ln(1+tariff)
##                + exporter x year x HS6 FE + importer x year x HS6 FE
##                + PAIR FE (i^j) ] * error
##   Aggregate to one matched effect:  delta_bar = SUM (N_gs / N_D) * delta_gs.
##
## ---------------------------------------------------------------------------
## *** CHUNKING ***  Stage 4 is heavy (~12 h). To run it in ~3 h sittings, set
## `chunk_sectors` below to the current chunk. The loop runs only those sectors
## and is resume-friendly (skips any already saved), so it ENDS NATURALLY when
## the chunk is done. Chunks:
##   Chunk 1: b_t mvh otn enr ppp prf omf ofd eeq aff   (the 10 small/medium)
##   Chunk 2: ome tal
##   Chunk 3: mff
##   Chunk 4: crp
## ---------------------------------------------------------------------------

## ============================================================================
## SECTION 1 -- ENVIRONMENT + CHUNK SELECTION
## ============================================================================
user_lib <- Sys.getenv("R_LIBS_USER"); if (nzchar(user_lib)) .libPaths(c(user_lib, .libPaths()))
library(data.table); library(fixest); library(dplyr); library(tidyr); library(readr); library(stringr)
setFixest_nthreads(parallel::detectCores())

## >>> EDIT THIS ONE LINE PER CHUNK, then re-run the script. <<<
## The loop is resume-friendly (it skips sectors already saved), so it ends
## naturally when the chunk is finished.
##   Chunk 1: b_t mvh otn enr ppp prf omf ofd eeq aff   (the 10 small/medium)  ~3.1 h
##   Chunk 2: ome tal                                                          ~2.1 h
##   Chunk 3: mff                                                              ~1.7 h
##   Chunk 4: crp                                                              ~1.8 h
chunk_sectors <- c("mff")   # Chunk 3 of 4

project_root <- "C:/Claude Code Project Folder/Fontange 2022"
sigma_dir    <- file.path(project_root, "Replic_FGO", "Replic_FGO")
etwfe_dir    <- file.path(project_root, "R_Replication", "ETWFE")
rd3_path     <- file.path(project_root, "R_Replication", "regression_data_3.rds")
out_dir      <- file.path(etwfe_dir, "FTA_matched2_GTAP23_Outputs", "stage4_etwfe_pairfe")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
waves <- c(2001, 2004, 2007, 2010, 2013, 2016)

## ============================================================================
## SECTION 2 -- INPUTS (same as the other matched stages)
## ============================================================================
hs6_to_g65 <- read_csv(file.path(etwfe_dir, "GTAP to HS6 (1).csv"), show_col_types = FALSE) |>
  transmute(hs6 = str_pad(as.character(Code), 6, pad = "0"), gtap65 = tolower(trimws(GSEC3_rev_lower_case)))
g65_to_g23 <- read_csv(file.path(etwfe_dir, "GTAP 65 to GTAP 23 Concordence (1).csv"), show_col_types = FALSE) |>
  transmute(gtap65 = tolower(trimws(`Full Disaggregation`)), gtap23 = tolower(trimws(GTAP23))) |> filter(!is.na(gtap65))
hs6_gtap23 <- hs6_to_g65 |> left_join(g65_to_g23, by = "gtap65") |> filter(!is.na(gtap23)) |> distinct(hs6, gtap23)
agri_sectors <- c("aff", "b_t", "ofd", "prf")
size_order  <- hs6_gtap23 |> count(gtap23, name = "n") |> arrange(n) |> pull(gtap23)
run_sectors <- size_order[size_order %in% chunk_sectors]     # this chunk, smallest first
gtap_groups <- split(hs6_gtap23$hs6, hs6_gtap23$gtap23)

rd <- as.data.table(readRDS(rd3_path)); setnames(rd, c("exporter_iso3","importer_iso3"), c("i","j"))
rd <- rd[year %in% waves, .(i = toupper(trimws(i)), j = toupper(trimws(j)), year = as.integer(year),
                            m_agri = as.integer(FTA_matched_agri), o_agri = as.integer(FTA_other_agri),
                            m_goods = as.integer(FTA_matched_goods), o_goods = as.integer(FTA_other_goods))]
fta_lookup <- unique(rbind(rd, rd[, .(i = j, j = i, year, m_agri, o_agri, m_goods, o_goods)]))

## ============================================================================
## SECTION 3 -- THE PER-SECTOR ESTIMATOR (ETWFE-matched + pair FE)
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

  L <- fta_lookup[, .(i, j, year, FTA_matched = get(paste0("m_", type)), FTA_other = get(paste0("o_", type)))]
  dt <- merge(dt, L, by = c("i", "j", "year"), all.x = TRUE)
  dt[is.na(FTA_matched), FTA_matched := 0L]; dt[is.na(FTA_other), FTA_other := 0L]
  dt[is.na(v), v := 0]; dt[, ln_tariff := log(ADV + 1)]
  dt[, flag := sum(v), by = .(i, hs6)]; dt <- dt[flag != 0]
  matched_share <- mean(dt$FTA_matched)

  ## cohort-specific baseline on the MATCHED FTA (same as Stage 2)
  dt[, cohort := { y <- year[FTA_matched == 1]; if (length(y)) min(y) else NA_integer_ }, by = .(i, j)]
  dt <- dt[is.na(cohort) | cohort != waves[1]]
  dt[, treat_cohort_year := "0_baseline"]
  dt[!is.na(cohort) & year >= cohort, treat_cohort_year := paste0("g", cohort, "_y", year)]
  dt[, treat_cohort_year := relevel(factor(treat_cohort_year), ref = "0_baseline")]
  if (nrow(dt) == 0L || nlevels(dt$treat_cohort_year) < 2) return(fail("no_variation", nrow(dt)))

  ## ETWFE(matched) + other control ; pair FE i^j replaces gravity controls (Stage 3)
  m <- tryCatch(
    fepois(v ~ i(treat_cohort_year, ref = "0_baseline") + FTA_other + ln_tariff |
             i^year^hs6 + j^year^hs6 + i^j,
           data = dt, cluster = ~ i^j, warn = FALSE, notes = FALSE),
    error = function(e) NULL)
  if (is.null(m)) return(fail("dropped", nrow(dt)))

  cf <- coef(m); V <- vcov(m)
  cells <- grep("^treat_cohort_year::", names(cf), value = TRUE)
  cells <- cells[!grepl("0_baseline", cells)]; cells <- cells[!is.na(cf[cells])]
  if (length(cells) == 0L) return(fail("no_cells", m$nobs))
  levs <- sub("^treat_cohort_year::", "", cells)
  N_gs <- vapply(levs, function(L) sum(dt$treat_cohort_year == L), numeric(1)); w <- N_gs / sum(N_gs)
  delta_bar <- sum(w * cf[cells]); se_bar <- sqrt(as.numeric(t(w) %*% V[cells, cells] %*% w))
  ctb <- summary(m)$coeftable
  data.table(gtap23 = sector, type = type, matched_coef = delta_bar, matched_SE = se_bar,
             t_stat = abs(delta_bar / se_bar),
             other_coef = if ("FTA_other" %in% names(cf)) unname(cf["FTA_other"]) else NA_real_,
             other_SE = if ("FTA_other" %in% rownames(ctb)) ctb["FTA_other", "Std. Error"] else NA_real_,
             n_cells = length(cells), n_hs6 = uniqueN(dt$hs6), nobs = m$nobs,
             matched_share = matched_share, status = "ok")
}

## ============================================================================
## SECTION 4 -- RUN THIS CHUNK'S SECTORS (ends when the chunk is done)
## ============================================================================
t_start <- Sys.time()
for (sec in run_sectors) {
  target <- file.path(out_dir, paste0("run_", sec, ".rds"))
  if (file.exists(target)) next
  out <- estimate_sector(sec); saveRDS(out, target)
  cat(format(Sys.time(), "%H:%M"), "-", sec, "(", out$type, ") ->", out$status,
      "| matched ETWFE+pairFE:", round(out$matched_coef, 3), "\n"); flush.console(); gc()
}
cat("\nCHUNK done in", round(as.numeric(Sys.time() - t_start, units = "mins"), 1), "minutes",
    "(sectors:", paste(run_sectors, collapse = ", "), ")\n")

## ============================================================================
## SECTION 5 -- RESULTS TABLE (from ALL stage-4 sectors saved so far)
## ============================================================================
results <- rbindlist(lapply(list.files(out_dir, "^run_.*\\.rds$", full.names = TRUE), readRDS))
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
write_csv(results_table, file.path(out_dir, "results_stage4_etwfe_pairfe.csv"))
cat("\nStage-4 sectors saved so far:", nrow(results_table), "of 14\n")
if (nrow(results_table) == 14) {
  summary_stage4 <- tibble(run = "Stage 4: ETWFE + Bilateral FE (matched FTA)",
    sectors_estimated = sum(results_table$status == "ok"),
    pct_positive = round(100 * mean(results_table$positive, na.rm = TRUE), 1),
    pct_pos_sig_1pct = round(100 * mean(results_table$pos_sig_1pct, na.rm = TRUE), 1),
    pct_pos_sig_5pct = round(100 * mean(results_table$pos_sig_5pct, na.rm = TRUE), 1),
    pct_pos_sig_10pct = round(100 * mean(results_table$pos_sig_10pct, na.rm = TRUE), 1))
  write_csv(summary_stage4, file.path(out_dir, "summary_stage4_etwfe_pairfe.csv"))
  cat("STAGE 4 COMPLETE (all 14).  positive & significant  1%:", summary_stage4$pct_pos_sig_1pct,
      " 5%:", summary_stage4$pct_pos_sig_5pct, " 10%:", summary_stage4$pct_pos_sig_10pct, "\n")
}

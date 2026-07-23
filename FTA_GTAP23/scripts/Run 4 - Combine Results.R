## ============================================================================
## RUN 4 -- COMBINED COMPARISON OF THE THREE FTA RUNS
## ============================================================================
## Reads the per-sector results written by Runs 1-3 and produces:
##   (A) a side-by-side table of the FTA coefficient + significance stars for
##       every GTAP-23 sector, one column block per method; and
##   (B) a headline summary comparing the share of sectors with a POSITIVE and
##       SIGNIFICANT FTA effect at the 1%, 5% and 10% levels across methods.
## Run this AFTER Runs 1, 2 and 3 have finished.
## ============================================================================

user_lib <- Sys.getenv("R_LIBS_USER"); if (nzchar(user_lib)) .libPaths(c(user_lib, .libPaths()))
library(dplyr); library(readr); library(tidyr); library(purrr)

project_root <- "C:/Claude Code Project Folder/Fontange 2022"
base_dir <- file.path(project_root, "R_Replication", "ETWFE", "FTA_GTAP23_Outputs")

## significance stars from a t-statistic (2.576 / 1.960 / 1.645 = 1% / 5% / 10%)
stars <- function(t) ifelse(is.na(t), "",
                     ifelse(t > 2.576, "***", ifelse(t > 1.960, "**", ifelse(t > 1.645, "*", ""))))

load_run <- function(sub, file, label) {
  read_csv(file.path(base_dir, sub, file), show_col_types = FALSE) |>
    transmute(gtap23,
              !!paste0(label, "_coef") := round(FTA_coef, 3),
              !!paste0(label, "_t")    := round(t_stat, 2),
              !!paste0(label, "_sig")  := stars(t_stat))
}

r1 <- load_run("run1_base",   "results_run1_base.csv",   "run1")
r2 <- load_run("run2_etwfe",  "results_run2_etwfe.csv",  "run2")
r3 <- load_run("run3_pairfe", "results_run3_pairfe.csv", "run3")

## ---- (A) side-by-side per-sector comparison -------------------------------
comparison <- r1 |> full_join(r2, by = "gtap23") |> full_join(r3, by = "gtap23") |> arrange(gtap23)
write_csv(comparison, file.path(base_dir, "COMBINED_comparison_by_sector.csv"))

## ---- (B) headline significance summary ------------------------------------
summ <- bind_rows(
  read_csv(file.path(base_dir, "run1_base",   "summary_run1_base.csv"),   show_col_types = FALSE),
  read_csv(file.path(base_dir, "run2_etwfe",  "summary_run2_etwfe.csv"),  show_col_types = FALSE),
  read_csv(file.path(base_dir, "run3_pairfe", "summary_run3_pairfe.csv"), show_col_types = FALSE))
write_csv(summ, file.path(base_dir, "COMBINED_significance_summary.csv"))

cat("\n===== FTA effect by GTAP-23 sector, three methods (coef + sig) =====\n")
print(as.data.frame(comparison |>
        select(gtap23, run1_coef, run1_sig, run2_coef, run2_sig, run3_coef, run3_sig)), row.names = FALSE)

cat("\n===== Share of sectors POSITIVE & SIGNIFICANT =====\n")
print(as.data.frame(summ |>
        select(run, sectors_estimated, pct_pos_sig_1pct, pct_pos_sig_5pct, pct_pos_sig_10pct)), row.names = FALSE)
cat("\n(*** 1%, ** 5%, * 10%)\n")

compare_run_1 <- estimates_FTA_GTAP_23 %>% 
  select(gtap, FTA_coef) %>% 
  left_join(sector_comparison %>% select(gtap23, run1_coef), by = c("gtap" = "gtap23"))


    ## Remake and compare concordence

hs6_to_g65 <- read_csv(file.path("Data", "GTAP to HS6.csv"), show_col_types = FALSE) |>
  transmute(hs6 = str_pad(as.character(Code), 6, pad = "0"),
            gtap65 = tolower(trimws(GSEC3_rev_lower_case))) # note that concordence is based on the revised GTAP codes NOT the same one's used in Fontagne

g65_to_g23 <- read_csv(file.path("Data", "GTAP 65 to GTAP 23 Concordence.csv"), show_col_types = FALSE) |>
  transmute(gtap65 = tolower(trimws(`Full Disaggregation`)), gtap23 = tolower(trimws(GTAP23))) |>
  filter(!is.na(gtap65))

hs6_gtap23 <- hs6_to_g65 |> left_join(g65_to_g23, by = "gtap65") |>
  filter(!is.na(gtap23)) |> distinct(hs6, gtap23) # this creates the concordance

hs6_codes_table <- read_csv("Data/hs6_codes_table.csv") %>% # load my original concordance
  select(hs6_codes, original_gtap23 = GTAP23) %>% 
  left_join(hs6_gtap23, by = c("hs6_codes" = "hs6"))

test <- hs6_codes_table %>% filter(original_gtap23 != gtap23)


#################################################################
# Look at Comparisons
#################################################################

# ---------------------------------------------------------------------
# 1. LABELS AND OPTIONS - the only block you should need to edit
# ---------------------------------------------------------------------

# Sector names. Anything not listed here falls back to the raw code.
sector_labels <- c(
  aff = "Agriculture, forestry & fishing",
  b_t = "Beverages & tobacco",
  crp = "Chemicals, rubber & plastics",
  eeq = "Electronic equipment",
  enr = "Energy & extraction",
  mff = "Metal and other Materials",
  mvh = "Motor vehicles & parts",
  ofd = "Food products nec",
  ome = "Machinery & equipment nec",
  omf = "Manufactures nec",
  otn = "Transport equipment nec",
  ppp = "Paper products & publishing",
  prf = "Meat and Processed foods", 
  tal = "Textiles, apparel & leather"
)

# Column headings for each run.
spec_labels <- c(
  run1 = "(1) Fontagne baseline",
  run2 = "(2) ETWFE",
  run3 = "(3) Bilateral FE"
)

# Row ordering: a numeric column name to sort by (descending), or NULL
# to keep sectors in alphabetical order.
sort_by <- "run1_coef"

# ---------------------------------------------------------------------
# 2. SIGNIFICANCE STARS
#    Rebuilt from the t-statistics rather than read from the *_sig
#    columns, so that non-significant cells come out blank instead of NA.
#    Normal-approximation cut-offs: 1%, 5%, 10%.
# ---------------------------------------------------------------------

star <- function(t) {
  case_when(
    is.na(t)        ~ "",
    abs(t) >= 2.576 ~ "***",
    abs(t) >= 1.960 ~ "**",
    abs(t) >= 1.645 ~ "*",
    TRUE            ~ ""
  )
}

# ---------------------------------------------------------------------
# 3. RESHAPE
#    Long by run, format each cell as text, then back to one row per
#    sector with two columns (coefficient, |t|) per run.
# ---------------------------------------------------------------------

sector_order <- if (is.null(sort_by)) {
  sort(sector_comparison$gtap23)
} else {
  sector_comparison$gtap23[order(-sector_comparison[[sort_by]])]
}

tab <- sector_comparison %>%
  select(gtap23, matches("^run[0-9]+_(coef|t)$")) %>%
  pivot_longer(
    -gtap23,
    names_to  = c("spec", ".value"),   # run1_coef -> spec = "run1", value = coef
    names_sep = "_"
  ) %>%
  mutate(
    coef_fmt = paste0(formatC(coef, format = "f", digits = 3), star(t)),
    t_fmt    = paste0("(", formatC(abs(t), format = "f", digits = 2), ")")
  ) %>%
  select(gtap23, spec, coef_fmt, t_fmt) %>%
  pivot_wider(
    names_from  = spec,
    values_from = c(coef_fmt, t_fmt),
    names_glue  = "{spec}_{.value}"
  ) %>%
  mutate(
    gtap23 = factor(gtap23, levels = sector_order),
    sector = coalesce(unname(sector_labels[as.character(gtap23)]),
                      as.character(gtap23))
  ) %>%
  arrange(gtap23) %>%
  select(sector,
         run1_coef_fmt, run1_t_fmt,
         run2_coef_fmt, run2_t_fmt,
         run3_coef_fmt, run3_t_fmt)

# ---------------------------------------------------------------------
# 4. BUILD THE TABLE
# ---------------------------------------------------------------------

tbl <- tab %>%
  gt() %>%
  tab_header(
    title    = md("**Estimated FTA effects by GTAP 23 sector**"),
    subtitle = "PPML gravity estimates, HS6 trade pooled to GTAP 23"
  ) %>%
  tab_spanner(spec_labels[["run1"]], columns = c(run1_coef_fmt, run1_t_fmt)) %>%
  tab_spanner(spec_labels[["run2"]], columns = c(run2_coef_fmt, run2_t_fmt)) %>%
  tab_spanner(spec_labels[["run3"]], columns = c(run3_coef_fmt, run3_t_fmt)) %>%
  cols_label(
    sector        = "Sector",
    run1_coef_fmt = "Coef.", run1_t_fmt = "|t|",
    run2_coef_fmt = "Coef.", run2_t_fmt = "|t|",
    run3_coef_fmt = "Coef.", run3_t_fmt = "|t|"
  ) %>%
  cols_align("left",  columns = sector) %>%
  cols_align("right", columns = -sector) %>%
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = list(cells_column_labels(), cells_column_spanners())
  ) %>%
  tab_source_note(md(
    "*Notes:* Dependent variable is bilateral trade; each cell is the FTA
     dummy coefficient from a separate PPML regression for that sector.
     Absolute t-statistics in parentheses.
     *** p < 0.01, ** p < 0.05, * p < 0.10."
  )) %>%
  tab_options(
    table.font.names             = c("Times New Roman", "serif"),
    table.font.size              = px(13),
    data_row.padding             = px(4),
    table_body.hlines.style      = "none",          # booktabs: no inner rules
    table.border.top.width       = px(2),
    table.border.bottom.width    = px(2),
    column_labels.border.top.width    = px(2),
    column_labels.border.bottom.width = px(1),
    heading.border.bottom.style  = "none",
    table.border.left.style      = "none",
    table.border.right.style     = "none",
    source_notes.font.size       = px(11)
  )

tbl


## ============================================================================
## ============================================================================
## SECTION 5 -- EXTENDED FOUR-SPECIFICATION TABLE   [ADDED — extension]
## ============================================================================
## Purpose
##   (a) add the fourth run -- Run 4: ETWFE + bilateral fixed effect -- to the
##       comparison; and
##   (b) rebuild the comparison in a full publication-style layout:
##         * ONE column per specification (not a coef/|t| pair);
##         * each cell shows the coefficient with significance stars and the
##           clustered standard error in parentheses beneath it (the classic
##           two-line regression-table cell);
##         * a "Specification" panel of Yes/No rows making the fixed-effect and
##           control choices explicit; and
##         * a "Summary" panel with the share of sectors that are positive and
##           significant at each level.
##
## This section is SELF-CONTAINED: it reads the four saved results CSVs, so it
## does not depend on any object built earlier in the file. Edit `results_dir`
## if your results live elsewhere.
## ============================================================================

library(gt); library(dplyr); library(tidyr); library(readr); library(stringr); library(purrr)

## ---- 5.1  Locate and load the four results tables --------------------------
## Each results_run*.csv has, per sector: gtap23, FTA_coef, FTA_SE, t_stat.
results_dir <- "../results"   # <- relative to FTA_GTAP23/scripts/ ; edit if needed

run_files <- c(
  run1 = "results_run1_base.csv",        # (1) Fontagne baseline (TWFE FTA dummy)
  run2 = "results_run2_etwfe.csv",       # (2) ETWFE staggered DiD
  run3 = "results_run3_pairfe.csv",      # (3) Bilateral (pair) fixed effect
  run4 = "results_run4_etwfe_pairfe.csv" # (4) ETWFE + bilateral fixed effect  [NEW]
)

read_run <- function(tag, file) {
  read_csv(file.path(results_dir, file), show_col_types = FALSE) |>
    transmute(gtap23,
              !!paste0(tag, "_coef") := FTA_coef,
              !!paste0(tag, "_se")   := FTA_SE,
              !!paste0(tag, "_t")    := t_stat)
}
sector_comparison4 <- imap(run_files, ~ read_run(.y, .x)) |>
  reduce(full_join, by = "gtap23")

## keep a plain CSV of the 4-run comparison alongside the others
write_csv(sector_comparison4, file.path(results_dir, "COMBINED_comparison_4runs.csv"))

## ---- 5.2  Labels (sector names reused; add the 4th specification) ----------
spec_labels4 <- c(
  run1 = "(1) Baseline",
  run2 = "(2) ETWFE",
  run3 = "(3) Bilateral FE",
  run4 = "(4) ETWFE + Bilateral FE"
)
runs <- names(spec_labels4)

star <- function(t) case_when(is.na(t) ~ "", abs(t) >= 2.576 ~ "***",
                              abs(t) >= 1.960 ~ "**", abs(t) >= 1.645 ~ "*", TRUE ~ "")

## ---- 5.3  PANEL A -- coefficient / (standard error) cells, one per spec -----
## Cell text = "coef*** <br> (se)".  <br> renders as a line break via fmt_markdown.
cell <- function(coef, se, t)
  ifelse(is.na(coef), "",
         paste0(formatC(coef, format = "f", digits = 3), star(t),
                "<br>(", formatC(se, format = "f", digits = 3), ")"))

panelA <- sector_comparison4 |>
  mutate(across(everything(), ~ .x)) |>
  transmute(
    gtap23,
    run1 = cell(run1_coef, run1_se, run1_t),
    run2 = cell(run2_coef, run2_se, run2_t),
    run3 = cell(run3_coef, run3_se, run3_t),
    run4 = cell(run4_coef, run4_se, run4_t)) |>
  mutate(rowlab = coalesce(unname(sector_labels[gtap23]), gtap23)) |>
  arrange(desc(sector_comparison4$run1_coef)) |>   # order by the baseline effect
  transmute(panel = "A. FTA effect by GTAP-23 sector", rowlab, run1, run2, run3, run4)

## ---- 5.4  PANEL B -- the specification map (what each column actually is) ---
panelB <- tribble(
  ~rowlab,                          ~run1, ~run2, ~run3, ~run4,
  "ETWFE (staggered DiD)",          "No",  "Yes", "No",  "Yes",
  "Gravity controls",               "Yes", "Yes", "No",  "No",
  "Bilateral (pair) fixed effect",  "No",  "No",  "Yes", "Yes",
  "Exporter x year x HS6 FE",       "Yes", "Yes", "Yes", "Yes",
  "Importer x year x HS6 FE",       "Yes", "Yes", "Yes", "Yes",
  "Tariff control",                 "Yes", "Yes", "Yes", "Yes"
) |> mutate(panel = "B. Specification")

## ---- 5.5  PANEL C -- share of sectors positive & significant ---------------
share_pos_sig <- function(tag, z) {
  co <- sector_comparison4[[paste0(tag, "_coef")]]
  tt <- sector_comparison4[[paste0(tag, "_t")]]
  sprintf("%.1f%%", 100 * mean(co > 0 & abs(tt) >= z, na.rm = TRUE))
}
panelC <- tibble(
  panel  = "C. Share of sectors positive and significant",
  rowlab = c("... at 1%", "... at 5%", "... at 10%"),
  run1 = c(share_pos_sig("run1",2.576), share_pos_sig("run1",1.960), share_pos_sig("run1",1.645)),
  run2 = c(share_pos_sig("run2",2.576), share_pos_sig("run2",1.960), share_pos_sig("run2",1.645)),
  run3 = c(share_pos_sig("run3",2.576), share_pos_sig("run3",1.960), share_pos_sig("run3",1.645)),
  run4 = c(share_pos_sig("run4",2.576), share_pos_sig("run4",1.960), share_pos_sig("run4",1.645)))

body4 <- bind_rows(panelA, panelB, panelC)

## ---- 5.6  BUILD THE PUBLICATION TABLE --------------------------------------
tbl4 <- body4 |>
  gt(groupname_col = "panel", rowname_col = "rowlab") |>
  tab_header(
    title    = md("**Table&nbsp;X. The effect of Free Trade Agreements on trade, by GTAP-23 sector**"),
    subtitle = md("*Product-level PPML gravity estimates (HS6 pooled to GTAP-23); dependent variable is bilateral trade*")) |>
  fmt_markdown(columns = all_of(runs)) |>
  cols_label(.list = setNames(as.list(unname(spec_labels4)), runs)) |>
  cols_align("center", columns = all_of(runs)) |>
  cols_align("left", columns = "rowlab") |>
  ## booktabs-style presentation
  tab_style(style = cell_text(weight = "bold"),
            locations = list(cells_column_labels(), cells_row_groups())) |>
  tab_style(style = cell_text(style = "italic"),
            locations = cells_row_groups()) |>
  tab_source_note(md(
    "*Notes:* Each column is a separate specification; each Panel-A cell is the
     average FTA effect for that sector, estimated by Poisson PML on the pooled
     HS6 products of the sector. Runs (2) and (4) report the ETWFE aggregate of
     the cohort-year treatment effects (Wooldridge 2021; Nagengast & Yotov 2023);
     runs (1) and (3) report a single FTA dummy. Coefficients are semi-elasticities
     (a value of x implies a trade change of exp(x)-1). Clustered (country-pair)
     standard errors in parentheses. *** p<0.01, ** p<0.05, * p<0.10.")) |>
  tab_options(
    table.font.names                  = c("Times New Roman", "serif"),
    table.font.size                   = px(13),
    data_row.padding                  = px(3),
    row_group.padding                 = px(6),
    table_body.hlines.style           = "none",
    table.border.top.width            = px(2),
    table.border.bottom.width         = px(2),
    column_labels.border.top.width    = px(2),
    column_labels.border.bottom.width = px(1),
    row_group.border.top.style        = "none",
    row_group.border.bottom.style     = "none",
    table.border.left.style           = "none",
    table.border.right.style          = "none",
    source_notes.font.size            = px(11),
    heading.border.bottom.style       = "none")

tbl4

## ---- 5.7  Save the table (HTML always; LaTeX for a paper appendix) ---------
gtsave(tbl4, file.path(results_dir, "Table_FTA_by_GTAP23_4specs.html"))
tryCatch(
  writeLines(as_latex(tbl4) |> as.character(),
             file.path(results_dir, "Table_FTA_by_GTAP23_4specs.tex")),
  error = function(e) message("LaTeX export skipped: ", conditionMessage(e)))

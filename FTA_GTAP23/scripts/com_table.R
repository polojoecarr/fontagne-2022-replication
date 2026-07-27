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

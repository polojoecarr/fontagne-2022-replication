## ============================================================================
## Build the SECOND matched-FTA summary table -- the regression_data_3 table --
## from whichever stages are complete. Re-run after each stage; it auto-detects
## the stage result CSVs present. THREE specifications only: (1), (3), (4);
## specification (2) (ETWFE + gravity controls) is deliberately not estimated.
## Outputs are named *_matched2_* so they never collide with the first table.
## ============================================================================
user_lib <- Sys.getenv("R_LIBS_USER"); if (nzchar(user_lib)) .libPaths(c(user_lib, .libPaths()))
suppressMessages({library(gt); library(dplyr); library(tidyr); library(readr); library(stringr); library(purrr)})

results_dir <- "C:/Claude Code Project Folder/Fontange 2022/R_Replication/FTA_matched2_GTAP23/results"

## stage metadata: file, column label, and specification flags for Panel B
stage_meta <- tribble(
  ~tag,   ~file,                            ~label,                     ~etwfe,~gravity,~pairfe,
  "s1",   "results_stage1_base.csv",        "(1) Baseline",             "No",  "Yes",   "No",
  "s3",   "results_stage3_pairfe.csv",      "(3) Bilateral FE",         "No",  "No",    "Yes",
  "s4",   "results_stage4_etwfe_pairfe.csv","(4) ETWFE + Bilateral FE", "Yes", "No",    "Yes")
stage_meta <- stage_meta |> filter(file.exists(file.path(results_dir, file)))   # only completed stages
tags <- stage_meta$tag
stopifnot(length(tags) >= 1)

sector_labels <- c(
  aff="Agriculture, forestry & fishing (agri)", b_t="Beverages & tobacco (agri)",
  ofd="Food products nec (agri)", prf="Meat & processed foods (agri)",
  crp="Chemicals, rubber & plastics", eeq="Electronic equipment",
  enr="Energy & extraction", mff="Metal & other materials",
  mvh="Motor vehicles & parts", ome="Machinery & equipment nec",
  omf="Manufactures nec", otn="Transport equipment nec",
  ppp="Paper products & publishing", tal="Textiles, apparel & leather")

## read each stage's matched coefficient + SE (uniform schema across stages)
read_stage <- function(tag, file) {
  read_csv(file.path(results_dir, file), show_col_types = FALSE) |>
    transmute(gtap23,
              !!paste0(tag, "_coef") := matched_coef,
              !!paste0(tag, "_se")   := matched_SE,
              !!paste0(tag, "_t")    := abs(matched_coef / matched_SE))
}
sc <- map2(stage_meta$tag, stage_meta$file, read_stage) |> reduce(full_join, by = "gtap23")
write_csv(sc, file.path(results_dir, "COMBINED_matched2_comparison.csv"))

star <- function(t) case_when(is.na(t)~"", abs(t)>=2.576~"***", abs(t)>=1.960~"**", abs(t)>=1.645~"*", TRUE~"")
cell <- function(coef, se, t) ifelse(is.na(coef), "",
  paste0(formatC(coef, format="f", digits=3), star(t), "<br>(", formatC(se, format="f", digits=3), ")"))

## Panel A: one cell (coef over SE) per completed stage
panelA <- sc |> mutate(rowlab = coalesce(unname(sector_labels[gtap23]), gtap23))
for (tg in tags) panelA[[tg]] <- cell(panelA[[paste0(tg,"_coef")]], panelA[[paste0(tg,"_se")]], panelA[[paste0(tg,"_t")]])
panelA <- panelA |> arrange(desc(.data[[paste0(tags[1],"_coef")]])) |>
  transmute(panel = "A. Matched-FTA effect by GTAP-23 sector", rowlab, !!!syms(tags))

## Panel B: specification map (only for the completed stages)
panelB <- bind_rows(
  tibble(rowlab="ETWFE (staggered DiD)",          !!!setNames(as.list(stage_meta$etwfe),   tags)),
  tibble(rowlab="Gravity controls",               !!!setNames(as.list(stage_meta$gravity), tags)),
  tibble(rowlab="Bilateral (pair) fixed effect",  !!!setNames(as.list(stage_meta$pairfe),  tags)),
  tibble(rowlab="Other-FTA control dummy",        !!!setNames(as.list(rep("Yes", length(tags))), tags)),
  tibble(rowlab="Exporter/importer x year x HS6 FE", !!!setNames(as.list(rep("Yes", length(tags))), tags))
) |> mutate(panel = "B. Specification")

## Panel C: share of sectors positive & significant
sps <- function(tag, z){ co<-sc[[paste0(tag,"_coef")]]; tt<-sc[[paste0(tag,"_t")]]
  sprintf("%.1f%%", 100*mean(co>0 & abs(tt)>=z, na.rm=TRUE)) }
panelC <- bind_rows(lapply(c("1%"=2.576,"5%"=1.960,"10%"=1.645) |> (\(x) split(unname(x), names(x)))(), \(z) NULL))
panelC <- tibble(rowlab=c("... at 1%","... at 5%","... at 10%"),
                 !!!setNames(lapply(tags, function(tg) sapply(c(2.576,1.960,1.645), \(z) sps(tg,z))), tags)) |>
  mutate(panel="C. Share of sectors positive and significant")

body <- bind_rows(panelA, panelB, panelC)
tbl <- body |> gt(groupname_col="panel", rowname_col="rowlab") |>
  tab_header(title = md("**The effect of *matched* FTAs on trade, by GTAP-23 sector**"),
             subtitle = md("*Narrow matched definition (regression_data_3); PPML gravity (HS6 pooled to GTAP-23); matched-FTA treatment, other-FTA control; dep. var. = bilateral trade*")) |>
  fmt_markdown(columns = all_of(tags)) |>
  cols_label(.list = setNames(as.list(stage_meta$label), tags)) |>
  cols_align("center", columns = all_of(tags)) |> cols_align("left", columns = "rowlab") |>
  tab_style(cell_text(weight="bold"), locations=list(cells_column_labels(), cells_row_groups())) |>
  tab_style(cell_text(style="italic"), locations=cells_row_groups()) |>
  tab_source_note(md("*Notes:* Each column is a separate specification; each Panel-A cell is the average effect of a **matched** FTA for that sector (Poisson PML on the pooled HS6 products), with the **other** (unmatched) FTA group entering as a time-varying control and no-FTA pairs as the baseline. Agri sectors use the agriculture FTA split, the rest use the goods split. Coefficients are semi-elasticities; clustered (country-pair) SEs in parentheses. *** p<0.01, ** p<0.05, * p<0.10.")) |>
  tab_options(table.font.names=c("Times New Roman","serif"), table.font.size=px(13),
              data_row.padding=px(3), table_body.hlines.style="none",
              table.border.top.width=px(2), table.border.bottom.width=px(2),
              column_labels.border.top.width=px(2), column_labels.border.bottom.width=px(1),
              source_notes.font.size=px(11), heading.border.bottom.style="none")

gtsave(tbl, file.path(results_dir, "Table_matched2_FTA_by_GTAP23.html"))
tryCatch(writeLines(as.character(as_latex(tbl)), file.path(results_dir, "Table_matched2_FTA_by_GTAP23.tex")),
         error=function(e) message("LaTeX skipped: ", conditionMessage(e)))

cat("\n===== MATCHED-FTA COMPARISON (", length(tags), "stage(s) ) =====\n")
disp <- sc |> mutate(sector=coalesce(unname(sector_labels[gtap23]),gtap23)) |> arrange(desc(.data[[paste0(tags[1],"_coef")]]))
for (tg in tags) disp[[stage_meta$label[match(tg,tags)]]] <- sprintf("%.3f%s", disp[[paste0(tg,"_coef")]], star(disp[[paste0(tg,"_t")]]))
print(as.data.frame(disp |> select(sector, all_of(stage_meta$label))), row.names=FALSE)
cat("\nOutputs: COMBINED_matched2_comparison.csv, Table_matched2_FTA_by_GTAP23.html/.tex\n")

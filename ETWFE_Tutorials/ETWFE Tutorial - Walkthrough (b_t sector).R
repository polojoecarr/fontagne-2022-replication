## ############################################################################
## ETWFE TUTORIAL -- A LINE-BY-LINE WALKTHROUGH OF THE STAGGERED DiD RUN
## ############################################################################
##
## PURPOSE
##   This is a teaching version of "Run 2 - FTA GTAP23 - ETWFE Staggered DiD.R".
##   It does the SAME thing, for ONE sector only, but broken into small steps
##   you can run one line at a time in RStudio (Ctrl+Enter) and inspect as you
##   go. After most steps there is a hint like:  ##> INSPECT: ...
##   telling you what object to look at (print it, or View() it) to see what
##   just happened.
##
## THE SECTOR
##   We use "b_t" = Beverages & Tobacco. Two reasons:
##     (i)  it is small (32 HS6 products) so every step runs in seconds; and
##     (ii) it is the sector where the answer FLIPS SIGN between the naive
##          method and the ETWFE -- which is the whole point of the exercise.
##          (Naive FTA dummy: +0.29 and "significant". ETWFE: -0.19.)
##       See the companion note "Forbidden Comparisons - Explainer.md" for WHY.
##
## THE THEORY THIS IMPLEMENTS  (Nagengast & Yotov 2023; Wooldridge 2021, 2023)
##   * Start from the Fontagne (2022) product-level gravity model, estimated by
##     PPML with fixed effects kept at the HS6 level.
##   * Replace the single "is there an FTA?" dummy with a full set of
##     cohort-by-year treatment terms delta_gs  (paper's Equation 3).
##   * A "cohort" g = the wave in which a country pair first signs an FTA.
##   * Compare each cohort to a CLEAN control (never-treated pairs + each
##     cohort's OWN pre-treatment years) -- never to already-treated pairs.
##   * Aggregate the delta_gs back to one number with the paper's weights
##     (Equation 7), and get its standard error by the delta method.
##
## Structure mirrors the main script: SECTIONS 1-5.
## ############################################################################


## ============================================================================
## SECTION 1 -- PACKAGES AND PATHS
## ============================================================================
user_lib <- Sys.getenv("R_LIBS_USER"); if (nzchar(user_lib)) .libPaths(c(user_lib, .libPaths()))
library(data.table)   # fast data handling
library(fixest)       # PPML with high-dimensional fixed effects + the i() helper
library(readxl)       # read the DTA workbook
library(dplyr); library(readr); library(stringr)

project_root <- "C:/Claude Code Project Folder/Fontange 2022"
sigma_dir    <- file.path(project_root, "Replic_FGO", "Replic_FGO")   # per-HS6 trade+tariff files
etwfe_dir    <- file.path(project_root, "R_Replication", "ETWFE")     # concordances + DTA data
waves        <- c(2001, 2004, 2007, 2010, 2013, 2016)                 # the six years we observe

SECTOR <- "b_t"   # <- change this to walk through a different sector


## ============================================================================
## SECTION 2 -- INPUTS: WHICH HS6 PRODUCTS, AND WHO HAS AN FTA WHEN
## ============================================================================

## ---- 2a. HS6 -> GTAP23, and pick out our sector's products ----------------
hs6_to_g65 <- read_csv(file.path(etwfe_dir, "GTAP to HS6 (1).csv"), show_col_types = FALSE) |>
  transmute(hs6 = str_pad(as.character(Code), 6, pad = "0"),
            gtap65 = tolower(trimws(GSEC3_rev_lower_case)))
g65_to_g23 <- read_csv(file.path(etwfe_dir, "GTAP 65 to GTAP 23 Concordence (1).csv"), show_col_types = FALSE) |>
  transmute(gtap65 = tolower(trimws(`Full Disaggregation`)), gtap23 = tolower(trimws(GTAP23))) |>
  filter(!is.na(gtap65))
hs6_gtap23 <- hs6_to_g65 |> left_join(g65_to_g23, by = "gtap65") |>
  filter(!is.na(gtap23)) |> distinct(hs6, gtap23)

hs6_list <- hs6_gtap23$hs6[hs6_gtap23$gtap23 == SECTOR]
length(hs6_list)
##> INSPECT: length(hs6_list) -- how many HS6 products we are about to pool (32 for b_t).

## ---- 2b. The FTA data, and each pair's TREATMENT COHORT --------------------
## An FTA here = a reciprocal goods agreement in the Deep Trade Agreements data.
fta_types <- c("FTA", "FTA & EIA", "CU", "CU & EIA")
bilateral <- read_excel(file.path(etwfe_dir, "DTA 2.0 - Vertical Content (v2).xlsx"),
                        sheet = "Bilateral Information") |>
  filter(type %in% fta_types) |>
  transmute(i = toupper(trimws(iso1)), j = toupper(trimws(iso2)), year = as.integer(year))

## make it symmetric (an FTA between i and j counts in both directions)
fta_active <- bind_rows(bilateral, rename(bilateral, i = j, j = i)) |>
  distinct(i, j, year) |> filter(year %in% waves)

## COHORT = the FIRST wave in which a pair has an FTA. This single number is
## what makes the design "staggered": different pairs switch on in different
## years. Pairs never seen with an FTA get NO cohort here -> they are the
## never-treated controls.
fta_cohort <- fta_active |> group_by(i, j) |>
  summarise(cohort = min(year), .groups = "drop") |> as.data.table()

table(fta_cohort$cohort)
##> INSPECT: table(fta_cohort$cohort) -- how many pairs first sign in each wave.
##          This is the staggered adoption pattern the ETWFE is built for.


## ============================================================================
## SECTION 3 -- BUILD THE SECTOR DATASET, STEP BY STEP
## ============================================================================

## ---- STEP 3.1  Read and stack the sector's HS6 files ----------------------
## Each Sigma_HS6_*.csv is one product: bilateral trade value (v), applied
## tariff (ADV), distance (DISTW) and the gravity dummies, for i x j x year.
read_one <- function(h) {
  f <- file.path(sigma_dir, paste0("Sigma_HS6_", h, ".csv"))
  d <- tryCatch(fread(f, sep = ";"), error = function(e) NULL)
  if (is.null(d) || nrow(d) == 0L) return(NULL)
  d[, hs6 := h]; d
}
dt <- rbindlist(lapply(hs6_list, read_one), use.names = TRUE, fill = TRUE)
dim(dt); head(dt)
##> INSPECT: head(dt) and dim(dt). One row = one exporter(i)-importer(j)-product(hs6)-year cell.

## ---- STEP 3.2  Attach each pair's treatment cohort ------------------------
dt <- merge(dt, fta_cohort, by = c("i", "j"), all.x = TRUE)   # cohort = NA means never-treated
dt[, .N, by = is.na(cohort)]
##> INSPECT: how many cells belong to never-treated pairs (cohort NA) vs treated pairs.

## ---- STEP 3.3  The Fontagne cleaning steps --------------------------------
dt[is.na(v), v := 0]                    # a missing trade cell is a genuine zero (PPML uses zeros)
dt[, ln_tariff := log(ADV + 1)]         # ln(1 + tariff): the trade-cost regressor
dt[, l_distw   := log(DISTW)]           # ln(distance): a gravity control
dt[, flag := sum(v), by = .(i, hs6)]    # an exporter that never ships a product carries no info...
dt <- dt[flag != 0]                     # ...so drop those exporter-product panels
dim(dt)
##> INSPECT: dim(dt) -- the working sample after cleaning.

## ---- STEP 3.4  Drop ALWAYS-TREATED pairs ----------------------------------
## A pair already treated in the very first wave (cohort == 2001) has NO
## pre-treatment period we can observe -> there is no "before" to compare its
## "after" against. We remove these. (Never-treated pairs stay: they are
## controls. Pairs first treated in 2004..2016 stay: they are our cohorts.)
##
## First keep a copy of the FULL sample (always-treated still in): we use it in
## Section 5 to show that DROPPING these pairs is itself part of why the answer
## moves, separately from the ETWFE saturation.
dt_full <- copy(dt)
dt <- dt[is.na(cohort) | cohort != waves[1]]
sort(unique(dt$cohort))
##> INSPECT: the surviving cohorts (2004, 2007, 2010, 2013, 2016) -- plus NA controls.

## ---- STEP 3.5  *** THE COHORT-SPECIFIC PRE-PERIOD BASELINE *** -------------
## This is the single most important idea in the whole script.
##
## We create ONE label per observation, `treat_cohort_year`:
##   * If a pair is TREATED and we are AT or AFTER its cohort year
##     (year >= cohort), the observation gets its OWN label "g<cohort>_y<year>".
##     Each such label becomes one treatment coefficient, delta_gs.
##   * EVERYTHING ELSE goes into a single reference bucket "0_baseline":
##        - never-treated pairs, AND
##        - treated pairs in their OWN pre-treatment years (year < cohort).
##
## Because a cohort's own pre-years sit in the baseline, each cohort is
## effectively measured against ITS OWN "before" (a cohort-specific pre-period
## baseline), together with the never-treated pairs. It is NEVER measured
## against another, already-treated cohort. That is what rules out the
## "forbidden comparisons" that bias the naive dummy.
dt[, treat_cohort_year := "0_baseline"]
dt[!is.na(cohort) & year >= cohort,
   treat_cohort_year := paste0("g", cohort, "_y", year)]
dt[, treat_cohort_year := relevel(factor(treat_cohort_year), ref = "0_baseline")]

## SEE THE DESIGN AS A GRID: rows = cohort, columns = year, cell = the label.
## Read across a row: the pre-treatment years read "0_baseline", then the
## treated years switch to that cohort's own g_y cells. That staircase IS the
## staggered design.
dcast(unique(dt[, .(cohort, year, treat_cohort_year)]), cohort ~ year,
      value.var = "treat_cohort_year")
##> INSPECT: the grid above. The lower-left triangle is baseline (pre-treatment
##          + never-treated); the upper-right staircase is the treated cells.

## ---- STEP 3.6  How many observations sit in each treated cell? ------------
## We will need these counts as the aggregation WEIGHTS in Step 3.9.
dt[treat_cohort_year != "0_baseline", .N, by = treat_cohort_year][order(treat_cohort_year)]
##> INSPECT: N per cohort-year cell = the number of treated observations N_gs.


## ============================================================================
## SECTION 4 -- THE ETWFE REGRESSION AND ITS AGGREGATION
## ============================================================================

## ---- STEP 4.1  Run the saturated PPML regression --------------------------
## i(treat_cohort_year, ref="0_baseline") expands into one dummy per treated
## cohort-year cell. Everything else is the ordinary Fontagne gravity model:
##   * ln_tariff + the four gravity controls (distance, colony, contiguity, language)
##   * fixed effects kept at the HS6 level: exporter x year x HS6, importer x year x HS6
## Standard errors are clustered by country-pair (i^j).
m <- fepois(v ~ i(treat_cohort_year, ref = "0_baseline") +
              ln_tariff + l_distw + COLONY + CONTIG + COMLANG_OFF |
              i^year^hs6 + j^year^hs6,
            data = dt, cluster = ~ i^j, warn = FALSE, notes = FALSE)
summary(m)
##> INSPECT: summary(m). Each "treat_cohort_year::g2004_y2004" line is a
##          cohort-year treatment effect delta_gs (a log-change in trade).

## ---- STEP 4.2  Pull out the cohort-year treatment effects -----------------
cf <- coef(m)                                        # all coefficients
V  <- vcov(m)                                        # their variance-covariance matrix
cells <- grep("^treat_cohort_year::", names(cf), value = TRUE)   # the delta_gs terms
cells <- cells[!grepl("0_baseline", cells) & !is.na(cf[cells])]  # drop reference / dropped cells
data.frame(cell = sub("^treat_cohort_year::", "", cells), delta_gs = round(cf[cells], 3))
##> INSPECT: the table of delta_gs. THESE are the clean, heterogeneity-robust
##          building blocks (paper's Equation 3). Note they are NOT all the
##          same -- the FTA effect differs by cohort and by year since signing.

## ---- STEP 4.3  Build the aggregation weights N_gs / N_D -------------------
## The paper's headline number (Equation 7) is a weighted average of the
## delta_gs, weighting each cell by its share of ALL treated observations.
levs <- sub("^treat_cohort_year::", "", cells)
N_gs <- vapply(levs, function(L) sum(dt$treat_cohort_year == L), numeric(1))   # treated obs per cell
w    <- N_gs / sum(N_gs)                                                        # weights, sum to 1
data.frame(cell = levs, N_gs = N_gs, weight = round(w, 3))
##> INSPECT: the weights. Bigger cells (more treated trade cells) count for more.

## ---- STEP 4.4  Aggregate to ONE number, with a delta-method SE ------------
delta_bar <- sum(w * cf[cells])                             # the aggregated FTA effect (Equation 7)
se_bar    <- sqrt(as.numeric(t(w) %*% V[cells, cells] %*% w))  # SE of a weighted sum (delta method)
t_bar     <- delta_bar / se_bar
c(ETWFE_effect = delta_bar, SE = se_bar, t = t_bar, pct_trade_change = exp(delta_bar) - 1)
##> INSPECT: the final ETWFE answer for this sector.


## ============================================================================
## SECTION 5 -- WHY THIS DIFFERS FROM THE NAIVE DUMMY (the punchline)
## ============================================================================
## The naive method (Run 1) replaces all the cohort-year terms with a SINGLE
## dummy: FTA_ = 1 whenever a pair currently has an FTA. Let us reproduce it and
## see where the ETWFE answer comes from -- built up in TWO honest steps.

## A helper that runs the naive single-dummy model on whatever sample we give it
naive_fit <- function(d) {
  d[, FTA_ := as.integer(!is.na(cohort) & year >= cohort)]
  mm <- fepois(v ~ FTA_ + ln_tariff + l_distw + COLONY + CONTIG + COMLANG_OFF |
                 i^year^hs6 + j^year^hs6, data = d, cluster = ~ i^j, warn = FALSE, notes = FALSE)
  summary(mm)$coeftable["FTA_", c("Estimate", "Std. Error")]
}

n_full    <- naive_fit(dt_full)   # (1) naive dummy, FULL sample  -> this is Run 1
n_matched <- naive_fit(dt)        # (2) naive dummy, ETWFE sample (always-treated removed)
## (3) is the ETWFE aggregate we already computed above: delta_bar / se_bar

cat("\n-------------------------------------------------------------------\n")
cat(sprintf("(1) Naive dummy, FULL sample          : %+.3f  (SE %.3f)\n", n_full[1], n_full[2]))
cat(sprintf("(2) Naive dummy, always-treated dropped: %+.3f  (SE %.3f)\n", n_matched[1], n_matched[2]))
cat(sprintf("(3) ETWFE aggregate (same sample as 2) : %+.3f  (SE %.3f)\n", delta_bar, se_bar))
cat("-------------------------------------------------------------------\n")
##
##> INSPECT and read the story for b_t:
##
##   (1) The naive dummy on the full sample says FTAs RAISE beverage/tobacco
##       trade (+0.29, "significant"). This is the number in Run 1.
##
##   (1)->(2)  Removing the ALWAYS-TREATED pairs alone drags it negative
##       (about -0.10). Those pairs (big, long-standing agreements like the EU)
##       have no clean "before" in our window, yet the naive dummy still used
##       them -- an unavoidable forbidden comparison. Take them out and the
##       apparent positive effect largely disappears.
##
##   (2)->(3)  Among the remaining STAGGERED cohorts, the naive dummy still
##       quietly uses EARLIER-treated pairs as controls for LATER-treated pairs.
##       The ETWFE's cohort-specific baseline (Step 3.5) forbids that, and the
##       estimate moves further to -0.19.
##
##   Bottom line: the heterogeneity-robust answer for b_t is NEGATIVE -- the
##   opposite of the naive headline. Both steps are the same underlying disease
##   ("forbidden comparisons"); the ETWFE is the cure.
##
##   To see EXACTLY how a forbidden comparison flips a sign -- with worked
##   numbers for the two cases (effects that grow over time, and effects that
##   differ across cohorts) -- read the companion note:
##       "Forbidden Comparisons - Explainer.md"

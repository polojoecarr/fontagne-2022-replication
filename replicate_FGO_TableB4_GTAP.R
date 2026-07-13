###############################################################################
##                                                                           ##
##   EXTENSION: REPLICATION OF TABLE B4                                       ##
##   "The trade elasticity by GTAP revision 10 sectors"                       ##
##                                                                           ##
##   Fontagne, Guimbard & Orefice, "Tariff-Based Product-Level Trade          ##
##   Elasticities", Journal of International Economics (2022), Online          ##
##   Appendix Table B4.                                                       ##
##                                                                           ##
##   ----------------------------------------------------------------------  ##
##   WHAT TABLE B4 IS                                                         ##
##   ----------------------------------------------------------------------  ##
##   Instead of one elasticity per HS6 product (Table 5), Table B4 reports    ##
##   one elasticity per GTAP (rev. 10) SECTOR. Each sector's elasticity is    ##
##   obtained by POOLING all the HS6 products that belong to that sector and  ##
##   estimating a SINGLE tariff coefficient for the pool.                     ##
##                                                                           ##
##   The specification is Equation (6) of the paper -- the pooled counterpart ##
##   of Equation (5). Crucially, the FIXED EFFECTS ARE KEPT AT THE HS6 LEVEL  ##
##   (paper, footnote 57: "we include both exporter-HS6-year and importer-    ##
##   HS6-year fixed effects to fully capture the multilateral resistance      ##
##   term"). This is exactly the authors' Stata call in GTAP_aggregation_v4.do:##
##       ppml_panel_sg v ln_tariff l_distw colony contig comlang_off,         ##
##            exporter(i) importer(j) year(year) industry(HS6) nopair robust  ##
##   where `industry(HS6)` interacts the exporter-year and importer-year      ##
##   fixed effects with the HS6 product. For GTAP sector g (pooling its HS6   ##
##   products k):                                                             ##
##                                                                           ##
##     X_ijk,t = exp[ a_ik,t + a_jk,t                                         ##
##                    + beta_g * ln(1+tariff_ijk,t)   <- ONE elasticity / sector##
##                    + gamma_g * ln(dist_ij) + delta_g * Z_ij ] * e          ##
##       a_ik,t = exporter x HS6 x year FE   (i^hs6^year)                     ##
##       a_jk,t = importer x HS6 x year FE   (j^hs6^year)                     ##
##                                                                           ##
##   The GTAP trade elasticity is  epsilon_g = 1 + beta_g.                    ##
##                                                                           ##
##   ----------------------------------------------------------------------  ##
##   *** PARALLELISATION NOTE -- DIFFERENT FROM THE EARLIER SCRIPTS ***       ##
##   ----------------------------------------------------------------------  ##
##   The Table 5 / headline scripts ran 5,050 TINY single-threaded            ##
##   regressions, so furrr (parallel across products) was the right tool.     ##
##   Here the situation is INVERTED: there are only 46 regressions, but each  ##
##   pools up to 770 HS6 products (~90 million rows, ~1.6 million fixed       ##
##   effects for Chemicals). A single such regression already needs ~10-15 GB ##
##   of RAM. Running several in parallel with furrr would hold several multi- ##
##   GB sectors in memory at once and exhaust it.                             ##
##                                                                           ##
##   We therefore DO NOT use furrr here. Instead we process the sectors       ##
##   SEQUENTIALLY and let fixest use ALL cpu cores INTERNALLY on each         ##
##   regression (setFixest_nthreads below). This bounds peak memory to the    ##
##   single largest sector while still using the whole machine.               ##
##                                                                           ##
###############################################################################


## ===========================================================================
## SECTION 1 -- ENVIRONMENT, PACKAGES AND PATHS
## ===========================================================================
user_lib <- Sys.getenv("R_LIBS_USER")
if (nzchar(user_lib)) .libPaths(c(user_lib, .libPaths()))

suppressMessages({
  library(data.table)   # fast CSV reading + in-memory pooling
  library(fixest)       # PPML with 3-way interacted fixed effects (fepois)
  library(readxl)       # read the GTAP concordance workbook
  library(dplyr); library(readr); library(stringr)
})

## Let fepois use every core INTERNALLY (see the parallelisation note above).
setFixest_nthreads(parallel::detectCores())

project_root <- "C:/Claude Code Project Folder/Fontange 2022"
data_dir     <- file.path(project_root, "Replic_FGO", "Replic_FGO")
concord_file <- file.path(project_root, "GTAP Concordence.xlsx")
out_dir      <- file.path(project_root, "R_Replication", "output_tableB4")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)


## ===========================================================================
## SECTION 2 -- THE HS6 -> GTAP (rev 10) CONCORDANCE
## ===========================================================================
## The workbook has one sheet per HS revision (H0=HS1992 ... H6=HS2022). The
## paper uses the "HS (rev 2007)" table, which is sheet H3. The GTAP rev-10
## sector code is the "GTAP 10" column (internally named GSEC3), e.g. CTL, OAP.
## Official GTAP codes carry underscores for a few sectors (B_T, C_B, I_S, P_C,
## V_F); Table B4 prints them without (bt, cb, is, pc, vf), so we strip "_" to
## make the codes line up.
concord <- read_excel(concord_file, sheet = "H3", skip = 1,
                      col_names = c("classif","hs6","desc_hs","hs4","gtap10","gtap11")) |>
  filter(!is.na(hs6), grepl("^[0-9]{5,6}$", as.character(hs6))) |>
  mutate(hs6  = str_pad(as.character(hs6), 6, pad = "0"),
         gtap = str_replace_all(toupper(trimws(gtap10)), "_", "")) |>
  distinct(hs6, gtap)

## The HS6 products we actually have (drop Monetary gold / Coins, as elsewhere).
all_files <- list.files(data_dir, pattern = "^Sigma_HS6_.*\\.csv$")
hs6_codes <- setdiff(str_match(all_files, "^Sigma_HS6_(.*)\\.csv$")[, 2],
                     c("710820", "711890"))

## Keep only products we can both locate and map, and list the GTAP sectors.
map <- concord |> filter(hs6 %in% hs6_codes)
gtap_sectors <- sort(unique(map$gtap))
cat("HS6 products mapped:", nrow(map),
    "| GTAP sectors to estimate:", length(gtap_sectors), "\n")


## ===========================================================================
## SECTION 3 -- ESTIMATE ONE GTAP SECTOR (pool its HS6 products)
## ===========================================================================
estimate_gtap_sector <- function(code) {

  hs6s <- map$hs6[map$gtap == code]
  files <- file.path(data_dir, paste0("Sigma_HS6_", hs6s, ".csv"))
  files <- files[file.exists(files)]

  ## --- read & pool all HS6 files of this sector -------------------------
  ## Read only the columns we need, to keep the (very large) pooled table as
  ## small as possible.
  parts <- lapply(seq_along(files), function(ix) {
    dt <- fread(files[ix], sep = ";",
                select = c("i","j","year","v","ADV","DISTW","COLONY","CONTIG","COMLANG_OFF"))
    dt[is.na(v), v := 0]                 # missing flow = zero flow
    dt[, hs6 := hs6s[ix]]                # tag the product (needed for the FE)
    dt
  })
  dt <- rbindlist(parts); rm(parts)

  ## --- transforms & sample cleaning (as in GTAP_aggregation_v4.do) ------
  dt[, ln_tariff := log(ADV + 1)]
  dt[, l_distw   := log(DISTW)]
  dt[, c("ADV", "DISTW") := NULL]        # free memory once logs are taken
  ## Drop exporters that never export a given HS6 -- now grouped by (i, HS6)
  ## because the pool spans several products (Stata: "bys i HS6: egen flag...").
  dt[, flag := sum(v), by = .(i, hs6)]
  dt <- dt[flag != 0]; dt[, flag := NULL]
  ## factors are lighter and faster for the high-dimensional fixed effects
  dt[, `:=`(i = as.factor(i), j = as.factor(j), hs6 = as.factor(hs6))]

  n_rows <- nrow(dt); n_prod <- length(files)

  ## --- pooled PPML: ONE tariff coefficient, HS6-level fixed effects ------
  m <- tryCatch(
    fepois(v ~ ln_tariff + l_distw + COLONY + CONTIG + COMLANG_OFF |
             i^hs6^year + j^hs6^year,
           data = dt, vcov = "hetero", warn = FALSE, notes = FALSE),
    error = function(e) NULL)
  rm(dt); gc()

  if (is.null(m) || !("ln_tariff" %in% names(coef(m))))
    return(data.table(gtap = code, n_hs6 = n_prod, n_rows = n_rows,
                      beta = NA_real_, se = NA_real_, dist = NA_real_, status = "failed"))

  ct <- summary(m)$coeftable
  data.table(gtap = code, n_hs6 = n_prod, n_rows = n_rows,
             beta = ct["ln_tariff", "Estimate"],
             se   = ct["ln_tariff", "Std. Error"],
             dist = if ("l_distw" %in% rownames(ct)) ct["l_distw","Estimate"] else NA_real_,
             status = "ok")
}


## ===========================================================================
## SECTION 4 -- RUN ALL 46 SECTORS SEQUENTIALLY  (fixest multithreaded)
## ===========================================================================
## Largest sectors first, so if memory is going to be a problem it surfaces
## immediately rather than 40 minutes in.
order_by_size <- map |> count(gtap, name = "n") |> arrange(desc(n)) |> pull(gtap)

t_start <- Sys.time(); res_list <- vector("list", length(order_by_size))
for (s in seq_along(order_by_size)) {
  code <- order_by_size[s]
  tt <- system.time(r <- estimate_gtap_sector(code))["elapsed"]
  res_list[[s]] <- r
  cat(sprintf("[%2d/%2d] %-4s  HS6=%3d  rows=%9d  beta=%8.2f  eps=%7.2f  (%4.0fs)\n",
              s, length(order_by_size), code, r$n_hs6, r$n_rows,
              r$beta, 1 + r$beta, tt))
  flush.console()
}
cat("\nAll GTAP sectors finished in",
    round(as.numeric(Sys.time() - t_start, units = "mins"), 1), "minutes.\n")

results <- rbindlist(res_list)


## ===========================================================================
## SECTION 5 -- TRADE ELASTICITIES + SIGNIFICANCE
## ===========================================================================
## Sign convention (as in the Stata): epsilon = 1 + beta. We keep EVERY sector
## (per the agreed reporting choice) and add a significance flag; the paper
## instead printed "NS" for sectors whose tariff coefficient is insignificant
## at the 1% level (t < 2.576) -- we show that side-by-side in SECTION 6.
z_crit_1pct <- 2.576
results <- results |>
  mutate(epsilon  = round(1 + beta, 2),
         t_stat   = round(abs(beta / se), 2),
         sig_1pct = t_stat > z_crit_1pct)

## Persist the raw per-sector output immediately (beta, se, epsilon, t-stat,
## significance) so the expensive 46-regression run is never lost to a
## downstream formatting error.
write_csv(results, file.path(out_dir, "gtap_sector_raw_results.csv"))


## ===========================================================================
## SECTION 6 -- ASSEMBLE TABLE B4 AND COMPARE TO THE PAPER
## ===========================================================================
## Published Table B4 values (code, description, elasticity; "NS" = not
## significant), transcribed from the JIE preprint p.47.
b4 <- tribble(
  ~gtap, ~description,                                                      ~paper,
  "OAP","Animal Products n.e.c.",                                           -4.27,
  "BT", "Beverages and Tobacco products",                                   -2.73,
  "CB", "Cane and Beet: sugar crops",                                       -2.33,
  "CTL","Cattle: bovine animals, live, other ruminants",                    -6.39,
  "CHM","Chemicals and chemical products",                                  -7.83,
  "COA","Coal: mining and agglomeration of hard coal",                        NA,
  "ELE","Computer, electronic and optical products",                       -5.26,
  "OCR","Crops n.e.c.",                                                     -2.87,
  "EEQ","Electrical equipment",                                            -4.62,
  "ELY","Electricity; steam and air conditioning supply",                  -9.48,
  "PFB","Fibres crops",                                                    -12.04,
  "FSH","Fishing and hunting (incl. related service activities)",          -6.65,
  "OFD","Food products n.e.c.",                                            -4.70,
  "FRS","Forestry: forestry, logging and related service activities",      -2.53,
  "GDT","Gas manufacture, distribution",                                      NA,
  "GAS","Gas: extraction of natural gas (incl. related activities)",          NA,
  "IS", "Iron and Steel: basic production and casting",                    -3.45,
  "LEA","Leather and related products",                                    -5.99,
  "OME","Machinery and equipment n.e.c.",                                  -4.23,
  "OMT","Meat products n.e.c",                                             -5.17,
  "CMT","Meat: fresh or chilled",                                          -4.04,
  "FMP","Metal products, except machinery and equipment",                  -4.25,
  "MIL","Milk and dairy products",                                         -4.77,
  "MVH","Motor vehicles, trailers and semi-trailers",                      -8.98,
  "NFM","Non-Ferrous Metals",                                             -13.39,
  "OSD","Oil Seeds: oil seeds and oleaginous fruit",                       -2.05,
  "OIL","Oil: extraction of crude petroleum (incl. related activities)",  -10.89,
  "GRO","Other Grains (maize, sorghum, barley, rye, oats, millets)",          NA,
  "OMF","Other Manufacturing (includes furniture)",                        -4.91,
  "OXT","Other Mining Extraction",                                         -8.28,
  "NMM","Other non-metallic mineral products",                             -4.83,
  "OTN","Other transport equipment",                                       -7.98,
  "PPP","Paper and Paper Products",                                        -8.18,
  "PC", "Petroleum and Coke",                                              -3.64,
  "BPH","Pharmaceuticals, medicinal chemical and botanical products",      -7.92,
  "PCR","Processed Rice: semi- or wholly milled, or husked",               -6.46,
  "PDR","Rice: seed, paddy (not husked)",                                  -7.63,
  "RPP","Rubber and plastics products",                                    -7.04,
  "SGR","Sugar and molasses",                                              -3.76,
  "TEX","Textiles",                                                        -6.04,
  "VOL","Vegetable Oils and fats",                                         -2.75,
  "VF", "Vegetables and Fruits (incl. nuts and edible roots)",             -4.02,
  "WAP","Wearing apparel",                                                 -3.84,
  "WHT","Wheat: seed, other",                                              -2.61,
  "LUM","Wood, products of wood, cork (except furniture) and straw",       -8.69,
  "WOL","Wool, silk, and other raw animal materials used in textile",      -7.28)

tableB4 <- b4 |>
  left_join(results |> select(gtap, epsilon, t_stat, sig_1pct, n_hs6), by = "gtap") |>
  mutate(paper_shown   = ifelse(is.na(paper), "NS", sprintf("%.2f", paper)),
         our_elasticity = epsilon,
         diff = ifelse(is.na(paper), NA_real_, round(epsilon - paper, 2))) |>
  select(gtap, description, n_hs6,
         our_elasticity, our_t = t_stat, our_sig_1pct = sig_1pct,
         paper_elasticity = paper_shown, diff)

write_csv(tableB4, file.path(out_dir, "TableB4_replication.csv"))

## Report
cat("\n================ TABLE B4 (replication vs paper) ================\n")
print(as.data.frame(tableB4), row.names = FALSE)

matched <- tableB4 |> filter(!is.na(diff))
cat(sprintf("\nSectors with a published (non-NS) value: %d\n", nrow(matched)))
cat(sprintf("Mean |difference| vs paper: %.3f  |  within 0.10: %d / %d  |  within 0.25: %d / %d\n",
            mean(abs(matched$diff)),
            sum(abs(matched$diff) <= 0.10), nrow(matched),
            sum(abs(matched$diff) <= 0.25), nrow(matched)))
cat("Paper's 4 'NS' sectors (COA, GDT, GAS, GRO) -- our point estimates & t-stats:\n")
print(as.data.frame(tableB4 |> filter(gtap %in% c("COA","GDT","GAS","GRO")) |>
        select(gtap, description, our_elasticity, our_t, our_sig_1pct)), row.names = FALSE)

cat("\nDONE. Outputs in:\n  ", out_dir, "\n")

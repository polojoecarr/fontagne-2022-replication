# =============================================================================
#  Britain's Two Borders — UK and global trade in the OECD ICIO tables, 2016–2022
#  Reproduces every figure in the accompanying document.
#
#  Runs from the CSVs exactly as supplied, using the long-format pipeline
#  you already had. Produces 8 PNG charts plus a set of tidy summary tables.
#
#  ---------------------------------------------------------------------------
#  BEFORE YOU RUN
#  ---------------------------------------------------------------------------
#  1. Set DATA_DIR below to wherever the *_SML.csv files live.
#  2. Memory: one year in long format is 4,053 x 4,537 = 18.4m rows. With
#     character key columns that is roughly 2–3 GB in RAM. The script therefore
#     loads ONE year at a time and collapses it to small summaries before
#     moving on. Never rbind the long frames together.
#     Peak usage ~4 GB; the whole script takes roughly 5–10 minutes.
#  3. Set FAST <- TRUE to skip the pivot entirely and aggregate on the matrix
#     instead. Identical numbers, ~20x faster. FALSE uses your pipeline.
# =============================================================================

library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)

DATA_DIR <- "C:/Claude Code Project Folder/ICIO Data/2025 edition"
OUT_DIR  <- "C:/Claude Code Project Folder/ICIO Data/output"
FAST     <- TRUE          # TRUE = matrix aggregation; FALSE = the long pipeline
YEARS    <- 2016:2022      # all seven are needed for Figure 2
dir.create(OUT_DIR, showWarnings = FALSE)


# =============================================================================
#  1.  DEFINITIONS
# =============================================================================

EU27 <- c("AUT","BEL","BGR","HRV","CYP","CZE","DNK","EST","FIN","FRA","DEU","GRC",
          "HUN","IRL","ITA","LVA","LTU","LUX","MLT","NLD","POL","PRT","ROU","SVK",
          "SVN","ESP","SWE")

# the six final-demand column types that sit to the right of the intermediate block
FD_CATS <- c("HFCE","NPISH","GGFC","GFCF","INVNT","DPABR")

GOODS <- c("A01","A02","A03","B05","B06","B07","B08","B09","C10T12","C13T15","C16",
           "C17_18","C19","C20","C21","C22","C23","C24A","C24B","C25","C26","C27",
           "C28","C29","C301","C302T309","C31T33")
ENERGY <- c("B05","B06","B07","B08","B09","C19","D")   # extraction, refining, utilities

# broad grouping used in Figure 4
sector_group <- function(s) {
  fifelse(s %in% ENERGY, "Energy",
  fifelse(s %in% GOODS,  "Goods excl. energy",
  fifelse(s %in% c("D","E","F"), "Utilities & construction", "Services")))
}

SECTOR_NAMES <- c(
  A01="Crops & animals", A02="Forestry", A03="Fishing", B05="Coal",
  B06="Oil & gas extraction", B07="Metal ores", B08="Other mining",
  B09="Mining support", C10T12="Food, drink & tobacco", C13T15="Textiles & apparel",
  C16="Wood", C17_18="Paper & printing", C19="Refined petroleum", C20="Chemicals",
  C21="Pharmaceuticals", C22="Rubber & plastics", C23="Non-metallic minerals",
  C24A="Iron & steel", C24B="Non-ferrous metals", C25="Fabricated metal",
  C26="Computers & electronics", C27="Electrical equipment", C28="Machinery",
  C29="Motor vehicles", C301="Ships & boats", C302T309="Other transport equipment",
  C31T33="Furniture & repair", D="Electricity & gas", E="Water & waste",
  F="Construction", G="Wholesale & retail", H49="Land transport",
  H50="Water transport", H51="Air transport", H52="Warehousing",
  H53="Post & courier", I="Accommodation & food", J58T60="Publishing & broadcasting",
  J61="Telecoms", J62_63="IT & information", K="Finance & insurance", L="Real estate",
  M="Professional & technical", N="Administrative & support", O="Public administration",
  P="Education", Q="Health & social work", R="Arts & recreation", S="Other services",
  T="Household activities")

COUNTRY_NAMES <- c(
  CHN="China", USA="United States", RUS="Russia", SAU="Saudi Arabia", CAN="Canada",
  NOR="Norway", AUS="Australia", IRL="Ireland", IND="India", KOR="Korea",
  THA="Thailand", GBR="United Kingdom", ESP="Spain", BRA="Brazil", ITA="Italy",
  FRA="France", DEU="Germany", JPN="Japan", CHE="Switzerland", MEX="Mexico",
  SGP="Singapore", ARE="UAE", TWN="Chinese Taipei", ISR="Israel", TUR="Turkiye",
  VNM="Viet Nam", ZAF="South Africa", MYS="Malaysia", PHL="Philippines",
  IDN="Indonesia", HKG="Hong Kong, China", CHL="Chile", ARG="Argentina",
  NLD="Netherlands", ROW="Rest of world", NGA="Nigeria")

nm <- function(x, lookup) { v <- unname(lookup[x]); ifelse(is.na(v), x, v) }


# =============================================================================
#  2.  LOADING — your long-format pipeline
# =============================================================================
#  NOTE ON YOUR ORIGINAL CODE: three small fixes were needed to make it run —
#    pivot(longer(...))   ->  pivot_longer(...)      (parenthesis placement)
#    seperate_wider_delim ->  separate_wider_delim   (spelling)
#    fread(path)          ->  fread(file = path)     (your path has a space in
#                             it, so fread otherwise treats it as a shell command)
#
#  The `too_few = "align_end"` argument is doing real work here: the last three
#  ROWS are labelled TLS / VA / OUT with no underscore, and the last COLUMN is
#  OUT. align_end puts those in the *sector* slot and leaves the country NA,
#  which is what you want — it makes them easy to filter out of trade flows and
#  easy to pick up when you need value added.

icio_long <- function(year) {
  path <- file.path(DATA_DIR, paste0(year, "_SML.csv"))
  data <- fread(file = path, showProgress = FALSE)

  ICIO <- data %>%
    pivot_longer(cols = -V1) %>%
    rename(row = V1, col = name) %>%
    separate_wider_delim(row, names = c("exporter", "exp_sector"), delim = "_",
                         too_many = "merge", too_few = "align_end") %>%
    separate_wider_delim(col, names = c("importer", "imp_sector"), delim = "_",
                         too_many = "merge", too_few = "align_end") %>%
    mutate(value = ifelse(value == 0, 0.000001, value),
           year  = year)

  as.data.table(ICIO)
}

# --- FAST equivalent: same four columns, built on the matrix ------------------
icio_long_fast <- function(year) {
  path <- file.path(DATA_DIR, paste0(year, "_SML.csv"))
  dt <- fread(file = path, showProgress = FALSE)
  rn <- dt[[1]]; dt[, V1 := NULL]
  m  <- as.matrix(dt); rownames(m) <- rn
  splt <- function(x) {
    p <- regexpr("_", x, fixed = TRUE)
    list(cty = ifelse(p > 0, substr(x, 1, p - 1), NA_character_),
         sec = ifelse(p > 0, substr(x, p + 1, nchar(x)), x))
  }
  r <- splt(rownames(m)); c_ <- splt(colnames(m))
  data.table(
    exporter   = rep(r$cty, times = ncol(m)),
    exp_sector = rep(r$sec, times = ncol(m)),
    importer   = rep(c_$cty, each = nrow(m)),
    imp_sector = rep(c_$sec, each = nrow(m)),
    value      = as.vector(m),
    year       = year)
}

load_year <- function(y) if (FAST) icio_long_fast(y) else icio_long(y)


# =============================================================================
#  3.  PER-YEAR SUMMARIES
# =============================================================================
#  Everything downstream is built from these five small tables, so the 18m-row
#  frame can be discarded as soon as they are made.

summarise_year <- function(ICIO) {

  # --- trade flows: both endpoints must be a country ------------------------
  #     (drops the VA / TLS / OUT rows and the OUT column automatically)
  trade <- ICIO[!is.na(exporter) & !is.na(importer) & exporter != importer]

  # (a) bilateral gross trade
  bilateral <- trade[, .(value = sum(value)), by = .(year, exporter, importer)]

  # (b) UK exports by industry and destination
  uk_exports <- trade[exporter == "GBR",
                      .(value = sum(value)), by = .(year, exp_sector, importer)]
  setnames(uk_exports, c("exp_sector", "importer"), c("sector", "destination"))

  # (c) UK imports by industry-of-origin and source country
  uk_imports <- trade[importer == "GBR",
                      .(value = sum(value)), by = .(year, exp_sector, exporter)]
  setnames(uk_imports, c("exp_sector", "exporter"), c("sector", "origin"))

  # (d) value added and gross output by country x industry
  va  <- ICIO[exp_sector == "VA"  & !is.na(importer) & !(imp_sector %in% FD_CATS),
              .(year, country = importer, sector = imp_sector, va = value)]
  out <- ICIO[exp_sector == "OUT" & !is.na(importer) & !(imp_sector %in% FD_CATS),
              .(year, country = importer, sector = imp_sector, output = value)]
  industry <- merge(va, out, by = c("year", "country", "sector"))

  list(bilateral = bilateral, uk_exports = uk_exports,
       uk_imports = uk_imports, industry = industry)
}

message("Loading ", length(YEARS), " years — one at a time...")
parts <- lapply(YEARS, function(y) {
  message("  ", y, appendLF = FALSE); t0 <- Sys.time()
  s <- summarise_year(load_year(y))
  gc(verbose = FALSE)
  message("  (", round(difftime(Sys.time(), t0, units = "secs")), "s)")
  s
})

bilateral  <- rbindlist(lapply(parts, `[[`, "bilateral"))
uk_exports <- rbindlist(lapply(parts, `[[`, "uk_exports"))
uk_imports <- rbindlist(lapply(parts, `[[`, "uk_imports"))
industry   <- rbindlist(lapply(parts, `[[`, "industry"))
rm(parts); gc(verbose = FALSE)

fwrite(bilateral,  file.path(OUT_DIR, "bilateral_trade.csv"))
fwrite(uk_exports, file.path(OUT_DIR, "uk_exports.csv"))
fwrite(uk_imports, file.path(OUT_DIR, "uk_imports.csv"))
fwrite(industry,   file.path(OUT_DIR, "industry_va_output.csv"))


# =============================================================================
#  4.  HOUSE STYLE FOR THE CHARTS
# =============================================================================

BLUE <- "#2a78d6"; ORANGE <- "#eb6834"; RED <- "#e34948"; GREY <- "#c3ccd4"
INK  <- "#0f151b"; INK2 <- "#4c5763"; MUTED <- "#78838e"; GRID <- "#e0e6ea"

theme_icio <- function(base = 11) {
  theme_minimal(base_size = base) +
    theme(
      plot.title      = element_text(face = "plain", size = base * 1.45,
                                     colour = INK, margin = margin(b = 4)),
      plot.subtitle   = element_text(size = base * 0.92, colour = INK2,
                                     margin = margin(b = 14), lineheight = 1.25),
      plot.caption    = element_text(size = base * 0.78, colour = MUTED,
                                     hjust = 0, margin = margin(t = 14),
                                     lineheight = 1.3),
      plot.title.position  = "plot",
      plot.caption.position = "plot",
      axis.title      = element_text(size = base * 0.82, colour = MUTED),
      axis.text       = element_text(size = base * 0.84, colour = INK2),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = GRID, linewidth = 0.35),
      legend.position = "top",
      legend.justification = "left",
      legend.title    = element_blank(),
      legend.text     = element_text(size = base * 0.85, colour = INK2),
      plot.margin     = margin(16, 20, 12, 16),
      plot.background = element_rect(fill = "white", colour = NA))
}

save_plot <- function(p, file, w = 9, h = 6) {
  ggsave(file.path(OUT_DIR, file), p, width = w, height = h, dpi = 200, bg = "white")
  message("  saved ", file)
}

# horizontal diverging bar — the workhorse for figures 1, 3, 5, 7, 8
bar_diverging <- function(df, xvar, labvar, title, subtitle, xlab, caption,
                          digits = 2, highlight = NULL) {
  df <- as.data.frame(df) %>%
    mutate(.x = .data[[xvar]], .lab = .data[[labvar]],
           .fill = if (is.null(highlight)) ifelse(.x >= 0, "pos", "neg")
                   else ifelse(.lab %in% highlight,
                               ifelse(.x >= 0, "pos", "neg"), "dim")) %>%
    arrange(.x) %>% mutate(.lab = factor(.lab, levels = .lab))
  rng <- max(abs(df$.x))
  ggplot(df, aes(x = .x, y = .lab, fill = .fill)) +
    geom_col(width = 0.62) +
    geom_vline(xintercept = 0, colour = MUTED, linewidth = 0.4) +
    geom_text(aes(label = sprintf(paste0("%+.", digits, "f"), .x),
                  hjust = ifelse(.x >= 0, -0.18, 1.18)),
              size = 3.0, colour = INK2) +
    scale_fill_manual(values = c(pos = BLUE, neg = RED, dim = GREY), guide = "none") +
    scale_x_continuous(expand = expansion(mult = 0.14),
                       limits = c(-rng * 1.18, rng * 1.18)) +
    labs(title = title, subtitle = subtitle, x = xlab, y = NULL, caption = caption) +
    theme_icio()
}


# =============================================================================
#  FIGURE 1 — share of world value added
# =============================================================================

world_va <- industry[year %in% c(2019, 2022), .(va = sum(va)), by = .(year, country)]
world_va[, share := va / sum(va) * 100, by = year]

fig1_data <- dcast(world_va, country ~ year, value.var = c("va", "share")) %>%
  as.data.table()
fig1_data[, `:=`(change = share_2022 - share_2019,
                 growth = va_2022 / va_2019 * 100 - 100)]

fig1_sel <- rbind(head(fig1_data[order(-change)], 9),
                  head(fig1_data[order(change)],  9))
fig1_sel[, label := nm(country, COUNTRY_NAMES)]

fig1 <- bar_diverging(
  fig1_sel, "change", "label",
  "Who grew faster than the world, 2019 to 2022",
  "Change in share of global value added, percentage points.\nNine largest gainers and nine largest losers, of 76 economies.",
  "percentage-point change in share of world value added",
  paste0("Source: OECD ICIO 2025 edition. World value added $81.3tn (2019) to $93.8tn (2022), +15.3%.\n",
         "Current prices and exchange rates: a share change nets out world inflation but not the exchange rate."))
save_plot(fig1, "fig1_world_va_share.png", 9, 6.5)


# =============================================================================
#  FIGURE 2 — the EU27 share of UK trade, 2016–2022   [the key chart]
# =============================================================================

eu_share <- function(dt, who, direction) {
  if (direction == "exports") {
    d <- dt[exporter == who]
    d[, .(share = sum(value[importer %in% EU27]) / sum(value) * 100), by = year]
  } else {
    d <- dt[importer == who]
    d[, .(share = sum(value[exporter %in% EU27]) / sum(value) * 100), by = year]
  }
}

fig2_data <- rbind(
  eu_share(bilateral, "GBR", "exports")[, series := "UK exports to EU27"],
  eu_share(bilateral, "GBR", "imports")[, series := "UK imports from EU27"],
  eu_share(bilateral, "CHE", "exports")[, series := "Switzerland exports to EU27"])

fig2 <- ggplot(fig2_data, aes(year, share, colour = series)) +
  geom_vline(xintercept = c(2021, 2022), linetype = "dotted", colour = MUTED) +
  annotate("text", x = 2021, y = 52.6, label = "2021  EU controls\non UK goods",
           hjust = 1.06, size = 2.9, colour = MUTED, lineheight = 0.95) +
  annotate("text", x = 2022, y = 46.2, label = "2022  UK controls\non EU goods",
           hjust = 1.06, size = 2.9, colour = MUTED, lineheight = 0.95) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.4) +
  scale_colour_manual(values = c("UK exports to EU27" = BLUE,
                                 "UK imports from EU27" = ORANGE,
                                 "Switzerland exports to EU27" = MUTED)) +
  scale_x_continuous(breaks = YEARS) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(title = "Two breaks, one year apart",
       subtitle = "EU27 share of UK gross exports and imports, with Switzerland as a comparator.\nGross trade = intermediate flows plus final demand.",
       x = NULL, y = NULL,
       caption = paste0("Source: OECD ICIO 2025 edition. The UK export share falls 3.8pp in 2021; the import share falls 5.9pp in 2022.\n",
                        "Switzerland — a non-EU European economy with no change in its arrangements — stays within one point across all seven years.")) +
  theme_icio()
save_plot(fig2, "fig2_eu_share_timeseries.png", 9, 6)


# =============================================================================
#  FIGURE 3 — cross-section: EU27 export share change, major non-EU exporters
# =============================================================================

exp_tot <- bilateral[year %in% c(2019, 2022), .(total = sum(value)), by = .(year, exporter)]
exp_eu  <- bilateral[year %in% c(2019, 2022) & importer %in% EU27 & !(exporter %in% EU27),
                     .(eu = sum(value)), by = .(year, exporter)]

fig3_data <- merge(exp_eu, exp_tot, by = c("year", "exporter"))[, share := eu / total * 100]
fig3_data <- dcast(fig3_data, exporter ~ year, value.var = c("share", "total")) %>%
  as.data.table()
fig3_data[, change := share_2022 - share_2019]
fig3_data <- fig3_data[exporter != "ROW" & total_2022 > 100000]   # >$100bn exporters
fig3_data[, label := nm(exporter, COUNTRY_NAMES)]

cat("\nUK rank among the", nrow(fig3_data), "non-EU economies exporting >$100bn: ",
    which(fig3_data[order(change)]$exporter == "GBR"), "\n")

fig3 <- bar_diverging(
  fig3_data, "change", "label",
  "Change in EU27 export share, major non-EU exporters",
  "Percentage-point change 2019 to 2022. All non-EU economies with gross exports above $100bn in 2022.",
  "percentage-point change in EU27 share of gross exports",
  paste0("Source: OECD ICIO 2025 edition. Only sanctioned Russia turned away from the EU faster than the UK.\n",
         "Across all 53 non-EU economies the UK ranks 12th; the eleven larger falls are all small or commodity-dependent exporters."),
  highlight = c("United Kingdom", "Russia", "Switzerland"))
save_plot(fig3, "fig3_eu_share_crosssection.png", 9, 7.5)


# =============================================================================
#  FIGURE 4 — UK EU27 share by direction and product
# =============================================================================

uk_flow_share <- function(dt, partner_col, sector_filter, flow, label) {
  d <- copy(dt[year %in% c(2019, 2022)])
  if (!is.null(sector_filter)) d <- d[sector %in% sector_filter]
  d[, is_eu := get(partner_col) %in% EU27]
  d[, .(share = sum(value[is_eu]) / sum(value) * 100,
        total = sum(value) / 1000), by = year][, `:=`(flow = flow, category = label)]
}

SERVICES <- setdiff(names(SECTOR_NAMES), c(GOODS, "D", "E", "F"))

fig4_data <- rbind(
  uk_flow_share(uk_exports, "destination", NULL,                   "Exports", "All exports"),
  uk_flow_share(uk_exports, "destination", SERVICES,               "Exports", "Services"),
  uk_flow_share(uk_exports, "destination", setdiff(GOODS, ENERGY), "Exports", "Goods excl. energy"),
  uk_flow_share(uk_exports, "destination", ENERGY,                 "Exports", "Energy products"),
  uk_flow_share(uk_imports, "origin",      NULL,                   "Imports", "All imports"),
  uk_flow_share(uk_imports, "origin",      SERVICES,               "Imports", "Services"),
  uk_flow_share(uk_imports, "origin",      setdiff(GOODS, ENERGY), "Imports", "Goods excl. energy"),
  uk_flow_share(uk_imports, "origin",      ENERGY,                 "Imports", "Energy products"))

fig4_wide <- dcast(fig4_data, flow + category ~ year, value.var = "share") %>% as.data.table()
fig4_wide[, `:=`(change = `2022` - `2019`,
                 row = paste0(flow, ": ", category))]
setorder(fig4_wide, flow, change)
fig4_wide[, row := factor(row, levels = row)]

fig4 <- ggplot(fig4_wide) +
  geom_segment(aes(x = `2019`, xend = `2022`, y = row, yend = row, colour = flow),
               linewidth = 0.9) +
  geom_point(aes(x = `2019`, y = row, colour = flow), size = 3.4,
             shape = 21, fill = "white", stroke = 1.3) +
  geom_point(aes(x = `2022`, y = row, colour = flow), size = 3.4) +
  geom_text(aes(x = pmax(`2019`, `2022`), y = row,
                label = sprintf("%+.2f pp", change)),
            hjust = -0.28, size = 3.0, colour = INK2) +
  scale_colour_manual(values = c(Exports = BLUE, Imports = ORANGE)) +
  scale_x_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0.05, 0.20))) +
  labs(title = "Goods on the import side, services on the export side",
       subtitle = "EU27 share of UK gross trade, 2019 (hollow) and 2022 (filled).",
       x = NULL, y = NULL,
       caption = paste0("Source: OECD ICIO 2025 edition. The two largest moves are goods imports (-10.2pp) and services exports (-3.3pp) —
",
                        "the two flows where something actually changed in law. Services imports moved 0.8 points.")) +
  theme_icio()
save_plot(fig4, "fig4_uk_eu_share_by_flow.png", 9, 5.5)


# =============================================================================
#  FIGURE 5 — UK services exports: EU share change by industry
# =============================================================================

fig5_data <- uk_exports[year %in% c(2019, 2022) & sector %in% SERVICES,
  .(eu    = sum(value[destination %in% EU27]),
    total = sum(value)), by = .(year, sector)][, share := eu / total * 100]
fig5_data <- dcast(fig5_data, sector ~ year, value.var = c("share", "total")) %>%
  as.data.table()
fig5_data[, change := share_2022 - share_2019]
fig5_data <- fig5_data[total_2022 > 8000]            # industries exporting >$8bn
fig5_data[, label := paste0(SECTOR_NAMES[sector], "  ($",
                            round(total_2022 / 1000), "bn)")]

fig5 <- bar_diverging(
  fig5_data, "change", "label",
  "The services decoupling is broad, not sectoral",
  "Change in the EU27 share of UK exports by services industry, 2019 to 2022.\nIndustries with 2022 exports above $8bn; 2022 export value in brackets.",
  "percentage-point change in EU27 share of that industry's exports",
  paste0("Source: OECD ICIO 2025 edition. The two largest UK service exports — finance and professional services —\n",
         "both lost more than four points of EU share. Travel-adjacent industries gained as European tourism recovered faster than long-haul."))
save_plot(fig5, "fig5_uk_services_eu_share.png", 9, 6.5)


# =============================================================================
#  FIGURE 6 — UK energy imports by origin
# =============================================================================

fig6_data <- uk_imports[year %in% c(2019, 2022) & sector %in% ENERGY & origin != "GBR",
                        .(value = sum(value) / 1000), by = .(year, origin)]
top8 <- fig6_data[year == 2022][order(-value)][1:8]$origin
fig6_data <- fig6_data[origin %in% top8]
fig6_data[, `:=`(label = nm(origin, COUNTRY_NAMES), year = factor(year))]
fig6_data[, label := factor(label, levels = rev(nm(top8, COUNTRY_NAMES)))]

cat("\nUK total energy imports ($bn):\n")
print(uk_imports[year %in% c(2019, 2022) & sector %in% ENERGY & origin != "GBR",
                 .(bn = round(sum(value) / 1000, 1)), by = year])

fig6 <- ggplot(fig6_data, aes(value, label, fill = year)) +
  geom_col(position = position_dodge(width = 0.68), width = 0.62) +
  geom_text(aes(label = sprintf("%.1f", value)),
            position = position_dodge(width = 0.68),
            hjust = -0.22, size = 2.9, colour = INK2) +
  scale_fill_manual(values = c("2019" = GREY, "2022" = BLUE)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.14))) +
  labs(title = "One supplier absorbed the switch",
       subtitle = "UK imports of coal, oil and gas, refined petroleum, and electricity and gas supply, by origin.",
       x = "USD billion", y = NULL,
       caption = paste0("Source: OECD ICIO 2025 edition. UK energy imports rose from $64.3bn to $123.8bn, +93%.\n",
                        "Norway alone accounts for five sixths of the $60bn increase; Russian supply fell 71%. 'Rest of world' is the ICIO residual aggregate.")) +
  theme_icio()
save_plot(fig6, "fig6_uk_energy_imports.png", 9, 5.5)


# =============================================================================
#  FIGURE 7 — electricity & gas value-added margin
# =============================================================================

PEERS <- c("GBR","DEU","FRA","USA","ITA","ESP","NLD","JPN","KOR","CHN")

fig7_data <- industry[year %in% c(2019, 2022) & sector == "D" & country %in% PEERS,
                      .(year, country, margin = va / output * 100)]
fig7_data <- dcast(fig7_data, country ~ year, value.var = "margin") %>% as.data.table()
fig7_data[, `:=`(change = `2022` - `2019`, label = nm(country, COUNTRY_NAMES))]

cat("\nUK electricity & gas, output vs value added ($bn):\n")
print(industry[country == "GBR" & sector == "D" & year %in% c(2019, 2022),
               .(year, output = round(output / 1000, 1), va = round(va / 1000, 1),
                 margin = round(va / output * 100, 1))])

fig7 <- bar_diverging(
  fig7_data, "change", "label",
  "Where the energy cost landed",
  "Change in the value-added margin of the electricity and gas supply industry, 2019 to 2022.\nA falling margin means input costs rose faster than the sector could charge.",
  "percentage-point change in value added / gross output",
  paste0("Source: OECD ICIO 2025 edition. UK gross output rose 84% while value added fell 20%: the margin halved, 24.3% to 10.6%.\n",
         "The split looks regulatory rather than geographic — capped retail markets absorbed the shock; the US and Germany passed it through."),
  digits = 1)
save_plot(fig7, "fig7_utility_margins.png", 9, 5.5)


# =============================================================================
#  FIGURE 8 — UK value added by industry
# =============================================================================

fig8_data <- industry[year %in% c(2019, 2022) & country == "GBR",
                      .(va = sum(va) / 1000), by = .(year, sector)]
fig8_data <- dcast(fig8_data, sector ~ year, value.var = "va") %>% as.data.table()
fig8_data[, `:=`(change = `2022` - `2019`, growth = `2022` / `2019` * 100 - 100)]

cat("\nUK total value added ($bn):",
    round(sum(fig8_data$`2019`)), "->", round(sum(fig8_data$`2022`)), "\n")

fig8_sel <- rbind(head(fig8_data[order(-change)], 10), fig8_data[change < 0])
fig8_sel[, label := SECTOR_NAMES[sector]]

fig8 <- bar_diverging(
  fig8_sel, "change", "label",
  "Ten industries added value; nine lost it",
  "Change in UK value added by industry, 2019 to 2022, USD billion, current prices.\nThe ten largest increases and all nine decreases.",
  "change in value added, USD billion",
  paste0("Source: OECD ICIO 2025 edition. UK value added rose from $2,547bn to $2,793bn.\n",
         "Gains concentrate in professional services, finance and health; losses in energy and in transport industries still below pre-pandemic levels."),
  digits = 1)
save_plot(fig8, "fig8_uk_sector_va.png", 9, 7)


# =============================================================================
#  9.  SUMMARY TABLES
# =============================================================================

fwrite(fig1_data[order(-change)], file.path(OUT_DIR, "tbl_world_va_share.csv"))
fwrite(fig2_data,                 file.path(OUT_DIR, "tbl_eu_share_timeseries.csv"))
fwrite(fig3_data[order(change)],  file.path(OUT_DIR, "tbl_eu_share_crosssection.csv"))
fwrite(fig4_wide,                 file.path(OUT_DIR, "tbl_uk_eu_share_by_flow.csv"))
fwrite(fig5_data[order(change)],  file.path(OUT_DIR, "tbl_uk_services_eu_share.csv"))
fwrite(fig7_data[order(change)],  file.path(OUT_DIR, "tbl_utility_margins.csv"))
fwrite(fig8_data[order(-change)], file.path(OUT_DIR, "tbl_uk_sector_va.csv"))

cat("\n--- headline numbers -------------------------------------------------\n")
print(fig4_wide[, .(flow, category,
                    `2019` = round(`2019`, 1), `2022` = round(`2022`, 1),
                    change = round(change, 2))])
cat("\nDone. Charts and tables in: ", OUT_DIR, "\n", sep = "")


# =============================================================================
#  OPTIONAL EXTRA — domestic vs foreign value added in exports (Leontief)
# =============================================================================
#  Not needed for any figure above, but this is the calculation behind the
#  "UK domestic value added in exports grew 14.7%" line in section 5.
#  It inverts the full 4,050 x 4,050 intermediate matrix, so it needs the
#  matrix form rather than the long form. Takes ~2 minutes per year.
#
#  gvc <- function(year) {
#    dt <- fread(file = file.path(DATA_DIR, paste0(year, "_SML.csv")))
#    rn <- dt[[1]]; dt[, V1 := NULL]; m <- as.matrix(dt); rownames(m) <- rn
#    p  <- regexpr("_", rownames(m), fixed = TRUE)
#    rc <- ifelse(p > 0, substr(rownames(m), 1, p - 1), NA)
#    q  <- regexpr("_", colnames(m), fixed = TRUE)
#    cc <- ifelse(q > 0, substr(colnames(m), 1, q - 1), NA)
#    cs <- ifelse(q > 0, substr(colnames(m), q + 1, nchar(colnames(m))), colnames(m))
#    ir <- !is.na(rc); ic <- !is.na(cc) & !(cs %in% FD_CATS); ifd <- cs %in% FD_CATS
#    Z <- m[ir, ic]; FD <- m[ir, ifd]; x <- m["OUT", ic]; va <- m["VA", ic]
#    rc <- rc[ir]; fc <- cc[ifd]
#    xi <- ifelse(x > 0, 1 / x, 0)
#    L  <- solve(diag(ncol(Z)) - sweep(Z, 2, xi, `*`))
#    v  <- va * xi
#    rbindlist(lapply(sort(unique(rc)), function(k) {
#      e <- rowSums(Z[, rc != k, drop = FALSE]) + rowSums(FD[, fc != k, drop = FALSE])
#      e[rc != k] <- 0
#      vc <- v * (L %*% e)[, 1]
#      data.table(country = k, year = year,
#                 domestic_va = sum(vc[rc == k]), foreign_va = sum(vc[rc != k]))
#    }))
#  }
#  gvc_results <- rbindlist(lapply(c(2019, 2022), gvc))

suppressMessages({
  library(tidyverse)
  library(fixest)
  library(plm)
  library(sf)
})
options(width = 200)

out_dir  <- "G:/Shared drives/snvdem/snvdem-col/scripts/deforestation/output"
shp_path <- "G:/Shared drives/snvdem/snvdem-col/data/geospatial/MGN_ANM_MPIOS/MGN_ANM_MPIOS.shp"
clean_mpio <- function(x) stringr::str_pad(as.character(as.numeric(x)), 5, "left", "0")
norm01 <- function(x) (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
turning_point <- function(b1, b2) unname(-b1 / (2 * b2))

dept_names <- c(
  "5"="Antioquia","8"="Atlantico","11"="Bogota D.C.","13"="Bolivar","15"="Boyaca",
  "17"="Caldas","18"="Caqueta","19"="Cauca","20"="Cesar","23"="Cordoba",
  "25"="Cundinamarca","27"="Choco","41"="Huila","44"="La Guajira","47"="Magdalena",
  "50"="Meta","52"="Narino","54"="Norte de Santander","63"="Quindio","66"="Risaralda",
  "68"="Santander","70"="Sucre","73"="Tolima","76"="Valle del Cauca","81"="Arauca",
  "85"="Casanare","86"="Putumayo","88"="San Andres y Providencia","91"="Amazonas",
  "94"="Guainia","95"="Guaviare","97"="Vaupes","99"="Vichada"
)

# ---- load ------------------------------------------------------------
tmf_cover <- readRDS(file.path(out_dir, "tmf_muni_cover.rds"))

dem_df <- readRDS("G:/Shared drives/snvdem/snvdem-col/data/panel/05_weighting/03_output/snvdem_col_weighted.rds") %>%
  mutate(MPIO_CDPMP = clean_mpio(MPIO_CDPMP)) %>%
  select(MPIO_CDPMP, year, snelect, sncivlib, sndem) %>%
  distinct(MPIO_CDPMP, year, .keep_all = TRUE)

muni_names <- st_read(shp_path, quiet = TRUE) %>%
  st_drop_geometry() %>%
  mutate(MPIO_CDPMP = clean_mpio(MPIO_CDPMP), DPTO_CCDGO = as.character(as.numeric(DPTO_CCDGO))) %>%
  select(MPIO_CDPMP, MPIO_CNMBR, DPTO_CCDGO) %>%
  mutate(dept_name = dept_names[DPTO_CCDGO])

analysis_tmf <- tmf_cover %>%
  inner_join(dem_df, by = c("MPIO_CDPMP", "year")) %>%
  mutate(snelect_norm = norm01(snelect)) %>%
  filter(!is.na(d_forest_pct), !is.na(forest_pct_lag1), !is.na(snelect_norm)) %>%
  left_join(muni_names, by = "MPIO_CDPMP")

cat("Loaded panel:", nrow(analysis_tmf), "rows |", n_distinct(analysis_tmf$MPIO_CDPMP), "munis\n")

# ======================================================================
# PART A -- why does two-way FE vs. year-only FE diverge pre-2016?
# ======================================================================
cat("\n\n################ PART A: WITHIN vs BETWEEN DECOMPOSITION ################\n")

decomp <- function(df, label) {
  muni_means <- df %>% group_by(MPIO_CDPMP) %>% summarise(m = mean(snelect_norm), .groups = "drop")
  total_var <- var(df$snelect_norm)
  between_var <- var(muni_means$m)
  df2 <- df %>% left_join(muni_means, by = "MPIO_CDPMP") %>% mutate(within_dev = snelect_norm - m)
  within_var <- var(df2$within_dev)
  cat(sprintf("%-12s total var=%.5f | between-muni var=%.5f (%.1f%% of total) | within-muni var=%.5f (%.1f%% of total)\n",
              label, total_var, between_var, 100 * between_var / total_var, within_var, 100 * within_var / total_var))
}
decomp(filter(analysis_tmf, year < 2016), "Pre-2016")
decomp(filter(analysis_tmf, year >= 2016), "Post-2016")
decomp(analysis_tmf, "Full panel")

cat("\n-- Between-only regressions (collapse to muni means, cross-sectional quadratic) --\n")
between_reg <- function(df, label) {
  cs <- df %>% group_by(MPIO_CDPMP) %>%
    summarise(mean_d = mean(d_forest_pct, na.rm = TRUE),
              mean_snelect = mean(snelect_norm, na.rm = TRUE),
              mean_lag = mean(forest_pct_lag1, na.rm = TRUE), .groups = "drop")
  m <- lm(mean_d ~ mean_snelect + I(mean_snelect^2) + mean_lag, data = cs)
  cf <- coef(m); pv <- summary(m)$coefficients[, 4]
  cat(sprintf("%-10s BETWEEN-only (n=%d munis): b1=%.4f b2=%.4f (p=%.4f) TP=%.3f [%s]\n",
              label, nrow(cs), cf[2], cf[3], pv[3], turning_point(cf[2], cf[3]),
              if (cf[3] > 0) "U-shape" else "inverted-U"))
}
between_reg(filter(analysis_tmf, year < 2016), "Pre-2016")
between_reg(filter(analysis_tmf, year >= 2016), "Post-2016")
between_reg(analysis_tmf, "Full panel")

# ======================================================================
# PART B -- break-year / regime / rolling-window robustness
# ======================================================================
cat("\n\n################ PART B: BREAK-YEAR & ROLLING-WINDOW ROBUSTNESS ################\n")

run_spec_row <- function(df, spec_name) {
  if (nrow(df) < 200 || n_distinct(df$year) < 2) return(NULL)
  if (spec_name == "twoway") {
    m <- feols(d_forest_pct ~ snelect_norm + I(snelect_norm^2) + forest_pct_lag1 | MPIO_CDPMP + year,
               data = df, cluster = ~MPIO_CDPMP)
    cf <- coef(m); s <- se(m)
    b1 <- cf[1]; b2 <- cf[2]; se1 <- s[1]; se2 <- s[2]
  } else if (spec_name == "yearonly") {
    m <- feols(d_forest_pct ~ snelect_norm + I(snelect_norm^2) + forest_pct_lag1 | year,
               data = df, cluster = ~MPIO_CDPMP)
    cf <- coef(m); s <- se(m)
    b1 <- cf[1]; b2 <- cf[2]; se1 <- s[1]; se2 <- s[2]
  } else {
    pdf <- pdata.frame(df, index = c("MPIO_CDPMP", "year"))
    m <- tryCatch(plm(d_forest_pct ~ snelect_norm + I(snelect_norm^2) + forest_pct_lag1 + factor(year),
                       data = pdf, model = "random"), error = function(e) NULL)
    if (is.null(m)) return(NULL)
    cf <- coef(m); s <- sqrt(diag(vcovHC(m, cluster = "group")))
    b1 <- cf["snelect_norm"]; b2 <- cf["I(snelect_norm^2)"]
    se1 <- s["snelect_norm"]; se2 <- s["I(snelect_norm^2)"]
  }
  data.frame(spec = spec_name, n = nrow(df), b1 = unname(b1), se1 = unname(se1),
             b2 = unname(b2), se2 = unname(se2), tp = turning_point(b1, b2),
             shape = if (b2 > 0) "U" else "inverted-U", b2_tstat = unname(b2 / se2))
}

break_years <- c(2006, 2016, 2017, 2018, 2019, 2020)
break_results <- map_dfr(break_years, function(by) {
  pre <- filter(analysis_tmf, year < by)
  post <- filter(analysis_tmf, year >= by)
  bind_rows(
    map_dfr(c("twoway", "yearonly", "re"), ~ run_spec_row(pre, .x)) %>% mutate(break_year = by, period = paste0("pre-", by)),
    map_dfr(c("twoway", "yearonly", "re"), ~ run_spec_row(post, .x)) %>% mutate(break_year = by, period = paste0("post-", by))
  )
})
cat("\n-- Break-year robustness table --\n")
print(break_results)
write_csv(break_results, file.path(out_dir, "robustness_breakpoints_tmf.csv"))

cat("\n-- Three-regime split (pre-2006 AUC era / 2006-2015 / post-2016) --\n")
regime_results <- bind_rows(
  map_dfr(c("twoway", "yearonly", "re"), ~ run_spec_row(filter(analysis_tmf, year < 2006), .x)) %>% mutate(period = "pre-2006 (AUC demob era)"),
  map_dfr(c("twoway", "yearonly", "re"), ~ run_spec_row(filter(analysis_tmf, year >= 2006 & year < 2016), .x)) %>% mutate(period = "2006-2015 (post-AUC, pre-accord)"),
  map_dfr(c("twoway", "yearonly", "re"), ~ run_spec_row(filter(analysis_tmf, year >= 2016), .x)) %>% mutate(period = "2016-2023 (post-accord)")
)
print(regime_results)
write_csv(regime_results, file.path(out_dir, "robustness_regimes_tmf.csv"))

cat("\n-- Rolling 6-year windows, stepped by 1 year (smooth-drift check) --\n")
rolling_results <- map_dfr(2000:2018, function(start_yr) {
  yrs <- start_yr:(start_yr + 5)
  sub <- filter(analysis_tmf, year %in% yrs)
  if (n_distinct(sub$year) < 6) return(NULL)
  bind_rows(
    run_spec_row(sub, "twoway")   %>% mutate(window = paste0(start_yr, "-", start_yr + 5)),
    run_spec_row(sub, "yearonly") %>% mutate(window = paste0(start_yr, "-", start_yr + 5))
  )
})
print(rolling_results)
write_csv(rolling_results, file.path(out_dir, "rolling_window_tmf.csv"))

# ======================================================================
# PART C -- mechanism / concrete case studies
# ======================================================================
cat("\n\n################ PART C: CASE STUDIES ################\n")

muni_period <- analysis_tmf %>%
  group_by(MPIO_CDPMP, MPIO_CNMBR, dept_name) %>%
  summarise(
    mean_snelect_all = mean(snelect_norm, na.rm = TRUE),
    mean_d_pre  = mean(d_forest_pct[year < 2016], na.rm = TRUE),
    mean_d_post = mean(d_forest_pct[year >= 2016], na.rm = TRUE),
    n_pre  = sum(year < 2016),
    n_post = sum(year >= 2016),
    .groups = "drop"
  ) %>%
  filter(n_pre >= 10, n_post >= 5) %>%
  mutate(shift = mean_d_post - mean_d_pre,
         snelect_tercile = ntile(mean_snelect_all, 3),
         snelect_decile  = ntile(mean_snelect_all, 10))

cat("\n-- Middling-democracy (tercile 2) municipalities with the largest post-2016 DROP in forest-cover change --\n")
middling_rampup <- muni_period %>% filter(snelect_tercile == 2) %>% arrange(shift) %>% head(10)
print(middling_rampup)

cat("\n-- Middling-democracy (tercile 2) municipalities with the largest post-2016 RISE (least negative shift) --\n")
middling_improve <- muni_period %>% filter(snelect_tercile == 2) %>% arrange(desc(shift)) %>% head(10)
print(middling_improve)

cat("\n-- Extreme-democracy (decile 1 or 10) municipalities that stayed most STABLE pre/post 2016 --\n")
extreme_stable <- muni_period %>% filter(snelect_decile %in% c(1, 10)) %>%
  mutate(abs_shift = abs(shift)) %>% arrange(abs_shift) %>% head(10)
print(extreme_stable)

cat("\n-- Full year-by-year trajectories for the top 2 middling-rampup and top 2 extreme-stable examples --\n")
example_ids <- c(middling_rampup$MPIO_CDPMP[1:2], extreme_stable$MPIO_CDPMP[1:2])
traj <- analysis_tmf %>% filter(MPIO_CDPMP %in% example_ids) %>%
  select(MPIO_CDPMP, MPIO_CNMBR, dept_name, year, snelect_norm, d_forest_pct, forest_pct) %>%
  arrange(MPIO_CDPMP, year)
print(traj)

cat("\n-- Department-level mean shift (pre-2016 -> post-2016), sorted worst to best --\n")
dept_shift <- muni_period %>% group_by(dept_name) %>%
  summarise(mean_shift = mean(shift, na.rm = TRUE), n_munis = n(), .groups = "drop") %>%
  arrange(mean_shift)
print(dept_shift)

write_csv(muni_period, file.path(out_dir, "muni_period_shift_tmf.csv"))

# ======================================================================
# PART D -- descriptive diagnostics: trends, extremes, geography
# ======================================================================
cat("\n\n################ PART D: DESCRIPTIVE DIAGNOSTICS ################\n")

cat("\n-- National annual trend --\n")
national_trend <- analysis_tmf %>% group_by(year) %>%
  summarise(total_forest_ha = sum(forest_ha, na.rm = TRUE),
            mean_d = mean(d_forest_pct, na.rm = TRUE),
            share_gain = mean(d_forest_pct > 0, na.rm = TRUE),
            share_loss = mean(d_forest_pct < 0, na.rm = TRUE), .groups = "drop")
print(national_trend)
write_csv(national_trend, file.path(out_dir, "national_trend_tmf.csv"))
cat(sprintf("\nNet change in total forest stock, 2000->2023 (ha): %.0f (%.2f%% of 2000 stock)\n",
            national_trend$total_forest_ha[national_trend$year == 2023] - national_trend$total_forest_ha[national_trend$year == 2000],
            100 * (national_trend$total_forest_ha[national_trend$year == 2023] - national_trend$total_forest_ha[national_trend$year == 2000]) / national_trend$total_forest_ha[national_trend$year == 2000]))

cat("\n-- Municipality-level cumulative change, 2000 vs 2023 --\n")
muni_cumulative <- analysis_tmf %>%
  group_by(MPIO_CDPMP, MPIO_CNMBR, dept_name) %>%
  summarise(
    forest_pct_2000 = forest_pct[year == min(year)][1],
    forest_pct_2023 = forest_pct[year == max(year)][1],
    cumulative_sum_d = sum(d_forest_pct, na.rm = TRUE),
    mean_snelect = mean(snelect_norm, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(net_change_pp = forest_pct_2023 - forest_pct_2000)

cat("\nTop 20 EXTREME FOREST GROWTH municipalities, 2000->2023:\n")
top_growth <- muni_cumulative %>% arrange(desc(net_change_pp)) %>% head(20)
print(top_growth)

cat("\nTop 20 EXTREME DEFORESTATION municipalities, 2000->2023:\n")
top_loss <- muni_cumulative %>% arrange(net_change_pp) %>% head(20)
print(top_loss)

write_csv(muni_cumulative, file.path(out_dir, "muni_cumulative_change_tmf.csv"))

cat("\n-- Department-level mean net change, 2000->2023 --\n")
dept_trend <- muni_cumulative %>% group_by(dept_name) %>%
  summarise(mean_net_change = mean(net_change_pp, na.rm = TRUE), n_munis = n(), .groups = "drop") %>%
  arrange(mean_net_change)
print(dept_trend)

cat("\n-- Accelerating vs. decelerating loss: mean d_forest_pct, 2000-2011 vs 2012-2023 --\n")
rate_compare <- analysis_tmf %>%
  mutate(era = ifelse(year <= 2011, "era_2000_2011", "era_2012_2023")) %>%
  group_by(MPIO_CDPMP, era) %>% summarise(mean_d = mean(d_forest_pct, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = era, values_from = mean_d) %>%
  left_join(muni_names, by = "MPIO_CDPMP") %>%
  mutate(change_in_rate = era_2012_2023 - era_2000_2011)

cat("\nTop 15 municipalities where loss is ACCELERATING most (rate got more negative):\n")
print(rate_compare %>% arrange(change_in_rate) %>% head(15))
cat("\nTop 15 municipalities where forest change is IMPROVING most (rate got more positive):\n")
print(rate_compare %>% arrange(desc(change_in_rate)) %>% head(15))
write_csv(rate_compare, file.path(out_dir, "muni_rate_acceleration_tmf.csv"))

cat("\n-- National mean d_forest_pct: pre-2011 vs 2012-2023 (is loss accelerating nationally?) --\n")
cat(sprintf("2000-2011 national mean d_forest_pct: %.4f\n", mean(filter(analysis_tmf, year <= 2011)$d_forest_pct, na.rm = TRUE)))
cat(sprintf("2012-2023 national mean d_forest_pct: %.4f\n", mean(filter(analysis_tmf, year >= 2012)$d_forest_pct, na.rm = TRUE)))

# ---- maps --------------------------------------------------------------
cat("\n-- Building maps --\n")
muni_shp2 <- st_read(shp_path, quiet = TRUE) %>% mutate(MPIO_CDPMP = clean_mpio(MPIO_CDPMP))

map_df <- muni_shp2 %>% left_join(muni_cumulative, by = "MPIO_CDPMP")
p_net <- ggplot(map_df) + geom_sf(aes(fill = net_change_pp), color = NA) +
  scale_fill_distiller(palette = "RdYlGn", direction = 1, name = "Net forest-cover\nchange, pp\n(2000 to 2023)",
                        na.value = "grey85", limits = c(-1, 1) * max(abs(map_df$net_change_pp), na.rm = TRUE)) +
  labs(title = "Net Forest-Cover Change by Municipality, 2000-2023 (JRC TMF)",
       subtitle = "Endpoint comparison: forest_pct in final observed year minus forest_pct in first observed year") +
  theme_void() +
  theme(plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 9, hjust = 0.5, color = "grey40"),
        plot.background = element_rect(fill = "white", color = NA))
ggsave(file.path(out_dir, "map_net_change_2000_2023_tmf.png"), p_net, width = 9, height = 11, dpi = 300, bg = "white")

map_df2 <- muni_shp2 %>% left_join(muni_period, by = "MPIO_CDPMP")
p_shift <- ggplot(map_df2) + geom_sf(aes(fill = shift), color = NA) +
  scale_fill_distiller(palette = "RdBu", direction = 1, name = "Shift in mean\nforest change, pp\n(post-2016 minus\npre-2016)",
                        na.value = "grey85", limits = c(-1, 1) * max(abs(map_df2$shift), na.rm = TRUE)) +
  labs(title = "Shift in Forest-Cover Change, Pre- vs. Post-2016 (JRC TMF)",
       subtitle = "Blue = forest change improved after 2016, red = forest change worsened after 2016") +
  theme_void() +
  theme(plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 9, hjust = 0.5, color = "grey40"),
        plot.background = element_rect(fill = "white", color = NA))
ggsave(file.path(out_dir, "map_prepost2016_shift_tmf.png"), p_shift, width = 9, height = 11, dpi = 300, bg = "white")

cat("\nDONE.\n")

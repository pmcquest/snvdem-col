# Co-author's LOO methodology (calc_loco / vd_df from 2026_04_LOO_tests.qmd),
# run unmodified against the CURRENT production data, for a same-data
# comparison against leave_one_out.R -- see loo_comparison_memo.md.

library(tidyverse)

snvdem <- readRDS("G:/Shared drives/snvdem/snvdem-col/data/panel/05_weighting/03_output/snvdem_col_weighted.rds")

chars <- c("rural_urban", "econ_dev", "capital", "north", "south",
           "west", "east", "violence", "sparse", "remote",
           "indigenous", "ruling_party")
char_labels <- c(
  "rural_urban" = "Rural/Urban", "econ_dev" = "Economic Development", "capital" = "Distance from Capital",
  "north" = "North", "south" = "South", "west" = "West", "east" = "East",
  "violence" = "Violence Index", "sparse" = "Sparse Population", "remote" = "Remoteness",
  "indigenous" = "Indigenous Population", "ruling_party" = "Ruling Party Strength"
)

calc_loco <- function(df, excl) {
  df %>% mutate(
    el_num_x =
      (if (excl != "rural_urban")  (avg0t1hi * el_Urban) + (avg0t1lo * el_Rural)                else 0) +
      (if (excl != "econ_dev")     (avg2t3hi * el_More_development) + (avg2t3lo * el_Less_development) else 0) +
      (if (excl != "capital")      (avg4t5hi * (1 - el_Inside_capital)) + (avg4t5lo * (1 - el_Outside_capital)) else 0) +
      (if (excl != "north")        (avg6 * el_North)         else 0) +
      (if (excl != "south")        (avg7 * el_South)         else 0) +
      (if (excl != "west")         (avg8 * el_West)          else 0) +
      (if (excl != "east")         (avg9 * el_East)          else 0) +
      (if (excl != "violence")     (avg10t11 * wt_el_1011)   else 0) +
      (if (excl != "sparse")       (avg12 * el_Sparse_population) else 0) +
      (if (excl != "remote")       (avg13 * el_Remote)       else 0) +
      (if (excl != "indigenous")   (avg14 * (1 - el_Indigenous)) else 0) +
      (if (excl != "ruling_party") (avg15t16hi * (1 - el_Ruling_party_strong)) + (avg15t16lo * (1 - el_Ruling_party_weak)) else 0),
    el_den_x =
      (if (excl != "rural_urban")  el_Urban + el_Rural                               else 0) +
      (if (excl != "econ_dev")     el_More_development + el_Less_development           else 0) +
      (if (excl != "capital")      (1 - el_Inside_capital) + (1 - el_Outside_capital) else 0) +
      (if (excl != "north")        el_North         else 0) +
      (if (excl != "south")        el_South         else 0) +
      (if (excl != "west")         el_West          else 0) +
      (if (excl != "east")         el_East          else 0) +
      (if (excl != "violence")     wt_el_1011       else 0) +
      (if (excl != "sparse")       el_Sparse_population else 0) +
      (if (excl != "remote")       el_Remote        else 0) +
      (if (excl != "indigenous")   (1 - el_Indigenous)  else 0) +
      (if (excl != "ruling_party") (1 - el_Ruling_party_strong) + (1 - el_Ruling_party_weak) else 0),
    cl_num_x =
      (if (excl != "rural_urban")  (avg0t1hi * cl_Urban) + (avg0t1lo * cl_Rural)     else 0) +
      (if (excl != "econ_dev")     (avg2t3hi * cl_More_development) + (avg2t3lo * cl_Less_development) else 0) +
      (if (excl != "capital")      (avg4t5hi * (1 - cl_Inside_capital)) + (avg4t5lo * (1 - cl_Outside_capital)) else 0) +
      (if (excl != "north")        (avg6 * cl_North)         else 0) +
      (if (excl != "south")        (avg7 * cl_South)         else 0) +
      (if (excl != "west")         (avg8 * cl_West)          else 0) +
      (if (excl != "east")         (avg9 * cl_East)          else 0) +
      (if (excl != "violence")     (avg10t11 * wt_cl_1011)   else 0) +
      (if (excl != "sparse")       (avg12 * cl_Sparse_population) else 0) +
      (if (excl != "remote")       (avg13 * cl_Remote)       else 0) +
      (if (excl != "indigenous")   (avg14 * (1 - cl_Indigenous)) else 0) +
      (if (excl != "ruling_party") (avg15t16hi * (1 - cl_Ruling_party_strong)) + (avg15t16lo * (1 - cl_Ruling_party_weak)) else 0),
    cl_den_x =
      (if (excl != "rural_urban")  cl_Urban + cl_Rural    else 0) +
      (if (excl != "econ_dev")     cl_More_development + cl_Less_development           else 0) +
      (if (excl != "capital")      (1 - cl_Inside_capital) + (1 - cl_Outside_capital) else 0) +
      (if (excl != "north")        cl_North         else 0) +
      (if (excl != "south")        cl_South         else 0) +
      (if (excl != "west")         cl_West          else 0) +
      (if (excl != "east")         cl_East          else 0) +
      (if (excl != "violence")     wt_cl_1011       else 0) +
      (if (excl != "sparse")       cl_Sparse_population else 0) +
      (if (excl != "remote")       cl_Remote        else 0) +
      (if (excl != "indigenous")   (1 - cl_Indigenous)  else 0) +
      (if (excl != "ruling_party") (1 - cl_Ruling_party_strong) + (1 - cl_Ruling_party_weak) else 0),
    snelect_x  = el_num_x / el_den_x,
    sncivlib_x = cl_num_x / cl_den_x,
    sndem_x    = 0.5 * (snelect_x + sncivlib_x)
  ) %>% select(-el_num_x, -el_den_x, -cl_num_x, -cl_den_x)
}

loco_results <- map_dfr(chars, function(ch) {
  excl_df <- calc_loco(snvdem, excl = ch)
  tibble(
    characteristic = ch, label = char_labels[ch],
    cor_snelect  = cor(excl_df$snelect_x,  snvdem$snelect,  use = "complete.obs"),
    cor_sncivlib = cor(excl_df$sncivlib_x, snvdem$sncivlib, use = "complete.obs"),
    cor_sndem    = cor(excl_df$sndem_x,    snvdem$sndem,    use = "complete.obs"),
    mad_sndem    = mean(abs(excl_df$sndem_x - snvdem$sndem), na.rm = TRUE)
  )
}) %>% arrange(cor_sndem)

cat("=== co-author method (unmodified calc_loco), current production data ===\n")
print(loco_results, n = Inf)

vd_df <- snvdem %>% mutate(
  wc_el_rural_urban  = (avg0t1hi * el_Urban)  + (avg0t1lo * el_Rural),
  wc_el_econ_dev     = (avg2t3hi * el_More_development) + (avg2t3lo * el_Less_development),
  wc_el_capital      = (avg4t5hi * (1 - el_Inside_capital)) + (avg4t5lo * (1 - el_Outside_capital)),
  wc_el_north = avg6*el_North, wc_el_south = avg7*el_South, wc_el_west = avg8*el_West, wc_el_east = avg9*el_East,
  wc_el_violence = avg10t11*wt_el_1011, wc_el_sparse = avg12*el_Sparse_population, wc_el_remote = avg13*el_Remote,
  wc_el_indigenous = avg14*(1-el_Indigenous),
  wc_el_ruling_party = (avg15t16hi*(1-el_Ruling_party_strong)) + (avg15t16lo*(1-el_Ruling_party_weak)),
  wc_cl_rural_urban  = (avg0t1hi * cl_Urban)  + (avg0t1lo * cl_Rural),
  wc_cl_econ_dev     = (avg2t3hi * cl_More_development) + (avg2t3lo * cl_Less_development),
  wc_cl_capital      = (avg4t5hi * (1 - cl_Inside_capital)) + (avg4t5lo * (1 - cl_Outside_capital)),
  wc_cl_north = avg6*cl_North, wc_cl_south = avg7*cl_South, wc_cl_west = avg8*cl_West, wc_cl_east = avg9*cl_East,
  wc_cl_violence = avg10t11*wt_cl_1011, wc_cl_sparse = avg12*cl_Sparse_population, wc_cl_remote = avg13*cl_Remote,
  wc_cl_indigenous = avg14*(1-cl_Indigenous),
  wc_cl_ruling_party = (avg15t16hi*(1-cl_Ruling_party_strong)) + (avg15t16lo*(1-cl_Ruling_party_weak))
)
vd_results <- tibble(
  characteristic = chars, label = char_labels[chars],
  cor_with_sndem = map_dbl(chars, ~{
    avg_wc <- (vd_df[[paste0("wc_el_", .x)]] + vd_df[[paste0("wc_cl_", .x)]]) / 2
    cor(avg_wc, vd_df$sndem, use = "complete.obs")
  })
) %>% arrange(desc(abs(cor_with_sndem)))

cat("\n=== co-author variance-decomposition method, current production data ===\n")
print(vd_results, n = Inf)

final_table <- loco_results %>%
  select(label, cor_snelect, cor_sncivlib, cor_sndem, mad_sndem) %>%
  left_join(vd_results %>% select(label, cor_with_sndem), by = "label") %>%
  arrange(cor_sndem) %>%
  rename(
    "Characteristic" = label,
    "LOO: Electoral Fairness" = cor_snelect,
    "LOO: Civil Liberties" = cor_sncivlib,
    "LOO: Overall SNVDEM" = cor_sndem,
    "Mean Abs. Deviation from Full Index" = mad_sndem,
    "Weighted Contribution to Overall Index" = cor_with_sndem
  )
write_csv(final_table, "G:/Shared drives/snvdem/snvdem-col/scripts/Exploratory/LOO/coauthor_LOO_Table.csv")

# South department-level check, referenced in loo_comparison_memo.md
cat("\n=== South (avg7) by department, 2023 ===\n")
dept_south <- snvdem %>% filter(year == 2023) %>% group_by(DPTO_CCDGO) %>%
  summarise(mean_south = mean(avg7, na.rm = TRUE), n = n()) %>% arrange(desc(mean_south))
print(dept_south, n = 33)

cat("\nSaved coauthor_LOO_Table.csv (current production data)\n")

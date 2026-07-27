#---- Step 3: Clean CDF data ----

# Pipeline:
# Step 1: Wrangle and clean raw data (Folder "01_empirical_data")
# Step 2: Impute missing values and merge into one panel (Folder "02_imputation", incl.
#         "02_imputation/03_merge_imputed" for the merge + CDF-standardize sub-stage)
# Step 3 (this script): Clean the data and calculate averages (Folder "03_geocoded_panel")
# Step 4: Subset V-Dem data, calculate criteria weights, apply national range (Folder "04_vdem_data")
# Step 5: Weight Averaged CDF data by V-Dem data (Folder "05_weighting")
# Step 6: Benchmark using national V-Dem data (Folder "06_benchmark")
# Step 7: Revise final snvdem index (Folder "07_final_snvdem_data")


## ----Setup ----
# load needed packages
library(tidyverse)
library(corrplot)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)


#---- Calculate average scores for criteria ----
# We import the CDF dataset of all variables and select which variables to average for the individual criteria to be weighted
colvars_cdf <- read_rds("G:/Shared drives/snvdem/snvdem-col/data/panel/02_imputation/03_merge_imputed/imputed_cdf_panel.rds")
colnames(colvars_cdf)
summary(colvars_cdf)



#---- Correlations ----
## A. Compute correlation matrix to make sure there are no perfectly correlated variables
cor_matrix <- cor(colvars_cdf[, c("IndRur_0t1", "PIB_2t3", "IDF_2t3", #0-1, 2-3
                                  "DisBog_4t5", #4-5
                                  "north6", "south7", "west8", "east9", #6-9 raw
                                  "axis_ns", "axis_we", # simplified to axis positions
                                  "Desp_1011", "VDays_1011", "HHomix_1011", "ViolInd_1011", #10-11
                                  "DenPob_12", #12
                                  "DisMer_13", "nAR_13pkm", "lAR_13pkm", "lMjRds_13", #13 (include per km area)
                                  "PropInd_14", #14: Indigenous selected (highly correlated to Ethnic Population (0.78))
                                  "RulPar_15t16", "RulParD_15t16") #15-16
                              ], use = "complete.obs")
# Visualize the correlation matrix as a heatmap
corrplot(cor_matrix, method = "circle", type = "upper", tl.col = "black", tl.srt = 45, addCoef.col = "black")



## B. Compute second correlation with different subset
cor_matrix2 <- cor(colvars_cdf[, c("IndRur_0t1", "PIB_2t3", "IDF_2t3", #0-1, 2-3
                                  "DisBog_4t5", #4-5
                                  #6-9* -- maybe simplify?
                                  "axis_ns", "axis_we",
                                  "ViolInd_1011", #10-11
                                  "DenPob_12", #12
                                  "DisMer_13", "lAR_13pkm", #13: lAR is highly correlated with DenPob_12...
                                  "PropInd_14", #14: Indigenous selected (highly correlated to Ethnic Population (0.78))
                                  "RulPar_15t16") #15-16
], use = "complete.obs")
# Visualize the correlation matrix as a heatmap
corrplot(cor_matrix2, method = "circle", type = "upper", tl.col = "black", tl.srt = 45, addCoef.col = "black")

## Subset data ----
colvars_cdfw <- colvars_cdf[, c("MPIO_CDPMP", "year", "DPTO_CCDGO", # year and categorical data
                                "IndRur_0t1", "PIB_2t3", "IDF_2t3", #0-1, 2-3
                                "DisBog_4t5", #4-5
                                "north6", "south7", "west8", "east9", #6-9: four separate directional variables
                                "ViolInd_1011", #10-11: select only the factor score variable
                                "DenPob_12", #12
                                "DisMer_13", "lAR_13pkm", #13: keep Roads per Km^2, although note it is correlated to 12 Pop. Sparseness (0.8) and to other indicator of 13 Remoteness (Distance to market, -0.6)
                                "PropInd_14", #14
                                "RulPar_15t16")] #15-16: if poor result, try Dichotomous measure (2/3 voted for ruling party)


#----Ensure correct score directions----
# MC (04/02/25)// To ensure that the index is calculated correctly, we have to make sure that [the municipality-year score (score_it)] for each dimension is calculated so that the high values are associated with greater democracy and the low values are associated with less democracy. In many cases, that is straightforward, because we expect high values of per capita GDP, for example, are associated with more democracy. That is true for urban % of population, too. ... make sure high values go with expected higher democracy and low values go with expected lower democracy.
#
# PIPELINE-STRUCTURE NOTE (2026-07-26, team decision w/ MC): direction-correction MOVED out
# of this script into Step 5 (05_weighting/01_weighting_geopredictors/
# 01_weighting_geopredictors.R), so it happens in exactly ONE place in the pipeline instead
# of here AND (for 3 of 6 flipped families) again in Step 5's relevance-weight matrices --
# see the 2026-07-21 note that used to sit here, which first flagged that double-flip risk.
# This script now passes IndRur_0t1, DisBog_4t5, ViolInd_1011, PropInd_14, and RulPar_15t16
# straight through RAW, under their original names -- no more Urban_0t1/BogProx_4t5/
# nonviolent_10t11/NonIndig_14/Compete_15t16 renaming-while-flipping. The generic
# rename_with() call in the "Calculate averages" section below still catches these and
# renames them to the avg0t1/avg4t5/avg10t11/avg14/avg15t16 convention (it matches on
# trailing digits, not on whether a flip happened) -- so CDF_averages.rds keeps the same
# column names as before, but those columns (plus the lAR_13pkm/DisMer_13 pair that used to
# become avg13, see below) are now RAW, NOT direction-corrected. Anyone reading
# CDF_averages.rds directly needs to know high no longer means "more democratic" for these
# columns -- that correction now happens only in Step 5, immediately after loading this
# file. avg2t3 (PIB_2t3 + IDF_2t3) and avg6-9 (north/south/west/east) are UNCHANGED by this
# move: avg2t3's two components were never flipped here (both already assumed
# high=more-democratic), and avg6-9 were already deliberately left raw/undecided -- still
# open, still nobody's flipped these, unaffected either way by today's change.
colnames(colvars_cdfw)

# No mutate/flip here anymore -- IndRur_0t1, DisBog_4t5, ViolInd_1011, PropInd_14, and
# RulPar_15t16 (and, for avg13, lAR_13pkm/DisMer_13 below) pass through unchanged, to be
# flipped exactly once in Step 5.

##----Visualize trends ----
# NOTE (2026-07-26): these three checks now run on RAW (pre-flip) values, since direction
# correction moved to Step 5 -- a line that used to trend "up = improving" here may now
# trend the opposite way (e.g. IndRur_0t1 rising = more rural, not more urban). This means these track the underlying raw variable, not
# the flipped avg0t1/avg4t5/etc. Step 5 has its own post-flip sanity check.

# econ variables are showing steady improvements, but nothing like a spike in 2017
df1 <- colvars_cdfw %>%
  group_by(year) %>%
  dplyr::summarize(
    across(
      c("IndRur_0t1", "PIB_2t3", "IDF_2t3", "DisBog_4t5"),
      ~mean(.x, na.rm = TRUE)
    )
  )

# crime factor scores as expected going down
df2 <- colvars_cdfw %>%
  group_by(year) %>%
  dplyr::summarize(
    across("ViolInd_1011",
           ~mean(., na.rm = TRUE))) # Criteria 10-11

# ruling party shows a dip in 2017
df3 <- colvars_cdfw %>%
  group_by(year) %>%
  dplyr::summarize(
    across(c("DenPob_12", "lAR_13pkm", "DisMer_13", "PropInd_14", "RulPar_15t16"),
           ~mean(., na.rm = TRUE))) # Criteria 12-16

# Reshape each wide year-means table (one column per variable) into long format
# so ggplot can map "variable" to color and draw one line per criterion.
df1 <- df1 %>%
  pivot_longer(-year, names_to = "variable", values_to = "value")
df2 <- df2 %>%
  pivot_longer(-year, names_to = "variable", values_to = "value")
df3 <- df3 %>%
  pivot_longer(-year, names_to = "variable", values_to = "value")

# Adjust data source as needed:
ggplot(df1, aes(x = year, y = value, color = variable)) + geom_line()
ggplot(df2, aes(x = year, y = value, color = variable)) + geom_line()
ggplot(df3, aes(x = year, y = value, color = variable)) + geom_line()



#---- Calculate averages (2t3) and rename variables----
# Caveat: by averaging, we are losing variation. It would be good to avoid this step.
#
# Criterion 13 (roads-per-area + market proximity) is deliberately NOT averaged here anymore
# (2026-07-26): one of its two components, DisMer_13, needs the direction flip that now
# happens in Step 5, and flipping AFTER averaging would not be equivalent to flipping
# BEFORE -- it would also flip lAR_13pkm, which was never supposed to be flipped. lAR_13pkm
# and DisMer_13 pass through this script separately, raw, under their original names; Step 5
# flips DisMer_13 and builds avg13 from the two there.
averages_df <- colvars_cdfw %>%
  # Criterion 2-3 has two component variables (GDP+IDF) with no separate weighting scheme
  # downstream, so they're collapsed here into a single 0-1 score by simple averaging.
  mutate(avg2t3 = (PIB_2t3 + IDF_2t3) / 2) %>%
  select(-PIB_2t3, -IDF_2t3) %>%
  rename(avg10t11 = ViolInd_1011) %>%
  # Rename all remaining single-variable criteria columns to the "avg<N>" naming convention,
  # so every criterion column downstream follows one pattern. .cols targets columns 4 onward
  # (i.e. everything after MPIO_CDPMP/year/DPTO_CCDGO) that don't already start with "avg",
  # excluding lAR_13pkm/DisMer_13 (the avg13 pair) so they keep their raw names -- Step 5
  # needs to find them by these exact names to build avg13 itself. .fn strips the variable's
  # leading label and keeps only the trailing digits (e.g. "IndRur_0t1" -> "avg0t1").
  rename_with(
    .fn = ~ paste0("avg", str_extract(., "\\d.*$")),
    .cols = 4:last_col() & !starts_with("avg") & !any_of(c("lAR_13pkm", "DisMer_13"))
  ) %>%
  relocate(avg2t3, .after = DPTO_CCDGO)

summary(averages_df)


##---- visualize means per variable ----
# Same idea as df1/df2/df3 above but now on the final renamed/averaged criteria
# set (all 0-16), in one combined plot instead of split groups.
dfavg <- averages_df %>%
  group_by(year) %>%
  dplyr::summarize(
    # Option A: Average everything that is numeric (Safest)
    # (relies on MPIO_CDPMP/DPTO_CCDGO being non-numeric IDs so they're excluded automatically)
    across(where(is.numeric), ~mean(., na.rm = TRUE)),
    .groups = "drop"
  )

dfavg2 <- dfavg %>%
  pivot_longer(-year, names_to = "variable", values_to = "value")

# Grab just the last year's point per variable, used to place a text label
# at the end of each line instead of relying on a legend.
label_data <- dfavg2 %>%
  group_by(variable) %>%
  filter(year == max(year)) %>%
  ungroup()

CDF_vars <- ggplot(dfavg2, aes(x = year, y = value, group = variable, color = variable)) +
  geom_line(linewidth = 1) +
  geom_text(
    data = label_data,
    aes(label = variable),
    hjust = -0.1,
    size = 3.5,
    fontface = "bold"
  ) +
  # Extend the x-axis past the last year so the end-of-line labels have room to render
  xlim(min(dfavg2$year), max(dfavg2$year) + 1.5) +
  labs(
    title = "Average Trends Over Time (Criteria 0-16)",
    subtitle = "0 = Associated with Less Democracy; 1 = Associated with More Democracy",
    x = "Year",
    y = "Mean Value (Average across Municipalities)",
    color = "Variable"
  ) +
  theme_minimal() +
  theme(legend.position = "none") # legend redundant given the end-of-line labels

# The most stable variables (those that hover around 0.5) are less informative than those that are further away: 2t3, 10t11, 14, 15t16.
ggsave("G:/Shared drives/snvdem/snvdem-col/data/panel/03_geocoded_panel/01_clean_geocoded/imgs/CDF-vars.png", CDF_vars, width = 14, height = 10, dpi = 300, bg = "white")



#---- Write to rds----
# This is the actual output artifact of the script: the municipality-year panel
# of direction-corrected, averaged CDF criteria scores. Everything below this
# point is further exploratory analysis/plotting, not additional output.
write_rds(averages_df, file = "G:/Shared drives/snvdem/snvdem-col/data/panel/03_geocoded_panel/01_clean_geocoded/CDF_averages.rds")

# Visualizations ----
# Faceted "spaghetti plot": one panel per criterion, showing individual
# municipality trajectories (noise) behind the national mean (signal), so it's
# possible to see whether a stable national average is masking municipal variation.

# 1. Sample 50 municipalities for the background lines (fixed seed for reproducibility)
set.seed(42)
sample_mpios <- sample(unique(averages_df$MPIO_CDPMP), 50)

# 2. Pivot to long format for ggplot (one row per municipality-year-criterion)
df_long2 <- averages_df %>%
  pivot_longer(starts_with("avg"), names_to = "variable", values_to = "value")

# 3. Create the faceted spaghetti plot
trends <- ggplot(df_long2, aes(x = year, y = value)) +
  # Background spaghetti lines (50 sampled municipalities)
  geom_line(data = df_long2 %>% filter(MPIO_CDPMP %in% sample_mpios),
            aes(group = MPIO_CDPMP), color = "gray80", alpha = 0.4, linewidth = 0.3) +
  # Bold National Mean line (stat_summary recomputes the mean per year/variable
  # across ALL municipalities, not just the 50 sampled for the background lines)
  stat_summary(fun = mean, geom = "line", aes(color = variable), linewidth = 1.2) +
  # Add a 0.5 reference line (midpoint of the standardized 0-1 CDF scale)
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray40", alpha = 0.5) +
  facet_wrap(~variable, scales = "fixed") + # fixed scales so panels are visually comparable
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Municipal Demographic & Democracy Trends (2000-2023)",
    subtitle = "Gray lines = 50 random municipalities; Colored line = National Average",
    x = "Year",
    y = "Standardized Value (0-1)",
    caption = "Standardized via CDF (Rank/N)"
  ) +
  theme_minimal() +
  theme(legend.position = "none", strip.text = element_text(face = "bold")) # facet labels replace the legend

ggsave("G:/Shared drives/snvdem/snvdem-col/data/panel/03_geocoded_panel/01_clean_geocoded/imgs/CDF-vars_trends.png", trends, width = 14, height = 10, dpi = 300, bg = "white")


# Several drops and rises in avg12 and avg14. Worth exploring more.
# Identify which specific municipalities drive the year-over-year jump in
# avg12 (population density) and avg14 (indigenous population share) between
# 2004 and 2005, to check whether it's a real shift or a data artifact.
visual_outliers <- averages_df %>%
  filter(year %in% c(2004, 2005)) %>%
  select(MPIO_CDPMP, year, avg12, avg14) %>%
  # Widen so each municipality has one row with both years' values as separate
  # columns (avg12_2004, avg12_2005, ...), enabling a same-row year-over-year diff
  pivot_wider(names_from = year, values_from = c(avg12, avg14)) %>%
  mutate(
    diff12 = abs(`avg12_2005` - `avg12_2004`),
    diff14 = abs(`avg14_2005` - `avg14_2004`)
  ) %>%
  # Sort by the single biggest jump in either category
  arrange(desc(pmax(diff12, diff14))) %>%
  slice_head(n = 10)

print(visual_outliers)

# 1. Identify the 'jumpers' and 'extremes'
extreme_mpios <- visual_outliers$MPIO_CDPMP

# 2. Pick 40 random mpios + the 10 biggest jumpers, so the re-plot below is
# guaranteed to show the outlier municipalities rather than risk sampling past them
set.seed(42)
representative_sample <- c(
  sample(setdiff(unique(averages_df$MPIO_CDPMP), extreme_mpios), 40),
  extreme_mpios
)

# 3. Re-plot using this 'Smart Sample', with a vertical marker at the 2004/2005
# boundary (2005.5) to visually locate the jump identified above
ggplot(df_long2 %>% filter(MPIO_CDPMP %in% representative_sample),
       aes(x = year, y = value)) +
  geom_line(aes(group = MPIO_CDPMP), color = "gray80", alpha = 0.5) +
  stat_summary(fun = mean, geom = "line", aes(color = variable), linewidth = 1.2) +
  facet_wrap(~variable) +
  geom_vline(xintercept = 2005.5, linetype = "dotted", color = "firebrick", alpha = 0.6)




# Understanding new variables ----
# Probes the relationship between avg2t3 (development: GDP + IDF) and
# avg10t11 (non-violence) beyond the linear correlation matrices computed at
# the top of the script, since CDF-ranked data can hide non-linear or
# non-constant (heteroskedastic) relationships that a single Pearson r would miss.
library(quantreg)

# Spearman's rank correlation measures strength of monotonic relationships, not linear ones
cor(averages_df$avg2t3, averages_df$avg10t11, method = "spearman")

# Hexbin plot will show where data is concentrated
ggplot(averages_df, aes(x = avg10t11, y = avg2t3)) +
  # Create hexbins; 'bins' controls the granularity (higher = smaller hexes)
  geom_hex(bins = 50) +
  # Use a color scale that makes high-density areas stand out
  scale_fill_viridis_c(option = "magma") +
  # Add the smoothing line on top to show the average trend
  geom_smooth(method = "loess", color = "cyan", se = FALSE, size = 1) +
  theme_minimal() +
  labs(title = "Hexbin Density of Development vs. Unrest (CDF Ranks)",
       subtitle = "Brighter areas represent higher concentrations of observations",
       x = "Unrest (avg10t11 Rank)",
       y = "Development (avg2t3 Rank)",
       fill = "Frequency")


# Quantile regression
# Fits separate regression lines at the 10th, 50th (median), and 90th
# percentiles of avg2t3 conditional on avg10t11, rather than just one mean-based
# OLS line -- this reveals if the development/non-violence relationship differs
# for the least- vs. most-developed municipalities (e.g. a widening/narrowing
# spread), which geom_smooth's single loess line can't show.
# tau = c(0.1, 0.5, 0.9) looks at the 10th, 50th (median), and 90th percentiles
qr_model <- rq(avg2t3 ~ avg10t11, data = averages_df, tau = c(0.1, 0.5, 0.9))
summary(qr_model)

# 2. Add the quantile lines to your hexbin plot
ggplot(averages_df, aes(x = avg10t11, y = avg2t3)) +
  geom_hex(bins = 50) +
  scale_fill_viridis_c(option = "magma") +
  # Add Quantile lines (10th, 50th, and 90th percentiles)
  geom_quantile(quantiles = c(0.1, 0.5, 0.9),
                color = "cyan",
                size = 1,
                alpha = 0.8) +
  theme_minimal() +
  labs(
    title = "Quantile Regression: Development vs. Non-violence",
    subtitle = "Lines represent the 10th, 50th, and 90th percentiles of Development",
    x = "Unrest (avg10t11 Rank)",
    y = "Development (avg2t3 Rank)"
  )

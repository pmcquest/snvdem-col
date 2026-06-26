# Generating benchmarked data for Colombia

library(tidyverse)
library(haven)

# Loading municipal indices for Colombia
index <- readRDS("Indices_both2.rds")

# Loading national-subnational means and ranges for all countries
# elections
SNEL <- read_dta("snlsffHPD.dta")

# civil liberties
SNCL <- read_dta("SNHPD.dta")

# Filter these for Col
SNELcol <- filter(SNEL, country_text_id=="COL" & year>1999)
SNCLcol <- filter(SNCL, country_text_id=="COL" & year>1999)

# Merge them
SNcol <- merge(SNELcol, SNCLcol, by = c("country_text_id", "year"), all.y = TRUE)
SNcol <- SNcol %>%
  select(country_text_id, year, CLSNmean, wtdCL_range, v2elffelr, weighted_range)

# merge with index
col_benchmark <- merge(SNcol, index, by="year", all.y = TRUE)

# Calculate the benchmarked indices
# First calculate needed means and quantiles

SNcol_by_year <- col_benchmark %>%
  group_by(year) %>%
  summarize(CLSNyrmean = mean(CLSNmean, na.rm = TRUE),
            snelectyrmean = mean(snelect, na.rm = TRUE),
            CLrange_975_025 = quantile(sncivlib, 0.975, na.rm = TRUE) - quantile(sncivlib, 0.025, na.rm = TRUE),
            ELrange_975_025 = quantile(snelect, 0.975, na.rm = TRUE) - quantile(snelect, 0.025, na.rm = TRUE))

# Getting a mean for CLrange_975_025 to replace a few missing values
mean(SNcol_by_year$CLrange_975_025, na.rm=TRUE)
# = 0.2443505
SNcol_by_year$CLrange_975_025[is.na(SNcol_by_year$CLrange_975_025)] <- 0.2443505

# Merge the aggregated values into the municipal-level data
col_benchmark <- merge(col_benchmark, SNcol_by_year, by="year", all.x = TRUE)

CLbench <- col_benchmark %>%
  mutate(CL_col_mt = CLSNmean + (sncivlib - CLSNyrmean)*wtdCL_range/CLrange_975_025,
         EL_col_mt = v2elffelr + (snelect - snelectyrmean)*weighted_range/ELrange_975_025)

# Visualizing them

snclplot<- ggplot(CLbench, aes(x = as.factor(year), y = CL_col_mt)) +
  geom_boxplot(stat = "boxplot", fill = "skyblue") +
  labs(title = "Distribution of Colombian municipalities on civil liberties",
       x = "", y = "Municipal-level civil liberties index") +
  scale_y_continuous(limits = c(0,1)) +
  theme_light()
ggsave(snclplot, filename = "snclplot.png", device = "png", height = 8, width = 12, units = "in", dpi = 300)


snelplot <- ggplot(CLbench, aes(x = as.factor(year), y = EL_col_mt)) +
  geom_boxplot(stat = "boxplot", fill = "salmon") +
  labs(title = "Distribution of Colombian municipalities on free and fair elections",
       x = "", y = "Municipal-level free & fair elections index") +
  scale_y_continuous(limits = c(-3.5, 3.5)) +
  theme_light()
ggsave(snelplot, filename = "snelplot.png", device = "png", height = 8, width = 12, units = "in", dpi = 300)

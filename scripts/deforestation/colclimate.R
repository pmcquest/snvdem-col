library(ColOpenData)
library(sf)
library(leaflet)
library(ggplot2)

# climate vignette: https://epiverse-trace.github.io/ColOpenData/articles/climate_data.html


#datasets_cli <- list_datasets("climate")
# head(datasets_cli)
dict_climate <- dictionary("IDEAM_CLIMATE_2023_MAY")
head(dict_climate)


# 3 methods for using data: station data, ROI, or municipality/department

# Method 3

divipola_municipality_code("CALDAS", "MANIZALES")

max_temperature_mpio <- download_climate(
  code = "17001",
  start_date = "2013-01-01",
  end_date = "2016-12-31",
  tag = "TMX_CON"
) %>% aggregate_climate("month")


ggplot(data = max_temperature_mpio) +
  geom_line(aes(x = date, y = value, group = station), color = "#106ba0") +
  ggtitle("Dry-bulb Temperature") +
  xlab("Date") +
  ylab("Dry-bulb temperature [C]") +
  theme_bw() +
  facet_grid(rows = vars(station))

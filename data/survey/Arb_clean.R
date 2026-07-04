#Arboleda data 

# In Feb. 2024, M. Coppedge (MC) went to the Universidad Sergio Arboleda in Bogota, Colombia, to teach a brief seminar. He asked attendees to participate in a survey on subnational democracy in Colombia. The survey instrument is here: https://drive.google.com/file/d/1JkCBwaYLvYS-cX7c3hjlJJXblRRW1tW7/view?usp=sharing.

# The survey consisted of four questions, open-ended. The respondents were instructed to provide years and geographic locations where (1) elections were less free and fair, (2) elections were more free and fair, (3) civil liberties were stronger, and (4) civil liberties were weaker. 

# In total, MC received hand-written responses from 9 individuals. The results were then transcribed by P. McQuestion (PM) into a quantitative data frame. 
## While textually the 9 responses were no more than a few words or locations each, in some cases they refer to multi-departmental or multi-municipal regions (e.g., Uraba Antioqueno) which have relatively specific geographic boundaries. In such cases, PM consulted with secondary sources to identify the municipalities included in these regions, according to official sources like DANE. For dates preceding 1990, PM elected to simplify these observations to time spans using "Year Start" and "Year end" variables, rather than creating individual municipality-year observations. 

# The "Arb_final" df contains 1280 observations of the following 11 variables:
## id = generic number used to sort responses in order of their appearance in the scanned document provided by MC.
## Arb_id = Respondent (unique)
## Q_id = identifies one of the four questions: 
### 1 = elections less free and fair (EL)
### 2 = elections more free and fair (EM)
### 3 = civil liberties stronger (CS)
### 4 = civil liberties weaker (CW)
## year = individual year 1990 onwards
## year_start = year prior to 1990
## year_end = year prior to 1990
## depto = department name
## municipio = municipality name
## misc_notes = comments by PM on what was provided by the respondents
## DPTO_CCDGO = DANE department codes
## MPIO_CDPMP = DANE municipality codes

# Note: The data frame includes department and municipal codes (DANE) in order to link these responses to the main data frame for mapping subnational democracy in Colombia. Perhaps the majority of the responses were departments or regional units of analysis; joining these data to the main data frame will activate the municipalities within each department.

# These responses are not a representative sample of expert knowledge on subnational democracy, but inter-respondent agreement around specific years and locations could be used to produce data for validation.


#---- A. Cleaning survey data ----

library(readxl)
Arboleda_0224 <- read_excel("G:/Shared drives/snvdem/snvdem-col/data/survey/Arboleda-0224.xlsx")

# load main df for project
df_final <- readRDS("G:/Shared drives/snvdem/snvdem-col/data/panel/final_data/df_final.rds")

# Load the necessary libraries
library(dplyr)
library(stringr)


# Function for reading chr variables and matching:
# This ensures exact matches for text keys (depto, provincia, municipio)

clean_names <- function(data) {
  data %>%
    mutate(
      depto = str_to_upper(trimws(depto)),
      municipio = str_to_upper(trimws(municipio))
    )
}

df_final_clean <- clean_names(df_final)
Arboleda_clean <- clean_names(Arboleda_0224)

# Create Lookup Tables (using cleaned data)
# 1. Department Lookup
lookup_depto <- df_final_clean %>%
  select(depto, DPTO_CCDGO) %>%
  distinct()
# 2. Municipal Lookup (Simplified two-key join)
lookup_mpio_simple <- df_final_clean %>%
  select(depto, municipio, MPIO_CDPMP) %>%
  distinct()

# Perform the Joins
Arboleda_updated <- Arboleda_clean %>%
  left_join(lookup_depto, by = "depto") %>%
  left_join(lookup_mpio_simple, 
            by = c("depto", "municipio"))

##---- Write to rds----
# saveRDS(Arboleda_updated, file = "G:/Shared drives/snvdem/snvdem-col/data/survey/Arb_final.rds")

#----B. Expand the survey data----
Arb_final <- readRDS("G:/Shared drives/snvdem/snvdem-col/data/survey/Arb_final.rds")
df_final <- readRDS("G:/Shared drives/snvdem/snvdem-col/data/panel/final_data/df_final.rds")

library(dplyr)

# Ensure codes are treated as characters to prevent issues with leading zeros
depto_mpio_code_map <- df_final %>%
  select(DPTO_CCDGO, MPIO_CDPMP) %>%
  distinct() %>%
  # Filter out any potentially invalid or missing codes in the map
  filter(!is.na(DPTO_CCDGO), !is.na(MPIO_CDPMP), 
         DPTO_CCDGO != "", MPIO_CDPMP != "")
# Records that already have a specific MPIO_CDPMP code identified
Arb_mpio <- Arb_final %>%
  filter(!is.na(MPIO_CDPMP), MPIO_CDPMP != "")
# Records that only have DPTO_CCDGO (MPIO_CDPMP is NA or empty)
Arb_depto_only <- Arb_final %>%
  filter(is.na(MPIO_CDPMP) | MPIO_CDPMP == "") %>%
  select(-MPIO_CDPMP)

# Expansion step:
Arb_depto_expanded <- Arb_depto_only %>%
  # Left join to the map on the 'DPTO_CCDGO' code.
  # This replicates every row by the number of municipalities in that department.
  left_join(depto_mpio_code_map, by = "DPTO_CCDGO") %>%
  # Ensure the join was successful (i.e., a new MPIO_CDPMP was assigned)
  filter(!is.na(MPIO_CDPMP))

# Combine the two resulting data frames
Arb_expanded_final <- bind_rows(Arb_mpio, Arb_depto_expanded)


#----C. Calculate inter-respondent agreement ----

# Calculate the agreement count for each specific location, year, and question
agreement_summary <- Arb_expanded_final %>%
  # Group by the finest level: municipality, year, and question ID
  group_by(DPTO_CCDGO, MPIO_CDPMP, Q_id, year) %>%
  summarise(
    # Count the number of unique respondents (Arb_id) who reported this combination
    respondent_count = n_distinct(Arb_id),
    .groups = 'drop'
  ) %>%
  # Calculate the percentage of agreement (since you have 9 total respondents)
  mutate(
    agreement_pct = (respondent_count / 9) * 100
  ) %>%
  # Sort to see the strongest agreements first
  arrange(desc(agreement_pct))

# Join the summary back to df_final to get the location names
final_map_data <- agreement_summary %>%
  left_join(df_final %>% select(DPTO_CCDGO, MPIO_CDPMP, depto, municipio) %>% distinct(),
            by = c("DPTO_CCDGO", "MPIO_CDPMP")) %>%
  
  # Reorder columns for clarity
  select(Q_id, year, depto, municipio, DPTO_CCDGO, MPIO_CDPMP, 
         respondent_count, agreement_pct)

##---- Write to rds----
# saveRDS(final_map_data, file = "G:/Shared drives/snvdem/snvdem-col/data/survey/Arb_agree.rds")

#---- D. Map the survey data ----

# Load the required libraries (Ensure gganimate and magick are installed)
library(sf)
library(ggplot2)
library(dplyr)
library(tidyr)
library(gganimate)
library(magick) # Required for the magick_renderer

# 0. Load Data
Arb_agree <- readRDS("G:/Shared drives/snvdem/snvdem-col/data/survey/Arb_agree.rds")
Arb_agree <- Arb_agree %>%
  select(-DPTO_CCDGO)

# Load the municipal boundaries
col <- st_read("G:/Shared drives/snvdem/snvdem-col/data/geospatial/2018pmq/BaseLayer/MGN_ANM_MPIOS.shp")
col <- col %>% select(1:8)

# Merge the Arboleda and Shp data
full_map_data <- col %>%
  mutate(MPIO_CDPMP = as.character(MPIO_CDPMP)) %>%
  left_join(Arb_agree, by = "MPIO_CDPMP") %>%
    mutate(year = as.integer(as.character(year))) %>%
  filter(!is.na(year)) # Remove any rows where year failed to convert

##---- D1. Create Yearly/Q_id Maps ----
map_directory <- "G:/Shared drives/snvdem/snvdem-col/data/survey/maps/agreement-years" 
file_prefix <- "agreement_"
file_suffix <- "_map.png"

# Calculate Global Min and Max (Fixed for percentage)
global_min <- 0
global_max <- 100

# Function to map Q_id to a readable name
get_q_title <- function(q_id) {
  titles <- c("1" = "Less Free/Fair Elections", "2" = "More Free/Fair Elections", 
              "3" = "Stronger Civil Liberties", "4" = "Weaker Civil Liberties")
  return(titles[as.character(q_id)])
}

# Loop over the unique combinations of Q_id and year present in the data.
plot_list <- full_map_data %>%
  st_drop_geometry() %>% # Drop geometry for faster grouping
  distinct(Q_id, year) %>%
  # Iterate over each combination
  group_by(Q_id, year) %>%
  group_map(~ {
    current_q_id <- .y$Q_id
    current_year <- .y$year
    # Filter the main map data for the current iteration
    year_data <- full_map_data %>%
      filter(Q_id == current_q_id, year == current_year) %>%
      # Replace NA agreement values (municipalities with no response that year) with 0
      mutate(agreement_pct = coalesce(agreement_pct, 0))
    # Define title
    plot_title <- paste0("Agreement on '", get_q_title(current_q_id), "' - Year ", current_year)
    year_plot <- ggplot() +
      geom_sf(data = col, fill = "gray95", color = "gray60", linewidth = 0.05) + 
      geom_sf(data = year_data, 
              aes(fill = agreement_pct), 
              color = "gray60", # Ensure borders are drawn on the data layer too, or use "transparent"
              linewidth = 0.05) +
      theme_void() +
      # ... (Theme settings remain the same) ...
      theme(panel.background = element_rect(color = "transparent", fill = "white"),
            plot.caption = element_text(size = 12),
            legend.background = element_rect(fill = "white", color = "lightgray"),
            legend.key = element_rect(fill = "white", color = "lightgray")) +
      scale_fill_viridis_c(option = "magma", direction = -1, limits = c(global_min, global_max), 
                           name = "Agreement (%)") +
      labs(title = plot_title)
    
    # Save the plot
    filename <- file.path(map_directory, paste0(file_prefix, "Q", current_q_id, "_Y", current_year, file_suffix))
    ggsave(filename = filename, plot = year_plot, height = 8, width = 10, device = "png", units = "in")
    
    print(paste("Map created and saved for Q_id:", current_q_id, "Year:", current_year))
    return(year_plot)
  })

###----Faceted Plots (Example Q_id = 1) ----
q1_map_data <- full_map_data %>% 
  filter(Q_id == 1) %>% 
  mutate(agreement_pct = coalesce(agreement_pct, 0)) # Clean NA for faceted map

faceted_plot <- ggplot() +
  geom_sf(data = col, 
          fill = "gray95", 
          color = "gray60", 
          linewidth = 0.05) + 
  geom_sf(data = q1_map_data, 
          aes(fill = agreement_pct), 
          color = "gray60",
          linewidth = 0.05) +
  theme_void() +
  scale_fill_viridis_c(option = "magma", direction = -1, limits = c(global_min, global_max)) +
  labs(fill = "Agreement (%)", title = "Agreement on 'Less Free/Fair Elections' Across Years") +
  facet_wrap(~ year, ncol = 7) 

ggsave(filename = file.path(map_directory, "faceted_agreement_Q1.png"), 
       plot = faceted_plot, height = 10, width = 15, device = "png", units = "in")

##----D2. Create EL and CL data----


# 1. Pivot and Calculate Net Differences (-100 to +100)
Arb_wide <- Arb_agree %>%
  pivot_wider(
    names_from = Q_id,
    values_from = agreement_pct,
    names_prefix = "Q"
  ) %>%
  mutate(across(starts_with("Q"), ~coalesce(., 0))) %>%
  mutate(
    # Net Elections Score (EL_net): Q2 (More free/fair) - Q1 (Less free/fair)
    EL_net = Q2 - Q1,
    
    # Net Civil Liberties Score (CL_net): Q3 (Stronger CL) - Q4 (Weaker CL)
    CL_net = Q3 - Q4
  )

# 2. Calculate Scaled Scores (0 to 100) and the Combined Score
Arb_scaled_scores <- Arb_wide %>%
  mutate(
    # Scaled Formula: 50 + (Net Score / 2)
    EL_scaled = 50 + (EL_net / 2),
    CL_scaled = 50 + (CL_net / 2),
    # Combined Score (Avg of EL_scaled and CL_scaled)
    COMBINED_scaled = (EL_scaled + CL_scaled) / 2
  ) %>%
  # Keep only the keys, year, and the three scaled scores
  select(MPIO_CDPMP, year, EL_scaled, CL_scaled, COMBINED_scaled) %>%
  # Pivot into a long format for the map loop
  pivot_longer(
    cols = c(EL_scaled, CL_scaled, COMBINED_scaled),
    names_to = "Metric",
    values_to = "Scaled_Score"
  )

# Full scaled map...
full_scaled_map_data <- col %>%
  left_join(Arb_scaled_scores, by = "MPIO_CDPMP") %>%
  mutate(year = as.integer(as.character(year))) %>%
  filter(!is.na(year))

# 1. Setup
map_directory <- "G:/Shared drives/snvdem/snvdem-col/data/survey/maps/ELCL" 
file_prefix <- "net_agreement_"
file_suffix <- "_map.png"

# Pre-calculate the dynamic limits for ALL three metrics (EL, CL, COMBINED)
dynamic_limits <- full_scaled_map_data %>%
  st_drop_geometry() %>%
  group_by(Metric) %>%
  summarise(
    # Find the global min and max for the metric
    Global_Min = min(Scaled_Score, na.rm = TRUE),
    Global_Max = max(Scaled_Score, na.rm = TRUE),
    
    # Calculate the largest deviation from the neutral center (50)
    Max_Deviation = max(abs(Global_Max - 50), abs(50 - Global_Min)),
    
    # Set the new symmetric limits
    Plot_Min = 50 - Max_Deviation,
    Plot_Max = 50 + Max_Deviation
  ) %>%
  ungroup()

# Display the limits to see the new range (for example)
print(dynamic_limits)

# 2. Map Generation Function for Scaled Scores (0-100) - CORRECTED
create_scaled_map <- function(data_to_plot, metric_name, current_year, limit_df) {
  
  details <- get_map_details(metric_name)
  
  # --- CORRECTED: Look up dynamic limits ---
  limits <- limit_df %>% filter(Metric == metric_name)
  plot_min <- limits$Plot_Min
  plot_max <- limits$Plot_Max
  
  year_plot <- ggplot() +
    geom_sf(data = col, fill = "gray95", color = "gray60", linewidth = 0.05) +
    geom_sf(data = data_to_plot, aes(fill = Scaled_Score), color = "gray60", linewidth = 0.05) +
    theme_void() +
    theme(legend.position = "bottom",
          plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5),
          axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank()) +
    
    # Use the adjusted limits
    scale_fill_distiller(palette = "RdYlBu", 
                         limits = c(plot_min, plot_max), # Uses dynamic limits
                         na.value = "gray95",
                         breaks = round(seq(plot_min, plot_max, length.out = 5)), # Set custom breaks
                         name = "Net Agreement (50 = Neutral)") +
    labs(title = paste(details$title, "- Year", current_year))
  
  # Save the plot
  filename <- file.path(map_directory, paste0(file_prefix, details$label, "_Y", current_year, file_suffix))
  ggsave(filename = filename, plot = year_plot, height = 8, width = 10, device = "png", units = "in")
  
  return(year_plot)
}


# 3. Loop and Generate All Maps - CORRECTED
plot_list_scaled <- scaled_metrics_to_map %>%
  group_by(Metric, year) %>%
  group_map(~ {
    current_metric <- .y$Metric
    current_year <- .y$year
    
    data_filtered <- full_scaled_map_data %>%
      filter(Metric == current_metric, year == current_year) %>%
      mutate(Scaled_Score = coalesce(Scaled_Score, 50)) 
    
    # Pass the pre-calculated limits to the function
    plot <- create_scaled_map(data_filtered, current_metric, current_year, dynamic_limits) 
    
    print(paste("Map created and saved for", current_metric, "Year:", current_year))
    return(plot)
  })

###----Animated combined map ----
library(ggplot2)
library(gganimate)
library(magick)

# Set the target metric
TARGET_METRIC <- "COMBINED_scaled"

# Filter data for the target metric
combined_map_data <- full_scaled_map_data %>% 
  filter(Metric == TARGET_METRIC) %>%
  # Replace NA scores with 50 (neutral point) for smooth animation
  mutate(Scaled_Score = coalesce(Scaled_Score, 50))

# Get the dynamic limits for the combined score
combined_limits <- dynamic_limits %>% 
  filter(Metric == TARGET_METRIC)

plot_min <- combined_limits$Plot_Min
plot_max <- combined_limits$Plot_Max

# Define the animation parameters
num_years <- n_distinct(combined_map_data$year)
total_frames <- num_years * 5 # 5 frames per year transition

# Create the base map plot
base_plot_combined <- ggplot() +
  geom_sf(data = col, fill = "gray95", color = "gray60", linewidth = 0.05) +
  geom_sf(data = combined_map_data, 
          aes(fill = Scaled_Score), 
          color = "gray60", 
          linewidth = 0.05) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        legend.position = "bottom",
        plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm")) +
  scale_fill_distiller(palette = "RdYlBu", 
                       limits = c(plot_min, plot_max),
                       na.value = "gray95",
                       breaks = round(seq(plot_min, plot_max, length.out = 5)),
                       name = "Net Agreement (50 = Neutral)") +
  
  labs(title = "Overall Net Score Over Time",
       subtitle = 'Year: {closest_state}') # gganimate variable


# Apply the animation layer
animated_map_combined <- base_plot_combined +
  transition_states(year, transition_length = 1, state_length = 0.5) +
  labs(title = "Overall Combined Score: Year {closest_state}")

# Render and Save the Animation
animation_filename <- file.path(map_directory, "animated_net_score_COMBINED.gif")

animate(animated_map_combined, 
        filename = animation_filename, 
        nframes = total_frames, 
        fps = 5, 
        height = 600, width = 800, units = "px", 
        renderer = magick_renderer()) 

cat(paste0("GIF animation saved as: ", animation_filename, "\n"))

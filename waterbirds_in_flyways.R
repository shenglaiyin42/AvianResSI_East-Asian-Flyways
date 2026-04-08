# ---
# Title: Integrated Waterbird Taxonomic Enrichment & Multi-Panel Visualization
# Author: S.Y.
# Date: 2026-04-08
# Description: This script filters flyway data, joins scientific taxonomy, 
#              and generates a 3-panel publication-quality figure.
# ---

# ---- 1. SETUP & LIBRARIES ----
# Define required packages for data handling and advanced visualization
packages <- c("tidyverse", "janitor", "scales", "viridis", "patchwork", "ggrepel")

# Install missing packages automatically if they are not already present
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load libraries into the workspace
library(tidyverse) # Core data processing and ggplot2
library(janitor)   # Clean column names
library(scales)    # Format percentages for labels
library(viridis)   # Scientific, colorblind-friendly palettes
library(patchwork) # Multi-panel plot arrangement
library(ggrepel)   # Smart label segments for donut charts

# Define Global Directories
# Input directory for raw CSV files
input_dir <- "/Users/shenglaiyin/Library/CloudStorage/OneDrive-UniversityofOklahoma/Avian Research Special Issue/John's review/AvianResSI_script/dat"
# Unified export directory for all CSV and TIFF outputs
export_path <- "/Users/shenglaiyin/Library/CloudStorage/OneDrive-UniversityofOklahoma/Avian Research Special Issue/John's review/AvianResSI_script/export"

# Set working directory for inputs
setwd(input_dir)
curr_date <- format(Sys.Date(), "%Y-%m-%d")

# Define Global Aesthetics
# Manual IUCN color palette (standard conservation colors)
iucn_colors <- c(
  "Critically Endangered" = "#7a0000", 
  "Endangered"            = "#d40000", 
  "Vulnerable"            = "#ff9e17", 
  "Near Threatened"       = "#d5b43c", 
  "Least Concern"         = "#a6cee3"
)

# Strict order for IUCN legend (Excluding 'Data Deficient' as requested)
iucn_order <- c("Critically Endangered", "Endangered", "Vulnerable", 
                "Near Threatened", "Least Concern")

# ---- 2. DATA IMPORT & CLEANING ----

# Import Master HBW list and standardize names
hbw_master <- read_csv("HBW_species_list.csv") %>% 
  clean_names() %>%
  select(
    scientific_name, 
    order_taxa = order, 
    family_taxa = family_name  # Based on user input, this contains Latin names (e.g. Anatidae)
  ) %>%
  mutate(sci_name_norm = str_to_lower(str_trim(scientific_name))) %>%
  distinct(sci_name_norm, .keep_all = TRUE)

# Import Flyway datasets
caf_data <- read_csv("Central-Asia-Flyway-Species.csv") %>% 
  clean_names() %>%
  mutate(sci_name_norm = str_to_lower(str_trim(scientific_name)))

eaaf_data <- read_csv("East-Asia-Australasia-Flyway-Species.csv") %>% 
  clean_names() %>%
  mutate(sci_name_norm = str_to_lower(str_trim(scientific_name)))

# ---- 3. FILTERING LOGIC ----

# Define taxonomic orders classified as waterbirds
waterbird_orders <- str_to_lower(c(
  "Anseriformes", "Charadriiformes", "Gaviiformes", "Podicipediformes", 
  "Procellariiformes", "Sphenisciformes", "Suliformes", "Pelecaniformes", 
  "Ciconiiformes", "Phoenicopteriformes", "Gruiformes"
))

# Regex pattern for keyword-based filtering (common names)
waterbird_keywords <- c(
  "Duck", "Goose", "Swan", "Gull", "Tern", "Plover", "Sandpiper", 
  "Heron", "Egret", "Ibis", "Grebe", "Cormorant", "Pelican", 
  "Stork", "Snipe", "Rail", "Spoonbill", "Frigatebird", "Booby", 
  "Gannet", "Shearwater", "Petrel", "Albatross", "Crake"
)
pattern <- paste0("\\b(", paste(waterbird_keywords, collapse = "|"), ")\\b")

# Function: process_waterbirds
# Parameters: data (flyway dataframe)
# Output: Dataframe enriched with HBW scientific family/order
process_waterbirds <- function(data) {
  # 1. Match by Order found in HBW master
  target_sci_names <- hbw_master %>% 
    filter(str_to_lower(order_taxa) %in% waterbird_orders) %>% 
    pull(sci_name_norm)
  
  match_by_order <- data %>% filter(sci_name_norm %in% target_sci_names)
  
  # 2. Match by Keywords in Common Name
  match_by_keyword <- data %>%
    filter(str_detect(common_name, regex(pattern, ignore_case = TRUE)) | 
             str_detect(family_name, regex(pattern, ignore_case = TRUE)))
  
  # 3. Combine unique species and join taxonomy
  final_list <- bind_rows(match_by_order, match_by_keyword) %>%
    distinct(sci_name_norm, .keep_all = TRUE) %>%
    left_join(hbw_master %>% select(sci_name_norm, order_taxa, family_taxa), 
              by = "sci_name_norm") %>%
    select(-sci_name_norm)
  
  return(final_list)
}

# Run processing for both flyways
caf_final  <- process_waterbirds(caf_data)
eaaf_final <- process_waterbirds(eaaf_data)

# Export cleaned CSVs to the export folder
write_csv(caf_final, file.path(export_path, paste0("CAF_Waterbirds_Enriched_", curr_date, ".csv")))
write_csv(eaaf_final, file.path(export_path, paste0("EAAF_Waterbirds_Enriched_", curr_date, ".csv")))

# ---- 4. VISUALIZATION: PANEL A (BAR CHART) ----

# Prepare combined data for comparative stacked bar
combined_flyway_data <- bind_rows(
  caf_final %>% mutate(flyway = "CAF"),
  eaaf_final %>% mutate(flyway = "EAAF")
) %>%
  group_by(flyway, family_taxa) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(flyway) %>%
  mutate(perc = count / sum(count))

# Create Panel A
panel_a <- ggplot(combined_flyway_data, aes(x = flyway, y = count, fill = family_taxa)) +
  geom_bar(stat = "identity", position = "stack", color = "white", size = 0.1) +
  # Labels: "18% Anatidae" on one single line
  geom_text(aes(label = ifelse(perc > 0.05, 
                               paste0(percent(perc, accuracy = 1), " ", family_taxa), 
                               "")), 
            position = position_stack(vjust = 0.5), # Vertical center of stack
            size = 6, color = "white", fontface = "bold") + 
  scale_fill_viridis_d(option = "D") + # Distinctive Viridis palette
  theme_bw() +
  labs(title = NULL, x = "Flyways", y = "Number of Species", fill = "Taxonomic Family") +
  theme(
    axis.text = element_text(size = 18), 
    axis.title = element_text(size = 20, face = "bold"), 
    legend.key.size = unit(0.8, "cm"), 
    legend.text = element_text(size = 14), 
    legend.title = element_text(face = "bold", size = 16)
  )

# ---- 5. VISUALIZATION: PANELS B & C (SYNCED DONUT CHARTS) ----

# Function: create_iucn_panel
# Parameters: df (flyway data), title (plot title)
# Output: IUCN status donut chart with synced labels and segments
create_iucn_panel <- function(df, title) {
  
  plot_data <- df %>%
    filter(iucn_red_list_category %in% iucn_order) %>%
    mutate(category = factor(iucn_red_list_category, levels = iucn_order)) %>%
    group_by(category) %>%
    summarise(count = n()) %>%
    mutate(perc = count / sum(count)) %>%
    # Reverse arrangement to sync with coord_polar direction
    arrange(desc(category)) %>% 
    mutate(ypos = cumsum(perc) - 0.5 * perc)
  
  ggplot(plot_data, aes(x = 1.4, y = perc, fill = category)) +
    geom_bar(stat = "identity", width = 0.6, color = "white") +
    coord_polar("y", start = 0, direction = -1) + # Direct correspondence to arrange(desc())
    xlim(0, 2.5) + # Space for segment growth
    theme_void() +
    scale_fill_manual(values = iucn_colors, breaks = iucn_order, limits = iucn_order) + 
    labs(title = title, fill = "IUCN Status") +
    # Repel labels with smart segments
    geom_text_repel(aes(x = 1.7, y = ypos, label = percent(perc, accuracy = 0.1)),
                    size = 7, fontface = "bold", 
                    nudge_x = 0.6, # Length of segment
                    segment.size = 0.5, 
                    segment.color = "grey30", 
                    direction = "y", 
                    min.segment.length = 0) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
      legend.text = element_text(size = 16),
      legend.title = element_text(size = 18, face = "bold")
    )
}

# Build conservation panels
panel_b <- create_iucn_panel(caf_final, "CAF Conservation Status")
panel_c <- create_iucn_panel(eaaf_final, "EAAF Conservation Status")

# ---- 6. MULTI-PANEL ASSEMBLY & TIFF EXPORT ----

# Arrange with patchwork: A top, B & C shared bottom
final_figure <- panel_a / (panel_b + panel_c) + 
  plot_layout(heights = c(1.6, 1), guides = "collect") + # 'collect' merges identical IUCN legends
  plot_annotation(tag_levels = 'A') & 
  theme(
    plot.tag = element_text(face = 'bold', size = 26), 
    legend.position = "right"
  )

# Define full filename
output_filename <- file.path(export_path, paste0("AvianRes_Integrated_Figure_", curr_date, ".tif"))

# Save as high-resolution 300 DPI TIFF
ggsave(
  filename = output_filename, 
  plot = final_figure, 
  device = "tiff", 
  dpi = 300, 
  width = 14, 
  height = 18, 
  units = "in",
  compression = "lzw"
)

cat("\nIntegration Successful. Enriched CSVs and Multi-panel Figure exported to:\n", export_path, "\n")

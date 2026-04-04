# ---
# Title: Flyway Comparison - Centered Density Legend
# Purpose: Top-left panel tags, leader lines, and date-appended filenames
# Author: S.Y.
# Date: 2026-04-04
# ---

# Section 1: Setup ----
library(tidyverse) # Loads ggplot2, dplyr, tidyr, stringr for data manipulation and plotting
library(scales)    # Provides formatting functions like comma() for axis labels
library(ggrepel)   # Essential for non-overlapping labels and connecting leader lines
library(patchwork) # Allows combining separate plots (A and B) into a single layout

# Path Configuration: Defining paths as variables to maintain a clean script
path_wd <- "/Users/shenglaiyin/Library/CloudStorage/OneDrive-UniversityofOklahoma/Avian Research Special Issue/John's review/AvianResSI_script"
path_dat <- file.path(path_wd, "dat")      # Directory for input data
path_export <- file.path(path_wd, "export") # Directory for final figure output

setwd(path_wd) # Sets the working directory for the session

# Section 2: Data Import and Processing ----
df <- read.csv(file.path(path_dat, "flyway_country_pop.csv")) # Reads the raw flyway data

# USER CONTROLS: Global variables to easily tweak the whole figure
threshold_percentile <- 0.75 # Top 25% densest countries will be labeled with text
global_font_size <- 9        # Baseline font size for the plots
common_limits <- c(1, 1e9)   # Ensures both axes start at 1 and end at 1 billion (log scale)

full_data <- df %>%
  filter(pop > 0, area_km2 > 0) %>% # Log scales cannot handle 0 or negative values
  mutate(country = case_when(
    # String cleaning: Renaming long country names for legend and label brevity
    country == "Republic of Korea" ~ "S. Korea",
    str_detect(country, "People's Republic|D\\.P\\.R|D. P. R.") ~ "N. Korea", # Regex to catch name variants
    country == "Lao People's Democratic Republic" ~ "Laos",
    country == "Russian Federation" ~ "Russia",
    country == "Hong Kong SAR, China" ~ "Hong Kong",
    country == "Macau SAR, China" ~ "Macau",
    country == "United Arab Emirates" ~ "UAE",
    country == "United States of America" ~ "USA",
    country == "Cocos (Keeling) Islands (Aus.)" ~ "Cocos Is.",
    country == "Christmas Island (Aus.)" ~ "Christmas Is.",
    country == "Ashmore and Cartier Islands (Aus.)" ~ "Ashmore Is.",
    country == "Norfolk Island (Aus.)" ~ "Norfolk Is.",
    country == "Papua New Guinea" ~ "PNG",
    country == "Islamic Republic of Iran" ~ "Iran", 
    country == "Kyrgyz Republic" ~ "Kyrgyz", 
    country == "Republic of Yemen" ~ "Yemen", 
    country == "Brunei Darussalam" ~ "Brunei", 
    
    TRUE ~ country # Keep original name if no match found
  )) %>%
  mutate(density = pop / area_km2) %>% # Create the density variable for bubble sizing
  group_by(flyway) %>% # Group by panel (CAF/EAAF) to calculate relative density
  mutate(is_high_density = density >= quantile(density, threshold_percentile)) %>% # Flag for labeling
  ungroup() # Remove grouping to prevent errors in subsequent operations

# Calculate universal size limits so a specific density value results in the same bubble size in both plots
size_limits <- c(min(full_data$density), max(full_data$density))

# Section 3: Plotting Function ----
create_flyway_plot <- function(data_subset, label_letter, flyway_name, include_density = FALSE) {
  
  p <- ggplot(data_subset, aes(x = area_km2, y = pop)) +
    # Add a reference line where population equals area (slope 1)
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
    # Draw bubbles: shape 21 allows for a fill color and a black border (stroke)
    geom_point(aes(size = density, fill = country), shape = 21, color = "black", stroke = 0.5) +
    # Labeling logic with connecting segments (leader lines)
    geom_text_repel(
      data = subset(data_subset, is_high_density == TRUE), # Only label flagged countries
      aes(label = country), 
      size = (global_font_size * 0.5), # Relative text sizing
      fontface = "bold", 
      box.padding = 0.8,       # Distance text must keep from other text boxes
      point.padding = 0.5,     # Distance text must keep from the bubbles
      max.overlaps = 20,       # Allow more overlaps to force more labels to appear
      # --- SEGMENT SETTINGS ---
      min.segment.length = 0,  # Forces lines to appear even if the text is very close to the bubble
      segment.color = "grey30", # Dark grey lines are less distracting than pure black
      segment.size = 0.6,      # Line thickness
      segment.alpha = 1,       # Fully opaque lines
      force = 2                # Repel force: higher values push labels further away, lengthening lines
    ) +
    # Axis scales: Logarithmic with comma-formatted labels
    scale_x_log10(labels = comma, limits = common_limits, expand = c(0, 0)) + 
    scale_y_log10(labels = comma, limits = common_limits, expand = c(0, 0)) +
    scale_fill_viridis_d(option = "turbo") + # Distinct colors for many countries
    # Bubble size scale: range defines the min/max diameter of circles in the plot
    scale_size_continuous(range = c(2, 15), limits = size_limits, name = "Density (population/km²)") + 
    coord_fixed(ratio = 1) + # Ensures 1 log-unit on X equals 1 log-unit on Y (square aspect)
    theme_bw(base_size = global_font_size) +
    labs(tag = label_letter, # Places 'A' or 'B' as a tag (usually top-left)
         x = "Area Under Flyway (km²)", 
         y = "Population", 
         fill = paste(flyway_name, "Country")) +
    theme(
      aspect.ratio = 1,                # Forces the plot area to be a perfect square
      panel.grid.minor = element_blank(), # Cleans up the background
      plot.tag = element_text(face = "bold", size = 16), # Style for panel labels (A, B)
      
      # Axis Styling: Manually overriding sizes for publication clarity
      axis.title = element_text(size = 13, face = "bold"), 
      axis.text = element_text(size = 12),                
      
      # Legend Styling
      legend.key.size = unit(0.7, "cm"),      # Size of the legend symbols
      legend.text = element_text(size = 9.5),   # Size of country names in legend
      legend.title = element_text(size = 10, face = "bold"), 
      legend.spacing.y = unit(0.05, "cm")     # Vertical tightness of legend rows
    )
  
  # Conditional logic to handle which legends are displayed in which panel
  if(include_density) {
    p <- p + guides(
      # override.aes makes legend dots a fixed size regardless of plot bubble sizes
      fill = guide_legend(ncol = 5, title.position = "top", order = 1, override.aes = list(size = 5)), 
      size = guide_legend(title.position = "top", order = 2)
    )
  } else {
    p <- p + guides(
      fill = guide_legend(ncol = 5, title.position = "top", order = 3, override.aes = list(size = 5)),
      size = "none" # Hide density in second plot so patchwork can "collect" a single shared version
    )
  }
  
  return(p)
}

# Section 4: Create Objects ----
# Generate the individual panel objects
plot_A <- create_flyway_plot(full_data %>% filter(flyway == "CAF"), "A", "CAF", include_density = TRUE)
plot_B <- create_flyway_plot(full_data %>% filter(flyway == "EAAF"), "B", "EAAF", include_density = FALSE)

# Section 5: Assembly ----
final_plot_to_save <- (plot_A | plot_B) + 
  plot_layout(guides = "collect", heights = c(1, 2.2)) & # Adjusted ratio slightly
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.box.justification = "top",
    legend.margin = margin(t = 10, r = 10, b = 10, l = 10), 
    legend.spacing.x = unit(0.5, "cm"),
    plot.margin = margin(t = 10, r = 5, b = 10, l = 5) 
  )

# Section 6: Export ----
current_date <- Sys.Date()
export_filename <- paste0("Population_per_KM2_under_flyways_", current_date, ".tif")

ggsave(
  filename = file.path(path_export, export_filename),
  plot = final_plot_to_save,
  device = "tiff",
  width = 16,      # Increased from 14 to provide horizontal room for legend text
  height = 9,      # Increased from 7/8 to accommodate 5-column rows
  dpi = 300,       
  compression = "lzw" 
)
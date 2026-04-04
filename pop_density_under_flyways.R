# ---
# Title: Flyway Comparison - Centered Density Legend
# Purpose: Top-left panel tags, leader lines, and date-appended filenames
# Author: S.Y.
# Date: 2026-04-04
# ---

# Section 1: Setup ----
library(tidyverse) # Loads a collection of packages (ggplot2, dplyr, etc.) for data science
library(scales)    # Provides functions for formatting axis labels (e.g., adding commas to large numbers)
library(ggrepel)   # Adds specialized labeling geometry to prevent text from overlapping data points
library(patchwork) # Allows for the easy combination of multiple ggplot objects into one layout

# Path Configuration: Defining directory paths as variables to keep the script organized
path_wd <- "/Users/shenglaiyin/Library/CloudStorage/OneDrive-UniversityofOklahoma/Avian Research Special Issue/John's review/AvianResSI_script"
path_dat <- file.path(path_wd, "dat")      # Sets the path for the folder containing raw data files
path_export <- file.path(path_wd, "export") # Sets the path for where the final TIFF files will be saved

setwd(path_wd) # Changes the current working directory to the specified project path

# Section 2: Data Import and Processing ----
df <- read.csv(file.path(path_dat, "flyway_country_pop.csv")) # Imports the flyway population CSV data

# USER CONTROLS: Global variables for easy adjustment of plot behavior
threshold_percentile <- 0.75 # Sets the cutoff (top 25%) for labeling the densest countries
global_font_size <- 9        # Sets a baseline font size for text elements across the plots
common_limits <- c(1, 1e9)   # Defines the fixed range for both X and Y log scales (1 to 1 billion)

full_data <- df %>%
  filter(pop > 0, area_km2 > 0) %>% # Removes zero or negative values to prevent errors on log scales
  mutate(country = case_when(
    # Renaming long country names to shorter versions for cleaner legends and labels
    country == "Republic of Korea" ~ "S. Korea",
    str_detect(country, "People's Republic|D\\.P\\.R|D. P. R.") ~ "N. Korea", 
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
    TRUE ~ country # Keeps the original name if none of the above conditions are met
  )) %>%
  mutate(density = pop / area_km2) %>% # Calculates population density per square kilometer
  group_by(flyway) %>% # Groups data by flyway (CAF or EAAF) for panel-specific calculations
  mutate(is_high_density = density >= quantile(density, threshold_percentile)) %>% # Identifies high-density outliers
  ungroup() # Removes the grouping metadata to ensure clean plotting

# Establishes fixed min/max density limits to ensure bubble sizes are consistent between panels
size_limits <- c(min(full_data$density), max(full_data$density))

# Section 3: Plotting Function ----
create_flyway_plot <- function(data_subset, label_letter, flyway_name, include_density = FALSE) {
  
  p <- ggplot(data_subset, aes(x = area_km2, y = pop)) +
    # Adds a diagonal reference line (slope of 1) where population equals area
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
    # Plots the data as bubbles; color represents country, size represents density
    geom_point(aes(size = density, fill = country), shape = 21, color = "black", stroke = 0.5) +
    # Adds "repelled" text labels for high-density countries with leader lines
    geom_text_repel(
      data = subset(data_subset, is_high_density == TRUE), 
      aes(label = country), 
      size = (global_font_size * 0.5), # Scales text size relative to global setting
      fontface = "bold", 
      box.padding = 0.8,       # Adds space around the text box
      point.padding = 0.5,     # Adds space between the text and the bubble
      max.overlaps = 20,       # Limits the number of overlapping labels allowed
      min.segment.length = 0,  # Ensures leader lines appear even for close labels
      segment.color = "grey30", # Sets a subtle dark grey color for leader lines
      segment.size = 0.6,      # Sets the thickness of the leader lines
      segment.alpha = 1,       # Keeps leader lines fully opaque
      force = 2                # Increases the strength of the label repulsion
    ) +
    # Configures the X-axis to log base 10 with comma formatting and fixed limits
    scale_x_log10(labels = comma, limits = common_limits, expand = c(0, 0)) + 
    # Configures the Y-axis to log base 10 with comma formatting and fixed limits
    scale_y_log10(labels = comma, limits = common_limits, expand = c(0, 0)) +
    # Uses the 'turbo' color palette for clear differentiation between many countries
    scale_fill_viridis_d(option = "turbo") + 
    # Sets the physical size range of the bubbles based on the density values
    scale_size_continuous(range = c(2, 15), limits = size_limits, name = "Density (population/km²)") + 
    # Forces a 1:1 ratio for the log axes to maintain the square geometry
    coord_fixed(ratio = 1) + 
    # Applies a clean black-and-white theme with the defined base font size
    theme_bw(base_size = global_font_size) +
    # Adds the panel label (A or B) and specific axis titles
    labs(tag = label_letter, 
         x = "Area Under Flyway (km²)", 
         y = "Population", 
         fill = paste(flyway_name, "Country")) +
    # Customizes the theme elements for publication quality
    theme(
      aspect.ratio = 1,                # Ensures the internal plot area is a perfect square
      panel.grid.minor = element_blank(), # Removes minor grid lines for a cleaner look
      plot.tag = element_text(face = "bold", size = 16), # Formats the 'A' and 'B' panel tags
      axis.title = element_text(size = 13, face = "bold"), # Formats the axis titles
      axis.text = element_text(size = 12),                 # Formats the axis tick labels
      legend.key.size = unit(0.7, "cm"),      # Adjusts the size of the color squares in the legend
      legend.text = element_text(size = 9.5),   # Adjusts the font size of country names in the legend
      legend.title = element_text(size = 10, face = "bold"), # Formats the legend headers
      legend.spacing.y = unit(0.05, "cm")     # Tightens the vertical space between legend items
    )
  
  # Logic to manage which legends appear in each panel for combined export
  if(include_density) {
    p <- p + guides(
      fill = guide_legend(ncol = 5, title.position = "top", order = 1, override.aes = list(size = 5)), 
      size = guide_legend(title.position = "top", order = 2) # Shows density legend in Plot A
    )
  } else {
    p <- p + guides(
      fill = guide_legend(ncol = 5, title.position = "top", order = 3, override.aes = list(size = 5)),
      size = "none" # Hides density legend in Plot B to prevent duplication
    )
  }
  
  return(p) # Outputs the finalized ggplot object
}

# Section 4: Create Objects ----
# Generates Plot A (CAF) and Plot B (EAAF) using the custom function
plot_A <- create_flyway_plot(full_data %>% filter(flyway == "CAF"), "A", "CAF", include_density = TRUE)
plot_B <- create_flyway_plot(full_data %>% filter(flyway == "EAAF"), "B", "EAAF", include_density = FALSE)

# Section 5: Assembly ----
# Combines the two plots side-by-side using patchwork
final_plot_to_save <- (plot_A | plot_B) + 
  # Collects legends to the bottom and sets the plot vs. legend height ratio
  plot_layout(guides = "collect", heights = c(1, 2.2)) & 
  theme(
    legend.position = "bottom",          # Places the shared legend at the base
    legend.box = "horizontal",           # Arranges the different legends side-by-side
    legend.box.justification = "top",    # Aligns the tops of the legend blocks
    legend.margin = margin(t = 10, r = 10, b = 10, l = 10), # Adds padding around the legend
    legend.spacing.x = unit(0.5, "cm"),   # Adds horizontal gap between the three legend groups
    plot.margin = margin(t = 10, r = 5, b = 10, l = 5) # Adjusts margins to pull A and B closer
  )

# Section 6: Export ----
current_date <- Sys.Date() # Retrieves the current system date for the filename
export_filename <- paste0("Population_per_KM2_under_flyways_", current_date, ".tif") # Creates the filename

# Saves the final high-resolution TIFF file
ggsave(
  filename = file.path(path_export, export_filename), # Full path for saving
  plot = final_plot_to_save, # The patchwork object to save
  device = "tiff",           # Specifies TIFF format
  width = 16,                # Sets total width in inches to allow room for 5 legend columns
  height = 9,                # Sets total height in inches to balance plots and legends
  dpi = 300,                 # Standard publication-quality resolution (dots per inch)
  compression = "lzw"        # Applies lossless LZW compression to reduce file size
)
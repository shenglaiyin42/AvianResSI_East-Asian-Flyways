# ==============================================================================
# Title: Avian Research Flyway Publication Analysis
# Subtitle: Overlap Processing and Publication-Ready Visualization
# Author: R Coding Assistant
# Date: 2026-04-04
# ==============================================================================

# ---- 1. Environment Setup ----

# Define project directory paths for better organization
path_wd <- "/Users/shenglaiyin/Library/CloudStorage/OneDrive-UniversityofOklahoma/Avian Research Special Issue/John's review/AvianResSI_script"
path_dat <- "/Users/shenglaiyin/Library/CloudStorage/OneDrive-UniversityofOklahoma/Avian Research Special Issue/John's review/AvianResSI_script/dat"
path_export <- "/Users/shenglaiyin/Library/CloudStorage/OneDrive-UniversityofOklahoma/Avian Research Special Issue/John's review/AvianResSI_script/export"

# Set the working directory
setwd(path_wd)

# Confirm working directory
message("Working directory set to: ", getwd())

# Load required packages
library(dplyr)    # For data manipulation and joining
library(ggplot2)  # For professional data visualization

# ---- 2. Data Import ----

# Load the raw CSV file from the 'dat' folder
# check.names = FALSE preserves the exact column name "Article Title"
df <- read.csv(file.path(path_dat, "flyway_pubs_all.csv"), 
               check.names = FALSE, 
               stringsAsFactors = FALSE)

# ---- 3. Identify Regional Overlaps ----

# 3.1. Filter for Central Asian Flyway records and get unique titles
ccaf_pubs <- df %>%
  filter(Region == "CAF") %>%
  select(Year, `Article Title`) %>%
  distinct()

# 3.2. Filter for East Asian-Australasian Flyway records and get unique titles
eaaf_pubs <- df %>%
  filter(Region == "EAAF") %>%
  select(Year, `Article Title`) %>%
  distinct()

# 3.3. Find the intersection (Articles present in BOTH CAF and EAAF)
both_df <- inner_join(ccaf_pubs, eaaf_pubs, by = c("Year", "Article Title")) %>%
  mutate(Region = "CAF and EAAF") # Assign the new category label

# ---- 4. Data Consolidation ----

# Merge the original dataset with the newly identified "Both" records
df_final <- bind_rows(df, both_df)

# Sort the final dataframe by Year and Title for internal consistency
df_final <- df_final %>%
  arrange(Year, `Article Title`)

# ---- 5. Data Preparation for Plotting ----

# 5.1. Convert Region to factor to control the stacking order (Bottom to Top)
df_final$Region <- factor(df_final$Region, 
                          levels = c("EAAF", "CAF", "CAF and EAAF", "Asian"))

# 5.2. Calculate the actual bar height (total rows in df_final per year)
annual_stack_heights <- df_final %>%
  group_by(Year) %>%
  summarise(StackHeight = n())

# 5.3. Calculate unique article count per year for the text labels
annual_unique_counts <- df_final %>%
  group_by(Year) %>%
  summarise(UniqueCount = n_distinct(`Article Title`))

# 5.4. Combine coordinates and labels into a single mapping object
label_data <- left_join(annual_stack_heights, annual_unique_counts, by = "Year")

# ---- 6. Visualization Construction ----

# Initialize the plot
p <- ggplot() +
  # Create the stacked bars
  geom_bar(data = df_final, 
           aes(x = factor(Year), fill = Region),
           position = "stack", 
           color = "white", 
           linewidth = 0.3) +
  
  # Add unique count labels at the top of the stack
  geom_text(data = label_data, 
            aes(x = factor(Year), y = StackHeight, label = UniqueCount), 
            inherit.aes = FALSE, 
            vjust = -0.8,        
            size = 5,            
            fontface = "bold",   
            color = "black") +   
  
  # Apply manual hex colors
  scale_fill_manual(values = c(
    "EAAF"  = "#a6cee3", 
    "CAF"   = "#b2df8a", 
    "CAF and EAAF"  = "#d5b43c", 
    "Asian" = "#ff9e17"
  )) +
  
  # Define Y-axis scale
  scale_y_continuous(
    breaks = seq(0, 50, by = 5), 
    expand = expansion(mult = c(0, 0.15)) 
  ) +
  
  # Configure axis labels and legend title
  labs(
    title = NULL, 
    subtitle = NULL, 
    x = "Year",
    y = "Publications (including overlaps)",
    fill = "Keywords"
  ) +
  
  theme_minimal() + # Base theme for clean look
  theme(
    axis.title = element_text(size = 16, face = "bold"), 
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14),
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 14),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

# Display the plot
print(p)

# ---- 7. Export and File Saving ----

# 7.1. Generate a date-stamped filename
file_name <- paste0("Flyway_publication_", format(Sys.Date(), "%Y-%m-%d"), ".tif")

# 7.2. Save as high-resolution TIFF in the 'export' folder
ggsave(
  filename = file.path(path_export, file_name),
  plot = p,
  device = "tiff",
  width = 8, 
  height = 6, 
  units = "in",
  dpi = 300,             
  compression = "lzw"    
)

# Output final confirmation message
message("Process complete. File saved to export folder: ", file_name)
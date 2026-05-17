# Load necessary libraries
library(tidyverse) # Loads dplyr, ggplot2, tidyr, readr, purrr, tibble, forcats
library(tidytuesdayR) # Loads functions for accessing TidyTuesday datasets

# Load the TidyTuesday data for 2022-11-22
tuesdata <- tt_load("2022-11-22")
museums <- tuesdata$museums

# Display current column names for reference
names(museums)

# --- Data Cleaning and Preprocessing ---
# Create a subset of the data with relevant columns and clean formatting
museum_subset <- museums |>
  # Select only the columns needed for analysis
  select(
    Year_opened, Year_closed, Area_Deprivation_index
  ) |>
  # Remove rows with missing values in these columns
  drop_na() |>
  # Split the 'Year_opened' column which might contain ranges (e.g., "1990:1995")
  # into two separate columns 'opened1' and 'opened2'
  separate_wider_delim(
    Year_opened,
    delim = ":",
    names = c("opened1", "opened2")
  ) |>
  # Similarly split 'Year_closed' into 'closed1' and 'closed2'
  separate_wider_delim(
    Year_closed,
    delim = ":",
    names = c("closed1", "closed2")
  ) |>
  # Convert the split string year columns into numeric values
  mutate(
    across(
      c(opened1, opened2, closed1, closed2), as.numeric
    ),
    # Convert Deprivation Index to a factor (categorical variable) with levels 1-10
    # Level 1 usually indicates least deprived, 10 most deprived
    Area_Deprivation_index = factor(
      Area_Deprivation_index,
      levels = 1:10
    )
  )

# Further refine the dataset to handle special values and ranges
museum_data <- museum_subset |>
  # Replace '9999' in closed years with NA (often used as a placeholder for "still open" or unknown)
  mutate(across(
    c(closed1, closed2),
    ~ if_else(.x == 9999, NA_real_, .x)
  )) |>
  # Calculate a single 'closed' year.
  # If the start and end of the closed range are the same, use that year.
  # If they differ, take the average and round it.
  mutate(closed = case_when(
    closed1 == closed2 ~ closed1,
    closed1 != closed2 ~ round((closed2 + closed1) / 2)
  )) |>
  # Calculate a single 'opened' year using the same logic as above.
  mutate(opened = case_when(
    opened1 == opened2 ~ opened1,
    opened1 != opened2 ~ round((opened2 + opened1) / 2)
  )) |>
  # Keep only the final cleaned columns
  select(Area_Deprivation_index, opened, closed) |>
  # Rename for easier access
  rename(deprivation = Area_Deprivation_index) |>
  # Sort by deprivation index
  arrange(deprivation)

# Preview the cleaned data
head(museum_data)

# --- Analysis Function ---
# Define a function to calculate the net number of open museums for a given year and deprivation index
# Arguments:
#   year: The year to check (e.g., 2000)
#   dep: The deprivation index to filter by
#   data: The dataset to use (defaults to museum_data)
num_year <- function(year, dep, data = museum_data) {
  # Filter data for the specific deprivation index
  df <- filter(data, deprivation == dep)
  
  # Count how many museums opened on or before the given year
  num_open <- sum(df$opened <= year)
  
  # Count how many museums closed on or before the given year.
  # na.rm = TRUE ensures we ignore museums that are still open (closed year is NA)
  num_closed <- sum(df$closed <= year, na.rm = TRUE)
  
  # The difference gives the number of currently active museums in that year
  diff <- num_open - num_closed
  return(diff)
}

# Test the function for year 2015 and deprivation index 1
num_year(2015, 1)

# --- Execute Analysis Over Time ---
# Define the range of years and deprivation indices to analyze
all_years <- 1960:2021
deps <- 1:10

# Use pmap_vec to iterate over every combination of year and deprivation index.
# expand.grid creates a data frame of all combinations.
output <- pmap_vec(
  expand.grid(all_years, deps),
  ~ num_year(year = .x, dep = .y)
)

# Reshape the flat output vector into a matrix where rows are years and columns are deprivation indices
results <- matrix(output,
  nrow = length(all_years),
  byrow = FALSE
)
# Set column names to match deprivation indices
colnames(results) <- 1:10

# --- Prepare Data for Plotting ---
# Convert the results matrix into a tidy format suitable for ggplot
plot_data <- results |>
  as_tibble() |>
  # Add the 'year' column back
  mutate(year = all_years) |>
  # Pivot from wide (columns 1-10) to long format
  # 'deprivation' will hold the column names (1-10)
  # 'museums' will hold the values (count of open museums)
  pivot_longer(
    -year,
    names_to = "deprivation",
    values_to = "museums"
  ) |>
  # Ensure deprivation is a factor with correct levels for plotting order
  mutate(
    deprivation = factor(deprivation, levels = 1:10)
  )

# Check the plot data
plot_data

# --- Visualization ---
# Plot the number of museums over time, colored by deprivation index
ggplot(
  data = plot_data,
  mapping = aes(
    x = year,
    y = museums,
    color = deprivation
  )
) +
  geom_line() # Draw lines connecting data points over years'

str(plot_data)

library(ggplot2)
library(dplyr)

# Max year for end labels
max_year <- max(plot_simplified$year)

label_data <- plot_simplified %>%
  filter(year == max_year)

ggplot() +
  
  # ─────────────────────────────────────────────
  # BACKGROUND: all deprivation deciles (faded)
  # ─────────────────────────────────────────────
  geom_line(
    data = plot_data,
    aes(
      x = year,
      y = museums,
      group = deprivation
    ),
    color = "#7A5A2E",   # soft brown
    alpha = 0.18,
    linewidth = 0.6
  ) +
  
  # ─────────────────────────────────────────────
  # FOREGROUND: grouped comparison lines
  # ─────────────────────────────────────────────
  geom_line(
    data = plot_simplified,
    aes(
      x = year,
      y = museums,
      color = group
    ),
    linewidth = 1.8,
    lineend = "round"
  ) +
  
  scale_color_manual(
    values = c(
      "Least deprived"  = "#3B2314",
      "Middle deprived" = "#A9712A",
      "Most deprived"   = "#7A3E1D"
    )
  ) +
  
  labs(
    title = "Inequality in Museum Access Over Time",
    subtitle = "Faded lines show all deprivation deciles; bold lines show grouped trends",
    x = NULL,
    y = "Number of Open Museums"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    # Yellow parchment background
    plot.background = element_rect(fill = "#F5D76E", color = NA),
    panel.background = element_rect(fill = "#F5D76E", color = NA),
    
    # Subtle aged grid
    panel.grid.major.y = element_line(
      color = "#D1B45A",
      linewidth = 0.5
    ),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    
    # Editorial text styling
    plot.title = element_text(
      face = "bold",
      size = 20,
      color = "#3B2314",
      hjust = 0
    ),
    plot.subtitle = element_text(
      size = 13,
      color = "#5A3A1E",
      hjust = 0
    ),
    
    axis.text = element_text(color = "#3B2314"),
    axis.title.y = element_text(face = "bold", color = "#3B2314"),
    
    legend.position = "none",
    plot.margin = margin(t = 20, r = 150, b = 20, l = 20)
  ) +
  
  # End-of-line labels
  geom_text(
    data = label_data,
    aes(
      x = max_year + 1,
      y = museums,
      label = paste(group, "areas"),
      color = group
    ),
    hjust = 0,
    size = 4.4,
    fontface = "bold"
  ) +
  
  coord_cartesian(
    xlim = c(min(plot_data$year), max_year + 3),
    clip = "off"
  )

library(ggplot2)
library(dplyr)

# Max year for labels
max_year <- max(plot_simplified$year)

label_data <- plot_simplified %>%
  filter(year == max_year)

ggplot() +
  
  # ─────────────────────────────────────────────
  # BACKGROUND: all deprivation deciles (faded)
  # ─────────────────────────────────────────────
  geom_line(
    data = plot_data %>% filter(deprivation != 6),
    aes(
      x = year,
      y = museums,
      group = deprivation
    ),
    color = "#7A5A2E",
    alpha = 0.15,
    linewidth = 0.6
  ) +
  
  # ─────────────────────────────────────────────
  # HIGHLIGHT: deprivation index = 6
  # ─────────────────────────────────────────────
  geom_line(
    data = plot_data %>% filter(deprivation == 6),
    aes(
      x = year,
      y = museums
    ),
    color = "#3B2314",   # deep dark brown
    linewidth = 2.2,
    lineend = "round"
  ) +
  
  # ─────────────────────────────────────────────
  # FOREGROUND: grouped summary lines (optional)
  # ─────────────────────────────────────────────
  geom_line(
    data = plot_simplified,
    aes(
      x = year,
      y = museums,
      color = group
    ),
    linewidth = 1.6,
    alpha = 0.9
  ) +
  
  scale_color_manual(
    values = c(
      "Least deprived"  = "#3B2314",
      "Middle deprived" = "#A9712A",
      "Most deprived"   = "#7A3E1D"
    )
  ) +
  
  labs(
    title = "Inequality in Museum Access Over Time",
    subtitle = "Deprivation index 6 currently has the highest number of operating museums",
    x = NULL,
    y = "Number of Open Museums"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    # Yellow parchment background
    plot.background = element_rect(fill = "#F5D76E", color = NA),
    panel.background = element_rect(fill = "#F5D76E", color = NA),
    
    # Aged gridlines
    panel.grid.major.y = element_line(
      color = "#D1B45A",
      linewidth = 0.5
    ),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    
    # Editorial styling
    plot.title = element_text(
      face = "bold",
      size = 20,
      color = "#3B2314",
      hjust = 0
    ),
    plot.subtitle = element_text(
      size = 13,
      color = "#5A3A1E",
      hjust = 0
    ),
    
    axis.text = element_text(color = "#3B2314"),
    axis.title.y = element_text(face = "bold", color = "#3B2314"),
    
    legend.position = "none",
    plot.margin = margin(t = 20, r = 160, b = 20, l = 20)
  ) +
  
  # End-of-line labels for grouped lines
  geom_text(
    data = label_data,
    aes(
      x = max_year + 1,
      y = museums,
      label = paste(group, "areas"),
      color = group
    ),
    hjust = 0,
    size = 4.3,
    fontface = "bold"
  ) +
  
  coord_cartesian(
    xlim = c(min(plot_data$year), max_year + 3),
    clip = "off"
  )


library(ggplot2)
library(dplyr)

max_year <- max(plot_simplified$year)

label_data <- plot_simplified %>%
  filter(year == max_year)

ggplot() +
  
  # ─────────────────────────────────────────────
  # BACKGROUND: all deprivation except 5 & 6
  # ─────────────────────────────────────────────
  geom_line(
    data = plot_data %>% filter(!deprivation %in% c(5, 6)),
    aes(
      x = year,
      y = museums,
      group = deprivation
    ),
    color = "#7A5A2E",
    alpha = 0.18,
    linewidth = 0.6
  ) +
  
  # ─────────────────────────────────────────────
  # EXTRA-FADED: deprivation = 5
  # ─────────────────────────────────────────────
  geom_line(
    data = plot_data %>% filter(deprivation == 5),
    aes(
      x = year,
      y = museums
    ),
    color = "#7A5A2E",
    alpha = 0.08,      # more faded than others
    linewidth = 0.6
  ) +
  
  # ─────────────────────────────────────────────
  # HIGHLIGHT: deprivation = 6 (same ink color)
  # ─────────────────────────────────────────────
  geom_line(
    data = plot_data %>% filter(deprivation == 6),
    aes(
      x = year,
      y = museums
    ),
    color = "#A9712A",  # SAME tone as middle-deprived line
    linewidth = 2.2,
    lineend = "round"
  ) +
  
  # ─────────────────────────────────────────────
  # GROUPED SUMMARY LINES
  # ─────────────────────────────────────────────
  geom_line(
    data = plot_simplified,
    aes(
      x = year,
      y = museums,
      color = group
    ),
    linewidth = 1.6
  ) +
  
  scale_color_manual(
    values = c(
      "Least deprived"  = "#3B2314",
      "Middle deprived" = "#A9712A",
      "Most deprived"   = "#7A3E1D"
    )
  ) +
  
  labs(
    title = "Inequality in Museum Access Over Time",
    subtitle = "Deprivation index 6 currently has the highest number of operating museums",
    x = NULL,
    y = "Number of Open Museums"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.background = element_rect(fill = "#F5D76E", color = NA),
    panel.background = element_rect(fill = "#F5D76E", color = NA),
    
    panel.grid.major.y = element_line(
      color = "#D1B45A",
      linewidth = 0.5
    ),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    
    plot.title = element_text(
      face = "bold",
      size = 20,
      color = "#3B2314",
      hjust = 0
    ),
    plot.subtitle = element_text(
      size = 13,
      color = "#5A3A1E",
      hjust = 0
    ),
    
    axis.text = element_text(color = "#3B2314"),
    axis.title.y = element_text(face = "bold", color = "#3B2314"),
    
    legend.position = "none",
    plot.margin = margin(t = 20, r = 160, b = 20, l = 20)
  ) +
  
  geom_text(
    data = label_data,
    aes(
      x = max_year + 1,
      y = museums,
      label = paste(group, "areas"),
      color = group
    ),
    hjust = 0,
    size = 4.3,
    fontface = "bold"
  ) +
  
  coord_cartesian(
    xlim = c(min(plot_data$year), max_year + 3),
    clip = "off"
  )

ggsave("museums.pdf", dpi = 300, width = 10, height = 6)

library(ggplot2)
library(dplyr)

# Get last year
max_year <- max(plot_simplified$year)

# Label positions (summary groups only)
label_data <- plot_simplified %>%
  filter(year == max_year)

ggplot() +
  
  # ─────────────────────────────────────────────
  # BACKGROUND: all deprivation lines EXCEPT 5 & 6
  # ─────────────────────────────────────────────
  geom_line(
    data = plot_data %>% filter(!deprivation %in% c(5, 6)),
    aes(
      x = year,
      y = museums,
      group = deprivation
    ),
    color = "#7A5A2E",
    alpha = 0.18,
    linewidth = 0.6
  ) +
  
  # ─────────────────────────────────────────────
  # EXTRA-FADED LINE: deprivation = 5
  # ─────────────────────────────────────────────
  geom_line(
    data = plot_data %>% filter(deprivation == 5),
    aes(
      x = year,
      y = museums
    ),
    color = "#7A5A2E",
    alpha = 0.08,
    linewidth = 0.6
  ) +
  
  # ─────────────────────────────────────────────
  # HERO LINE: deprivation = 6
  # ─────────────────────────────────────────────
  geom_line(
    data = plot_data %>% filter(deprivation == 6),
    aes(
      x = year,
      y = museums
    ),
    color = "#A9712A",
    linewidth = 2.2,
    lineend = "round"
  ) +
  
  # ─────────────────────────────────────────────
  # GROUPED SUMMARY LINES
  # ─────────────────────────────────────────────
  geom_line(
    data = plot_simplified,
    aes(
      x = year,
      y = museums,
      color = group
    ),
    linewidth = 1.6
  ) +
  
  scale_color_manual(
    values = c(
      "Least deprived"  = "#3B2314",
      "Middle deprived" = "#A9712A",
      "Most deprived"   = "#7A3E1D"
    )
  ) +
  
  labs(
    title = "Inequality in Museum Access Over Time",
    subtitle = "Deprivation index 6 currently has the highest number of operating museums",
    x = NULL,
    y = "Number of Open Museums"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.background = element_rect(fill = "#F5D76E", color = NA),
    panel.background = element_rect(fill = "#F5D76E", color = NA),
    
    panel.grid.major.y = element_line(
      color = "#D1B45A",
      linewidth = 0.5
    ),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    
    plot.title = element_text(
      face = "bold",
      size = 20,
      color = "#3B2314",
      hjust = 0
    ),
    plot.subtitle = element_text(
      size = 13,
      color = "#5A3A1E",
      hjust = 0
    ),
    
    axis.text = element_text(color = "#3B2314"),
    axis.title.y = element_text(face = "bold", color = "#3B2314"),
    
    legend.position = "none",
    plot.margin = margin(t = 20, r = 160, b = 20, l = 20)
  ) +
  
  # ─────────────────────────────────────────────
  # DIRECT LABELS
  # ─────────────────────────────────────────────
  geom_text(
    data = label_data,
    aes(
      x = max_year + 1,
      y = museums,
      label = paste(group, "areas"),
      color = group
    ),
    hjust = 0,
    size = 4.3,
    fontface = "bold"
  ) +
  
  coord_cartesian(
    xlim = c(min(plot_data$year), max_year + 3),
    clip = "off"
  )

# Save
ggsave("museums.pdf", dpi = 300, width = 10, height = 6)




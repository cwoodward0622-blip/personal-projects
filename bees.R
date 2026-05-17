library(tidyverse)
library(tidytuesdayR)

tuesdata <- tt_load("2022-01-11")
colony <- tuesdata$colony

head(colony)

str(colony)

# spc_tbl_ [1,222 × 10] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
#  $ year           : num [1:1222] 2015 2015 2015 2015 2015 ...
#  $ months         : chr [1:1222] "January-March" "January-March" "January-March" "January-March" ...
#  $ state          : chr [1:1222] "Alabama" "Arizona" "Arkansas" "California" ...
#  $ colony_n       : num [1:1222] 7000 35000 13000 1440000 3500 3900 305000 104000 10500 81000 ...
#  $ colony_max     : num [1:1222] 7000 35000 14000 1690000 12500 3900 315000 105000 10500 88000 ...
#  $ colony_lost    : num [1:1222] 1800 4600 1500 255000 1500 870 42000 14500 380 3700 ...
#  $ colony_lost_pct: num [1:1222] 26 13 11 15 12 22 13 14 4 4 ...
#  $ colony_added   : num [1:1222] 2800 3400 1200 250000 200 290 54000 47000 3400 2600 ...
#  $ colony_reno    : num [1:1222] 250 2100 90 124000 140 NA 25000 9500 760 8000 ...
#  $ colony_reno_pct: num [1:1222] 4 6 1 7 1 NA 8 9 7 9 ...
#  - attr(*, "spec")=
#   .. cols(
#   ..   year = col_double(),
#   ..   months = col_character(),
#   ..   state = col_character(),
#   ..   colony_n = col_double(),
#   ..   colony_max = col_double(),
#   ..   colony_lost = col_double(),
#   ..   colony_lost_pct = col_double(),
#   ..   colony_added = col_double(),
#   ..   colony_reno = col_double(),
#   ..   colony_reno_pct = col_double()
#   .. )
#  - attr(*, "problems")=<externalptr>

# -------------------------------------------------------------------------
# Exploratory Data Analysis: Bee Colony Losses
# -------------------------------------------------------------------------

# 1. Data Cleaning & Preparation
# Check for missing values
colony %>%
    summarise(across(everything(), ~ sum(is.na(.))))

# Filter out rows with missing key metrics if necessary or just inspect
# Note: "Other States" might need attention if we do state-level analysis
clean_colony <- colony %>%
    filter(!state %in% c("United States", "Other States")) %>% # Remove aggregates for state analysis
    filter(!is.na(colony_lost))

# 2. Univariate Analysis

# Histogram of Colony Loss Percentage
ggplot(clean_colony, aes(x = colony_lost_pct)) +
    geom_histogram(binwidth = 1, fill = "orange", color = "black") +
    labs(
        title = "Distribution of Colony Loss Percentage",
        x = "Percentage of Colony Loss",
        y = "Frequency"
    ) +
    theme_minimal()

# Boxplot of Colony Losses to see distribution and outliers
ggplot(clean_colony, aes(y = colony_lost)) +
    geom_boxplot(fill = "gold") +
    labs(
        title = "Boxplot of Colony Losses (Count)",
        y = "Number of Colonies Lost"
    ) +
    scale_y_log10(labels = scales::comma) + # Log scale due to high skewness
    theme_minimal()

# 3. Time Series Analysis

# Trend of Average Loss Percentage Over Years
colony_loss_year <- clean_colony %>%
    group_by(year) %>%
    summarise(
        avg_loss_pct = mean(colony_lost_pct, na.rm = TRUE),
        total_lost = sum(colony_lost, na.rm = TRUE)
    )

ggplot(colony_loss_year, aes(x = year, y = avg_loss_pct)) +
    geom_line(color = "darkred", size = 1) +
    geom_point(color = "darkred", size = 3) +
    labs(
        title = "Average Colony Loss Percentage Over Years",
        x = "Year",
        y = "Average Loss (%)"
    ) +
    theme_minimal()

# Seasonal Pattern (Months)
colony_loss_month <- clean_colony %>%
    group_by(months) %>%
    summarise(avg_loss_pct = mean(colony_lost_pct, na.rm = TRUE)) %>%
    mutate(months = factor(months, levels = c("January-March", "April-June", "July-September", "October-December")))

ggplot(colony_loss_month, aes(x = months, y = avg_loss_pct)) +
    geom_col(fill = "honeyDew3", color = "black") + # Placeholder color
    labs(
        title = "Seasonal Pattern of Colony Loss",
        x = "Season",
        y = "Average Loss (%)"
    ) +
    theme_minimal()

# 4. Spatial Analysis (State-level)

# Top 10 States with highest average loss percentage
top_loss_states <- clean_colony %>%
    group_by(state) %>%
    summarise(avg_loss_pct = mean(colony_lost_pct, na.rm = TRUE)) %>%
    arrange(desc(avg_loss_pct)) %>%
    head(10)

ggplot(top_loss_states, aes(x = reorder(state, avg_loss_pct), y = avg_loss_pct)) +
    geom_col(fill = "firebrick") +
    coord_flip() +
    labs(
        title = "Top 10 States by Average Colony Loss Percentage",
        x = "State",
        y = "Average Loss (%)"
    ) +
    theme_minimal()

# 5. Correlation: Colony Size vs Loss Pct
ggplot(clean_colony, aes(x = colony_n, y = colony_lost_pct)) +
    geom_point(alpha = 0.3) +
    scale_x_log10(labels = scales::comma) +
    geom_smooth(method = "lm", color = "blue") +
    labs(
        title = "Colony Size vs. Loss Percentage",
        x = "Number of Colonies (Log Scale)",
        y = "Loss Percentage"
    ) +
    theme_minimal()

# ==========================================================
# U.S. Honeybee Colony Losses
# Tidytuesday (2022-01-11)
# Infographic-Style Donut Charts
# ==========================================================

# Load libraries
library(tidyverse)
library(tidytuesdayR)

# ----------------------------------------------------------
# Load data
# ----------------------------------------------------------
tuesdata <- tt_load("2022-01-11")
colony <- tuesdata$colony

# ----------------------------------------------------------
# Prepare yearly U.S. winter colony loss data
# (Years ending April 1 ≈ January–March)
# ----------------------------------------------------------
bee_yearly <- colony %>%
  filter(
    state == "United States",
    months == "January-March"
  ) %>%
  mutate(
    season = paste0(year, "-", substr(year + 1, 3, 4))
  ) %>%
  select(season, colony_lost_pct) %>%
  drop_na()

# ----------------------------------------------------------
# Create donut chart data (lost vs remaining)
# ----------------------------------------------------------
donut_data <- bee_yearly %>%
  mutate(
    lost = colony_lost_pct,
    remaining = 100 - colony_lost_pct
  ) %>%
  pivot_longer(
    cols = c(lost, remaining),
    names_to = "status",
    values_to = "value"
  )

# ----------------------------------------------------------
# Plot infographic-style donut charts
# ----------------------------------------------------------
ggplot(donut_data, aes(x = 2, y = value, fill = status)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  xlim(0.5, 2.5) +
  
  # Small multiples by season
  facet_wrap(~ season, nrow = 2) +
  
  # Color palette similar to infographic
  scale_fill_manual(
    values = c(
      "lost" = "#F4A261",
      "remaining" = "#F6E8C3"
    )
  ) +
  
  # Percentage labels in center
  geom_text(
    data = bee_yearly,
    aes(
      x = 2,
      y = 0,
      label = paste0(colony_lost_pct, "%")
    ),
    inherit.aes = FALSE,
    size = 5,
    fontface = "bold",
    color = "#E76F51"
  ) +
  
  # Titles & labels
  labs(
    title = "U.S. Honeybees Suffer One of Their Deadliest Seasons",
    subtitle = "Estimated share of beekeepers’ colonies lost (Years ending April 1)",
    fill = NULL
  ) +
  
  # Clean infographic styling
  theme_void() +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 12),
    strip.text = element_text(size = 11, face = "bold"),
    legend.position = "none",
    plot.margin = margin(15, 15, 15, 15)
  )

# ----------------------------------------------------------
# Load libraries
# ----------------------------------------------------------
library(tidyverse)
library(tidytuesdayR)

# ----------------------------------------------------------
# Load data
# ----------------------------------------------------------
tuesdata <- tt_load("2022-01-11")
colony <- tuesdata$colony

# ----------------------------------------------------------
# Prepare yearly U.S. winter colony loss data
# ----------------------------------------------------------
bee_yearly <- colony %>%
  filter(
    state == "United States",
    months == "January-March"
  ) %>%
  mutate(
    season = paste0(year, "-", substr(year + 1, 3, 4))
  ) %>%
  select(season, colony_lost_pct) %>%
  drop_na() %>%
  arrange(season)

# ----------------------------------------------------------
# Create honeycomb layout
# ----------------------------------------------------------
bee_hex <- bee_yearly %>%
  mutate(
    col = rep(1:4, length.out = n()),
    row = rep(c(2, 1), each = 4, length.out = n()),
    x = col * 2 + if_else(row %% 2 == 0, 1, 0),
    y = row * 2
  )

# ----------------------------------------------------------
# Hexagon function
# ----------------------------------------------------------
hexagon <- function(cx, cy, size = 0.9) {
  tibble(
    x = cx + size * cos(seq(0, 2*pi, length.out = 7)),
    y = cy + size * sin(seq(0, 2*pi, length.out = 7))
  )
}

# ----------------------------------------------------------
# Build hexagon polygons SAFELY (no unnest issues)
# ----------------------------------------------------------
hex_polygons <- map_dfr(
  1:nrow(bee_hex),
  function(i) {
    hexagon(
      bee_hex$x[i],
      bee_hex$y[i]
    ) %>%
      mutate(
        season = bee_hex$season[i],
        colony_lost_pct = bee_hex$colony_lost_pct[i]
      )
  }
)

# ----------------------------------------------------------
# Plot honeycomb infographic
# ----------------------------------------------------------
ggplot(hex_polygons, aes(x, y, group = season, fill = colony_lost_pct)) +
  
  geom_polygon(color = "white", linewidth = 1) +
  
  # Percent labels
  geom_text(
    data = bee_hex,
    aes(
      x = x,
      y = y,
      label = paste0(colony_lost_pct, "%")
    ),
    inherit.aes = FALSE,
    size = 6,
    fontface = "bold",
    color = "#E76F51"
  ) +
  
  # Season labels
  geom_text(
    data = bee_hex,
    aes(
      x = x,
      y = y - 1.4,
      label = season
    ),
    inherit.aes = FALSE,
    size = 4,
    fontface = "bold"
  ) +
  
  scale_fill_gradient(
    low = "#F6E8C3",
    high = "#F4A261"
  ) +
  
  coord_equal() +
  theme_void() +
  
  labs(
    title = "U.S. Honeybees Suffer One of Their Deadliest Seasons",
    subtitle = "Estimated share of beekeepers’ colonies lost (Years ending April 1)"
  ) +
  
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "none",
    plot.margin = margin(20, 20, 20, 20)
  )

# ----------------------------------------------------------
# Load libraries
# ----------------------------------------------------------
library(tidyverse)
library(tidytuesdayR)

# ----------------------------------------------------------
# Load data
# ----------------------------------------------------------
tuesdata <- tt_load("2022-01-11")
colony <- tuesdata$colony

# ----------------------------------------------------------
# Prepare yearly U.S. winter colony loss data
# ----------------------------------------------------------
bee_yearly <- colony %>%
  filter(
    state == "United States",
    months == "January-March"
  ) %>%
  mutate(
    season = paste0(year, "-", substr(year + 1, 3, 4))
  ) %>%
  select(season, colony_lost_pct) %>%
  drop_na() %>%
  arrange(season)

# ----------------------------------------------------------
# Create honeycomb layout (positions)
# ----------------------------------------------------------
bee_hex <- bee_yearly %>%
  mutate(
    col = rep(1:4, length.out = n()),
    row = rep(c(2, 1), each = 4, length.out = n()),
    x = col * 2 + if_else(row %% 2 == 0, 1, 0),
    y = row * 2
  )

# ----------------------------------------------------------
# Hexagon function
# ----------------------------------------------------------
hexagon <- function(cx, cy, size = 0.9) {
  tibble(
    x = cx + size * cos(seq(0, 2*pi, length.out = 7)),
    y = cy + size * sin(seq(0, 2*pi, length.out = 7))
  )
}

# ----------------------------------------------------------
# Build hexagon polygons
# ----------------------------------------------------------
hex_polygons <- map_dfr(
  1:nrow(bee_hex),
  function(i) {
    hexagon(bee_hex$x[i], bee_hex$y[i]) %>%
      mutate(
        season = bee_hex$season[i],
        colony_lost_pct = bee_hex$colony_lost_pct[i]
      )
  }
)

# ----------------------------------------------------------
# Plot honeycomb infographic (FINAL VERSION)
# ----------------------------------------------------------
ggplot(hex_polygons, aes(x, y, group = season, fill = colony_lost_pct)) +
  
  # Hexagons
  geom_polygon(color = "white", linewidth = 1) +
  
  # Percentage labels (moved UP)
  geom_text(
    data = bee_hex,
    aes(
      x = x,
      y = y + 0.25,
      label = paste0(colony_lost_pct, "%")
    ),
    inherit.aes = FALSE,
    size = 6,
    fontface = "bold",
    color = "#E76F51"
  ) +
  
  # Season labels (moved closer to hexagons)
  geom_text(
    data = bee_hex,
    aes(
      x = x,
      y = y - 0.9,
      label = season
    ),
    inherit.aes = FALSE,
    size = 4,
    fontface = "bold"
  ) +
  
  # Fill scale
  scale_fill_gradient(
    low = "#F6E8C3",
    high = "#F4A261"
  ) +
  
  coord_equal() +
  theme_void() +
  
  labs(
    title = "U.S. Honeybees Suffer One of Their Deadliest Seasons",
    subtitle = "Estimated share of beekeepers’ colonies lost (Years ending April 1)"
  ) +
  
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "none",
    plot.margin = margin(20, 20, 20, 20)
  )


# ----------------------------------------------------------
# Load libraries
# ----------------------------------------------------------
library(tidyverse)
library(tidytuesdayR)

# ----------------------------------------------------------
# Load data
# ----------------------------------------------------------
tuesdata <- tt_load("2022-01-11")
colony <- tuesdata$colony

# ----------------------------------------------------------
# Prepare yearly U.S. winter colony loss data
# ----------------------------------------------------------
bee_yearly <- colony %>%
  filter(
    state == "United States",
    months == "January-March"
  ) %>%
  mutate(
    season = paste0(year, "-", substr(year + 1, 3, 4))
  ) %>%
  select(season, colony_lost_pct) %>%
  drop_na() %>%
  arrange(season)

# ----------------------------------------------------------
# Create honeycomb layout (positions)
# ----------------------------------------------------------
bee_hex <- bee_yearly %>%
  mutate(
    col = rep(1:4, length.out = n()),
    row = rep(c(2, 1), each = 4, length.out = n()),
    x = col * 2 + if_else(row %% 2 == 0, 1, 0),
    y = row * 2
  )

# ----------------------------------------------------------
# Hexagon function
# ----------------------------------------------------------
hexagon <- function(cx, cy, size = 0.9) {
  tibble(
    x = cx + size * cos(seq(0, 2 * pi, length.out = 7)),
    y = cy + size * sin(seq(0, 2 * pi, length.out = 7))
  )
}

# ----------------------------------------------------------
# Build hexagon polygons
# ----------------------------------------------------------
hex_polygons <- map_dfr(
  1:nrow(bee_hex),
  function(i) {
    hexagon(bee_hex$x[i], bee_hex$y[i]) %>%
      mutate(
        season = bee_hex$season[i],
        colony_lost_pct = bee_hex$colony_lost_pct[i]
      )
  }
)

# ----------------------------------------------------------
# Plot honeycomb infographic (FINAL VERSION)
# ----------------------------------------------------------
ggplot(hex_polygons, aes(x, y, group = season, fill = colony_lost_pct)) +
  
  # Hexagons
  geom_polygon(color = "white", linewidth = 1) +
  
  # Percentage labels (slightly ABOVE hexagons)
  geom_text(
    data = bee_hex,
    aes(
      x = x,
      y = y + 0.45,
      label = paste0(colony_lost_pct, "%")
    ),
    inherit.aes = FALSE,
    size = 6,
    fontface = "bold",
    color = "#E76F51"
  ) +
  
  # Season labels (slightly BELOW hexagons)
  geom_text(
    data = bee_hex,
    aes(
      x = x,
      y = y - 0.6,
      label = season
    ),
    inherit.aes = FALSE,
    size = 4,
    fontface = "bold"
  ) +
  
  # Fill scale
  scale_fill_gradient(
    low = "#F6E8C3",
    high = "#F4A261"
  ) +
  
  coord_equal() +
  theme_void() +
  
  labs(
    title = "U.S. Honeybees Suffer One of Their Deadliest Seasons",
    subtitle = "Estimated share of beekeepers’ colonies lost (Years ending April 1)"
  ) +
  
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "none",
    plot.margin = margin(20, 20, 20, 20)
  )


# ----------------------------------------------------------
# Load libraries
# ----------------------------------------------------------
library(tidyverse)
library(tidytuesdayR)

# ----------------------------------------------------------
# Load data
# ----------------------------------------------------------
tuesdata <- tt_load("2022-01-11")
colony <- tuesdata$colony

# ----------------------------------------------------------
# Prepare yearly U.S. winter colony loss data
# ----------------------------------------------------------
bee_yearly <- colony %>%
  filter(
    state == "United States",
    months == "January-March"
  ) %>%
  mutate(
    season = paste0(year, "-", substr(year + 1, 3, 4))
  ) %>%
  select(season, colony_lost_pct) %>%
  drop_na() %>%
  arrange(season)

# ----------------------------------------------------------
# Create honeycomb layout (positions)
# ----------------------------------------------------------
bee_hex <- bee_yearly %>%
  mutate(
    col = rep(1:4, length.out = n()),
    row = rep(c(2, 1), each = 4, length.out = n()),
    x = col * 2 + if_else(row %% 2 == 0, 1, 0),
    y = row * 2
  )

# ----------------------------------------------------------
# Hexagon function
# ----------------------------------------------------------
hexagon <- function(cx, cy, size = 0.9) {
  tibble(
    x = cx + size * cos(seq(0, 2 * pi, length.out = 7)),
    y = cy + size * sin(seq(0, 2 * pi, length.out = 7))
  )
}

# ----------------------------------------------------------
# Build hexagon polygons
# ----------------------------------------------------------
hex_polygons <- map_dfr(
  1:nrow(bee_hex),
  function(i) {
    hexagon(bee_hex$x[i], bee_hex$y[i]) %>%
      mutate(
        season = bee_hex$season[i],
        colony_lost_pct = bee_hex$colony_lost_pct[i]
      )
  }
)

# ----------------------------------------------------------
# Plot honeycomb infographic (FINAL VERSION)
# ----------------------------------------------------------
ggplot(hex_polygons, aes(x, y, group = season, fill = colony_lost_pct)) +
  
  # Hexagons
  geom_polygon(color = "white", linewidth = 1) +
  
  # Percentage labels (JUST OUTSIDE top of hexagons)
  geom_text(
    data = bee_hex,
    aes(
      x = x,
      y = y + 1.0,
      label = paste0(colony_lost_pct, "%")
    ),
    inherit.aes = FALSE,
    size = 6,
    fontface = "bold",
    color = "#E76F51"
  ) +
  
  # Season labels (JUST OUTSIDE bottom of hexagons)
  geom_text(
    data = bee_hex,
    aes(
      x = x,
      y = y - 1.05,
      label = season
    ),
    inherit.aes = FALSE,
    size = 4,
    fontface = "bold"
  ) +
  
  # Fill scale
  scale_fill_gradient(
    low = "#F6E8C3",
    high = "#F4A261"
  ) +
  
  coord_equal(clip = "off") +
  theme_void() +
  
  labs(
    title = "U.S. Honeybees Suffer One of Their Deadliest Seasons",
    subtitle = "Estimated share of beekeepers’ colonies lost (Years ending April 1)"
  ) +
  
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "none",
    plot.margin = margin(40, 40, 40, 40)
  )

# ----------------------------------------------------------
# Load libraries
# ----------------------------------------------------------
library(tidyverse)
library(tidytuesdayR)

# ----------------------------------------------------------
# Load data
# ----------------------------------------------------------
tuesdata <- tt_load("2022-01-11")
colony <- tuesdata$colony

# ----------------------------------------------------------
# Prepare yearly U.S. winter colony loss data
# ----------------------------------------------------------
bee_yearly <- colony %>%
  filter(
    state == "United States",
    months == "January-March"
  ) %>%
  mutate(
    season = paste0(year, "-", substr(year + 1, 3, 4))
  ) %>%
  select(season, colony_lost_pct) %>%
  drop_na() %>%
  arrange(season)

# ----------------------------------------------------------
# Create honeycomb layout (positions)
# ----------------------------------------------------------
bee_hex <- bee_yearly %>%
  mutate(
    col = rep(1:4, length.out = n()),
    row = rep(c(2, 1), each = 4, length.out = n()),
    x = col * 2 + if_else(row %% 2 == 0, 1, 0),
    y = row * 2
  )

# ----------------------------------------------------------
# Hexagon function
# ----------------------------------------------------------
hexagon <- function(cx, cy, size = 0.9) {
  tibble(
    x = cx + size * cos(seq(0, 2 * pi, length.out = 7)),
    y = cy + size * sin(seq(0, 2 * pi, length.out = 7))
  )
}

# ----------------------------------------------------------
# Build hexagon polygons
# ----------------------------------------------------------
hex_polygons <- map_dfr(
  1:nrow(bee_hex),
  function(i) {
    hexagon(bee_hex$x[i], bee_hex$y[i]) %>%
      mutate(
        season = bee_hex$season[i],
        colony_lost_pct = bee_hex$colony_lost_pct[i]
      )
  }
)

# ----------------------------------------------------------
# Plot honeycomb infographic (FINAL with LEGEND)
# ----------------------------------------------------------
ggplot(hex_polygons, aes(x, y, group = season, fill = colony_lost_pct)) +
  
  # Hexagons
  geom_polygon(color = "white", linewidth = 1) +
  
  # Percentage labels (above hexagons)
  geom_text(
    data = bee_hex,
    aes(
      x = x,
      y = y + 1.0,
      label = paste0(colony_lost_pct, "%")
    ),
    inherit.aes = FALSE,
    size = 6,
    fontface = "bold",
    color = "#E76F51"
  ) +
  
  # Season labels (below hexagons)
  geom_text(
    data = bee_hex,
    aes(
      x = x,
      y = y - 1.05,
      label = season
    ),
    inherit.aes = FALSE,
    size = 4,
    fontface = "bold"
  ) +
  
  # Fill scale + explanatory legend
  scale_fill_gradient(
    low = "#F6E8C3",
    high = "#F4A261",
    name = "Colony Loss (%)\nDarker = More Bees Lost",
    guide = guide_colorbar(
      barheight = unit(6, "cm"),
      barwidth = unit(0.8, "cm"),
      title.position = "top",
      title.hjust = 0.5
    )
  ) +
  
  coord_equal(clip = "off") +
  theme_void() +
  
  labs(
    title = "U.S. Honeybees Suffer One of Their Deadliest Seasons",
    subtitle = "Estimated share of beekeepers’ colonies lost (Years ending April 1)"
  ) +
  
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 12),
    
    legend.position = "right",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 9),
    
    plot.margin = margin(40, 60, 40, 40)
  )

# ----------------------------------------------------------
# Load libraries
# ----------------------------------------------------------
library(tidyverse)
library(tidytuesdayR)

# ----------------------------------------------------------
# Load data
# ----------------------------------------------------------
tuesdata <- tt_load("2022-01-11")
colony <- tuesdata$colony

# ----------------------------------------------------------
# Prepare yearly U.S. winter colony loss data
# ----------------------------------------------------------
bee_yearly <- colony %>%
  filter(
    state == "United States",
    months == "January-March"
  ) %>%
  mutate(
    season = paste0(year, "-", substr(year + 1, 3, 4))
  ) %>%
  select(season, colony_lost_pct) %>%
  drop_na() %>%
  arrange(season)

# ----------------------------------------------------------
# Create honeycomb layout (positions)
# ----------------------------------------------------------
bee_hex <- bee_yearly %>%
  mutate(
    col = rep(1:4, length.out = n()),
    row = rep(c(2, 1), each = 4, length.out = n()),
    x = col * 2 + if_else(row %% 2 == 0, 1, 0),
    y = row * 2
  )

# ----------------------------------------------------------
# Hexagon function
# ----------------------------------------------------------
hexagon <- function(cx, cy, size = 0.9) {
  tibble(
    x = cx + size * cos(seq(0, 2 * pi, length.out = 7)),
    y = cy + size * sin(seq(0, 2 * pi, length.out = 7))
  )
}

# ----------------------------------------------------------
# Build hexagon polygons
# ----------------------------------------------------------
hex_polygons <- map_dfr(
  1:nrow(bee_hex),
  function(i) {
    hexagon(bee_hex$x[i], bee_hex$y[i]) %>%
      mutate(
        season = bee_hex$season[i],
        colony_lost_pct = bee_hex$colony_lost_pct[i]
      )
  }
)

# ----------------------------------------------------------
# Plot honeycomb infographic (FINAL with LEGEND)
# ----------------------------------------------------------
ggplot(hex_polygons, aes(x, y, group = season, fill = colony_lost_pct)) +
  
  # Hexagons
  geom_polygon(color = "white", linewidth = 1) +
  
  # Percentage labels (above hexagons)
  geom_text(
    data = bee_hex,
    aes(
      x = x,
      y = y + 1.0,
      label = paste0(colony_lost_pct, "%")
    ),
    inherit.aes = FALSE,
    size = 6,
    fontface = "bold",
    color = "#E76F51"
  ) +
  
  # Season labels (below hexagons)
  geom_text(
    data = bee_hex,
    aes(
      x = x,
      y = y - 1.05,
      label = season
    ),
    inherit.aes = FALSE,
    size = 4,
    fontface = "bold"
  ) +
  
  # Fill scale + explanatory legend
  scale_fill_gradient(
    low = "#F6E8C3",
    high = "#F4A261",
    name = "Colony Loss (%)\nDarker = More Bees Lost",
    guide = guide_colorbar(
      barheight = unit(6, "cm"),
      barwidth = unit(0.8, "cm"),
      title.position = "top",
      title.hjust = 0.5
    )
  ) +
  
  coord_equal(clip = "off") +
  theme_void() +
  
  labs(
    title = "U.S. Honeybees Suffer One of Their Deadliest Seasons",
    subtitle = "Estimated share of beekeepers’ colonies lost (Years ending April 1)"
  ) +
  
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 12),
    
    legend.position = "right",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 9),
    
    plot.margin = margin(40, 60, 40, 40)
  )


# ----------------------------------------------------------
# Load libraries
# ----------------------------------------------------------
library(tidyverse)
library(tidytuesdayR)
library(grid)   # for unit()

# ----------------------------------------------------------
# Load data
# ----------------------------------------------------------
tuesdata <- tt_load("2022-01-11")
colony <- tuesdata$colony

# ----------------------------------------------------------
# Select states to compare (edit as desired)
# ----------------------------------------------------------
states_to_plot <- c(
  "California",
  "Texas",
  "Florida",
  "North Dakota",
  "Washington"
)

# ----------------------------------------------------------
# Prepare yearly state-level winter colony loss data
# ----------------------------------------------------------
bee_yearly <- colony %>%
  filter(
    months == "January-March",
    state %in% states_to_plot
  ) %>%
  mutate(
    season = paste0(year, "-", substr(year + 1, 3, 4))
  ) %>%
  select(state, season, colony_lost_pct) %>%
  drop_na() %>%
  arrange(state, season)

# ----------------------------------------------------------
# Create honeycomb layout (same layout reused for each state)
# ----------------------------------------------------------
bee_hex <- bee_yearly %>%
  group_by(state) %>%
  mutate(
    col = rep(1:4, length.out = n()),
    row = rep(c(2, 1), each = 4, length.out = n()),
    x = col * 2 + if_else(row %% 2 == 0, 1, 0),
    y = row * 2
  ) %>%
  ungroup()

# ----------------------------------------------------------
# Hexagon function
# ----------------------------------------------------------
hexagon <- function(cx, cy, size = 0.9) {
  tibble(
    x = cx + size * cos(seq(0, 2 * pi, length.out = 7)),
    y = cy + size * sin(seq(0, 2 * pi, length.out = 7))
  )
}

# ----------------------------------------------------------
# Build hexagon polygons
# ----------------------------------------------------------
hex_polygons <- map_dfr(
  1:nrow(bee_hex),
  function(i) {
    hexagon(bee_hex$x[i], bee_hex$y[i]) %>%
      mutate(
        state = bee_hex$state[i],
        season = bee_hex$season[i],
        colony_lost_pct = bee_hex$colony_lost_pct[i]
      )
  }
)

# ----------------------------------------------------------
# Plot faceted honeycomb infographic
# ----------------------------------------------------------
ggplot(hex_polygons,
       aes(x, y, group = interaction(state, season), fill = colony_lost_pct)) +
  
  # Hexagons
  geom_polygon(color = "white", linewidth = 0.9) +
  
  # Percentage labels
  geom_text(
    data = bee_hex,
    aes(
      x = x,
      y = y + 1.0,
      label = paste0(colony_lost_pct, "%")
    ),
    inherit.aes = FALSE,
    size = 4,
    fontface = "bold",
    color = "#E76F51"
  ) +
  
  # Season labels
  geom_text(
    data = bee_hex,
    aes(
      x = x,
      y = y - 1.05,
      label = season
    ),
    inherit.aes = FALSE,
    size = 3,
    fontface = "bold"
  ) +
  
  # Facet by state
  facet_wrap(~ state) +
  
  # Fill scale + explanatory legend
  scale_fill_gradient(
    low = "#F6E8C3",
    high = "#F4A261",
    name = "Colony Loss (%)\nDarker = More Bees Lost",
    guide = guide_colorbar(
      barheight = unit(6, "cm"),
      barwidth = unit(0.8, "cm"),
      title.position = "top",
      title.hjust = 0.5
    )
  ) +
  
  coord_equal(clip = "off") +
  theme_void() +
  
  labs(
    title = "U.S. Honeybee Colony Losses by State",
    subtitle = "Estimated share of colonies lost during winter (Years ending April 1)"
  ) +
  
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 12),
    
    strip.text = element_text(size = 12, face = "bold"),
    
    legend.position = "right",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 9),
    
    plot.margin = margin(40, 60, 40, 40)
  )


# ----------------------------------------------------------
# Load libraries
# ----------------------------------------------------------
library(tidyverse)
library(tidytuesdayR)
library(grid)

# ----------------------------------------------------------
# Load data
# ----------------------------------------------------------
tuesdata <- tt_load("2022-01-11")
colony <- tuesdata$colony

# ----------------------------------------------------------
# States to compare
# ----------------------------------------------------------
states_to_plot <- c(
  "California",
  "Florida",
  "North Dakota",
  "Texas",
  "Washington"
)

# ----------------------------------------------------------
# Prepare data
# ----------------------------------------------------------
bee_yearly <- colony %>%
  filter(
    months == "January-March",
    state %in% states_to_plot
  ) %>%
  mutate(
    season = paste0(year, "-", substr(year + 1, 3, 4))
  ) %>%
  select(state, season, colony_lost_pct) %>%
  drop_na() %>%
  arrange(state, season)

# ----------------------------------------------------------
# Honeycomb layout
# ----------------------------------------------------------
bee_hex <- bee_yearly %>%
  group_by(state) %>%
  mutate(
    col = rep(1:4, length.out = n()),
    row = rep(c(2, 1), each = 4, length.out = n()),
    x = col * 2 + if_else(row %% 2 == 0, 1, 0),
    y = row * 2
  ) %>%
  ungroup()

# ----------------------------------------------------------
# Hexagon function (slightly smaller)
# ----------------------------------------------------------
hexagon <- function(cx, cy, size = 0.75) {
  tibble(
    x = cx + size * cos(seq(0, 2 * pi, length.out = 7)),
    y = cy + size * sin(seq(0, 2 * pi, length.out = 7))
  )
}

# ----------------------------------------------------------
# Build polygons
# ----------------------------------------------------------
hex_polygons <- map_dfr(
  1:nrow(bee_hex),
  function(i) {
    hexagon(bee_hex$x[i], bee_hex$y[i]) %>%
      mutate(
        state = bee_hex$state[i],
        season = bee_hex$season[i],
        colony_lost_pct = bee_hex$colony_lost_pct[i]
      )
  }
)

# ----------------------------------------------------------
# Plot
# ----------------------------------------------------------
ggplot(
  hex_polygons,
  aes(x, y, group = interaction(state, season), fill = colony_lost_pct)
) +
  
  geom_polygon(color = "white", linewidth = 0.9) +
  
  # Percentage labels
  geom_text(
    data = bee_hex,
    aes(x = x, y = y + 0.95, label = paste0(colony_lost_pct, "%")),
    inherit.aes = FALSE,
    size = 3.8,
    fontface = "bold",
    color = "#E76F51"
  ) +
  
  # Season labels
  geom_text(
    data = bee_hex,
    aes(x = x, y = y - 0.95, label = season),
    inherit.aes = FALSE,
    size = 3,
    fontface = "bold"
  ) +
  
  facet_wrap(~ state, ncol = 3) +
  
  scale_fill_gradient(
    low = "#F6E8C3",
    high = "#F4A261",
    name = "Colony Loss (%)\nDarker = More Bees Lost",
    guide = guide_colorbar(
      barheight = unit(6, "cm"),
      barwidth = unit(0.8, "cm"),
      title.position = "top",
      title.hjust = 0.5
    )
  ) +
  
  # ⬅️ THIS is the key spacing fix inside panels
  coord_equal(
    clip = "off",
    xlim = c(1.5, 10.5),
    ylim = c(1, 6)
  ) +
  
  theme_void() +
  
  labs(
    title = "U.S. Honeybee Colony Losses by State",
    subtitle = "Estimated share of colonies lost during winter (Years ending April 1)"
  ) +
  
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 12),
    
    strip.text = element_text(size = 13, face = "bold"),
    
    # ⬅️ THIS spreads the facets apart
    panel.spacing = unit(2.2, "lines"),
    
    legend.position = "right",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 9),
    
    plot.margin = margin(40, 70, 40, 40)
  )


# ----------------------------------------------------------
# Load libraries
# ----------------------------------------------------------
library(tidyverse)
library(tidytuesdayR)
library(grid)

# ----------------------------------------------------------
# Load data
# ----------------------------------------------------------
tuesdata <- tt_load("2022-01-11")
colony <- tuesdata$colony

# ----------------------------------------------------------
# Identify top 6 states with biggest increase in colony loss
# ----------------------------------------------------------
top_states <- colony %>%
  filter(
    months == "January-March",
    state != "United States"
  ) %>%
  mutate(
    season = paste0(year, "-", substr(year + 1, 3, 4))
  ) %>%
  arrange(state, year) %>%
  group_by(state) %>%
  summarise(
    first_loss = first(colony_lost_pct),
    last_loss  = last(colony_lost_pct),
    change = last_loss - first_loss,
    .groups = "drop"
  ) %>%
  arrange(desc(change)) %>%
  slice_head(n = 6)

states_to_plot <- top_states$state

# ----------------------------------------------------------
# Prepare plotting data
# ----------------------------------------------------------
bee_yearly <- colony %>%
  filter(
    months == "January-March",
    state %in% states_to_plot
  ) %>%
  mutate(
    season = paste0(year, "-", substr(year + 1, 3, 4))
  ) %>%
  select(state, season, colony_lost_pct) %>%
  drop_na() %>%
  arrange(state, season)

# ----------------------------------------------------------
# Honeycomb layout
# ----------------------------------------------------------
bee_hex <- bee_yearly %>%
  group_by(state) %>%
  mutate(
    col = rep(1:4, length.out = n()),
    row = rep(c(2, 1), each = 4, length.out = n()),
    x = col * 2 + if_else(row %% 2 == 0, 1, 0),
    y = row * 2
  ) %>%
  ungroup()

# ----------------------------------------------------------
# Hexagon function
# ----------------------------------------------------------
hexagon <- function(cx, cy, size = 0.75) {
  tibble(
    x = cx + size * cos(seq(0, 2 * pi, length.out = 7)),
    y = cy + size * sin(seq(0, 2 * pi, length.out = 7))
  )
}

# ----------------------------------------------------------
# Build hex polygons
# ----------------------------------------------------------
hex_polygons <- map_dfr(
  1:nrow(bee_hex),
  function(i) {
    hexagon(bee_hex$x[i], bee_hex$y[i]) %>%
      mutate(
        state = bee_hex$state[i],
        season = bee_hex$season[i],
        colony_lost_pct = bee_hex$colony_lost_pct[i]
      )
  }
)

# ----------------------------------------------------------
# Plot
# ----------------------------------------------------------
ggplot(
  hex_polygons,
  aes(x, y, group = interaction(state, season), fill = colony_lost_pct)
) +
  
  geom_polygon(color = "white", linewidth = 0.9) +
  
  geom_text(
    data = bee_hex,
    aes(x = x, y = y + 0.95, label = paste0(colony_lost_pct, "%")),
    inherit.aes = FALSE,
    size = 3.8,
    fontface = "bold",
    color = "#E76F51"
  ) +
  
  geom_text(
    data = bee_hex,
    aes(x = x, y = y - 0.95, label = season),
    inherit.aes = FALSE,
    size = 3,
    fontface = "bold"
  ) +
  
  facet_wrap(~ state, ncol = 3) +
  
  scale_fill_gradient(
    low = "#F6E8C3",
    high = "#F4A261",
    name = "Colony Loss (%)\nDarker = More Bees Lost",
    guide = guide_colorbar(
      barheight = unit(6, "cm"),
      barwidth = unit(0.8, "cm"),
      title.position = "top",
      title.hjust = 0.5
    )
  ) +
  
  coord_equal(
    clip = "off",
    xlim = c(1.5, 10.5),
    ylim = c(1, 6)
  ) +
  
  theme_void() +
  
  labs(
    title = "Top 6 U.S. States With Largest Increases in Honeybee Colony Loss",
    subtitle = "States ranked by increase in winter colony losses over time"
  ) +
  
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 12),
    strip.text = element_text(size = 13, face = "bold"),
    panel.spacing = unit(2.3, "lines"),
    legend.position = "right",
    plot.margin = margin(40, 70, 40, 40)
  )


# ----------------------------------------------------------
# Load libraries
# ----------------------------------------------------------
library(tidyverse)
library(tidytuesdayR)
library(grid)

# ----------------------------------------------------------
# Load data
# ----------------------------------------------------------
tuesdata <- tt_load("2022-01-11")
colony <- tuesdata$colony

# ----------------------------------------------------------
# Identify top 6 states with highest colony losses (AVERAGE)
# ----------------------------------------------------------
top_states <- colony %>%
  filter(
    months == "January-March",
    state != "United States"
  ) %>%
  group_by(state) %>%
  summarise(
    avg_loss = mean(colony_lost_pct, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_loss)) %>%
  slice_head(n = 6)

states_to_plot <- top_states$state

# ----------------------------------------------------------
# Prepare plotting data
# ----------------------------------------------------------
bee_yearly <- colony %>%
  filter(
    months == "January-March",
    state %in% states_to_plot
  ) %>%
  mutate(
    season = paste0(year, "-", substr(year + 1, 3, 4))
  ) %>%
  select(state, season, colony_lost_pct) %>%
  drop_na() %>%
  arrange(state, season)

# ----------------------------------------------------------
# Honeycomb layout
# ----------------------------------------------------------
bee_hex <- bee_yearly %>%
  group_by(state) %>%
  mutate(
    col = rep(1:4, length.out = n()),
    row = rep(c(2, 1), each = 4, length.out = n()),
    x = col * 2 + if_else(row %% 2 == 0, 1, 0),
    y = row * 2
  ) %>%
  ungroup()

# ----------------------------------------------------------
# Hexagon function
# ----------------------------------------------------------
hexagon <- function(cx, cy, size = 0.75) {
  tibble(
    x = cx + size * cos(seq(0, 2 * pi, length.out = 7)),
    y = cy + size * sin(seq(0, 2 * pi, length.out = 7))
  )
}

# ----------------------------------------------------------
# Build polygons
# ----------------------------------------------------------
hex_polygons <- map_dfr(
  1:nrow(bee_hex),
  function(i) {
    hexagon(bee_hex$x[i], bee_hex$y[i]) %>%
      mutate(
        state = bee_hex$state[i],
        season = bee_hex$season[i],
        colony_lost_pct = bee_hex$colony_lost_pct[i]
      )
  }
)

# ----------------------------------------------------------
# Plot
# ----------------------------------------------------------
ggplot(
  hex_polygons,
  aes(x, y, group = interaction(state, season), fill = colony_lost_pct)
) +
  
  geom_polygon(color = "white", linewidth = 0.9) +
  
  geom_text(
    data = bee_hex,
    aes(x = x, y = y + 0.95, label = paste0(colony_lost_pct, "%")),
    inherit.aes = FALSE,
    size = 3.8,
    fontface = "bold",
    color = "#E76F51"
  ) +
  
  geom_text(
    data = bee_hex,
    aes(x = x, y = y - 0.95, label = season),
    inherit.aes = FALSE,
    size = 3,
    fontface = "bold"
  ) +
  
  facet_wrap(~ state, ncol = 3) +
  
  scale_fill_gradient(
    low = "#F6E8C3",
    high = "#F4A261",
    name = "Colony Loss (%)\nDarker = More Bees Lost",
    guide = guide_colorbar(
      barheight = unit(6, "cm"),
      barwidth = unit(0.8, "cm"),
      title.position = "top",
      title.hjust = 0.5
    )
  ) +
  
  coord_equal(
    clip = "off",
    xlim = c(1.5, 10.5),
    ylim = c(1, 6)
  ) +
  
  theme_void() +
  
  labs(
    title = "Top 6 U.S. States With Highest Honeybee Colony Losses",
    subtitle = "Ranked by average winter colony loss percentage"
  ) +
  
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 12),
    strip.text = element_text(size = 13, face = "bold"),
    panel.spacing = unit(2.3, "lines"),
    legend.position = "right",
    plot.margin = margin(40, 70, 40, 40)
  )

# ----------------------------------------------------------
# Load libraries
# ----------------------------------------------------------
library(tidyverse)
library(tidytuesdayR)

# ----------------------------------------------------------
# Load data
# ----------------------------------------------------------
tuesdata <- tt_load("2022-01-11")
colony <- tuesdata$colony

# ----------------------------------------------------------
# Identify top 6 states with highest colony losses (AVERAGE)
# ----------------------------------------------------------
top_states <- colony %>%
  filter(
    months == "January-March",
    state != "United States"
  ) %>%
  group_by(state) %>%
  summarise(
    avg_loss = mean(colony_lost_pct, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_loss)) %>%
  slice_head(n = 6)

states_to_plot <- top_states$state

# ----------------------------------------------------------
# Prepare plotting data
# ----------------------------------------------------------
bee_yearly <- colony %>%
  filter(
    months == "January-March",
    state %in% states_to_plot
  ) %>%
  mutate(
    season = paste0(year, "-", substr(year + 1, 3, 4))
  ) %>%
  select(state, season, colony_lost_pct) %>%
  drop_na()

# ----------------------------------------------------------
# Create x/y positions
# ----------------------------------------------------------
bee_hex <- bee_yearly %>%
  mutate(
    season = factor(season, levels = sort(unique(season))),
    state  = factor(state, levels = rev(states_to_plot))
  ) %>%
  mutate(
    x = as.numeric(season) * 2,
    y = as.numeric(state) * 2
  )

# ----------------------------------------------------------
# Hexagon function
# ----------------------------------------------------------
hexagon <- function(cx, cy, size = 0.75) {
  tibble(
    x = cx + size * cos(seq(0, 2 * pi, length.out = 7)),
    y = cy + size * sin(seq(0, 2 * pi, length.out = 7))
  )
}

# ----------------------------------------------------------
# Build polygons
# ----------------------------------------------------------
hex_polygons <- map_dfr(
  1:nrow(bee_hex),
  function(i) {
    hexagon(bee_hex$x[i], bee_hex$y[i]) %>%
      mutate(
        state = bee_hex$state[i],
        season = bee_hex$season[i],
        colony_lost_pct = bee_hex$colony_lost_pct[i]
      )
  }
)

# ----------------------------------------------------------
# Plot
# ----------------------------------------------------------
ggplot(
  hex_polygons,
  aes(
    x, y,
    group = interaction(state, season),
    fill = colony_lost_pct
  )
) +
  
  geom_polygon(color = "white", linewidth = 0.9) +
  
  geom_text(
    data = bee_hex,
    aes(x = x, y = y + 0.9, label = paste0(colony_lost_pct, "%")),
    inherit.aes = FALSE,
    size = 3.5,
    fontface = "bold",
    color = "#E76F51"
  ) +
  
  geom_text(
    data = bee_hex,
    aes(x = x, y = y - 0.9, label = season),
    inherit.aes = FALSE,
    size = 3,
    fontface = "bold"
  ) +
  
  scale_fill_gradient(
    low = "#F6E8C3",
    high = "#F4A261",
    name = "Colony Loss (%)\nDarker = More Bees Lost"
  ) +
  
  scale_y_continuous(
    breaks = unique(bee_hex$y),
    labels = levels(bee_hex$state)
  ) +
  
  scale_x_continuous(
    breaks = unique(bee_hex$x),
    labels = levels(bee_hex$season)
  ) +
  
  coord_equal(clip = "off") +
  
  theme_void() +
  
  labs(
    title = "Top 6 U.S. States With Highest Honeybee Colony Losses",
    subtitle = "States on Y-axis, Winter Seasons on X-axis"
  ) +
  
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "right",
    plot.margin = margin(40, 60, 40, 60)
  )


# ----------------------------------------------------------
# Load libraries
# ----------------------------------------------------------
library(tidyverse)
library(tidytuesdayR)

# ----------------------------------------------------------
# Load data
# ----------------------------------------------------------
tuesdata <- tt_load("2022-01-11")
colony <- tuesdata$colony

# ----------------------------------------------------------
# Identify top 6 states with highest colony losses (AVERAGE)
# ----------------------------------------------------------
top_states <- colony %>%
  filter(
    months == "January-March",
    state != "United States"
  ) %>%
  group_by(state) %>%
  summarise(
    avg_loss = mean(colony_lost_pct, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_loss)) %>%
  slice_head(n = 6)

states_to_plot <- top_states$state

# ----------------------------------------------------------
# Prepare plotting data
# ----------------------------------------------------------
bee_yearly <- colony %>%
  filter(
    months == "January-March",
    state %in% states_to_plot
  ) %>%
  mutate(
    season = paste0(year, "-", substr(year + 1, 3, 4))
  ) %>%
  select(state, season, colony_lost_pct) %>%
  drop_na()

# ----------------------------------------------------------
# Create x/y positions
# ----------------------------------------------------------
bee_hex <- bee_yearly %>%
  mutate(
    season = factor(season, levels = sort(unique(season))),
    state  = factor(state, levels = rev(states_to_plot))
  ) %>%
  mutate(
    x = as.numeric(season) * 2,
    y = as.numeric(state) * 2
  )

# ----------------------------------------------------------
# Hexagon function
# ----------------------------------------------------------
hexagon <- function(cx, cy, size = 0.75) {
  tibble(
    x = cx + size * cos(seq(0, 2 * pi, length.out = 7)),
    y = cy + size * sin(seq(0, 2 * pi, length.out = 7))
  )
}

# ----------------------------------------------------------
# Build hexagon polygons
# ----------------------------------------------------------
hex_polygons <- map_dfr(
  seq_len(nrow(bee_hex)),
  function(i) {
    hexagon(bee_hex$x[i], bee_hex$y[i]) %>%
      mutate(
        state = bee_hex$state[i],
        season = bee_hex$season[i],
        colony_lost_pct = bee_hex$colony_lost_pct[i]
      )
  }
)

# ----------------------------------------------------------
# Plot
# ----------------------------------------------------------
ggplot(
  hex_polygons,
  aes(
    x, y,
    group = interaction(state, season),
    fill = colony_lost_pct
  )
) +
  
  # Hexagons
  geom_polygon(color = "white", linewidth = 0.9) +
  
  # Percentages centered inside hexagons
  geom_text(
    data = bee_hex,
    aes(
      x = x,
      y = y,
      label = paste0(colony_lost_pct, "%")
    ),
    inherit.aes = FALSE,
    size = 3.8,
    fontface = "bold",
    color = "#9C2D1C"
  ) +
  
  # Color scale
  scale_fill_gradient(
    low = "#F6E8C3",
    high = "#F4A261",
    name = "Colony Loss (%)\nDarker = More Bees Lost"
  ) +
  
  # Y-axis: state names
  scale_y_continuous(
    breaks = unique(bee_hex$y),
    labels = levels(bee_hex$state),
    expand = expansion(mult = c(0.05, 0.05))
  ) +
  
  # X-axis: winter seasons
  scale_x_continuous(
    breaks = unique(bee_hex$x),
    labels = levels(bee_hex$season),
    expand = expansion(mult = c(0.05, 0.05))
  ) +
  
  coord_equal(clip = "off") +
  
  labs(
    title = "Top 6 U.S. States With Highest Honeybee Colony Losses",
    subtitle = "States on Y-axis, Winter Seasons on X-axis",
    x = NULL,
    y = NULL
  ) +
  
  theme_void() +
  
  theme(
    plot.title = element_text(size = 22, face = "bold"),
    plot.subtitle = element_text(size = 13),
    axis.text.x = element_text(
      size = 11,
      face = "bold",
      margin = margin(t = 10)
    ),
    axis.text.y = element_text(
      size = 12,
      face = "bold",
      margin = margin(r = 10)
    ),
    legend.position = "right",
    plot.margin = margin(40, 70, 40, 70)
  )

# ----------------------------------------------------------
# Load libraries
# ----------------------------------------------------------
library(tidyverse)
library(tidytuesdayR)
library(scales)

# ----------------------------------------------------------
# Load data
# ----------------------------------------------------------
tuesdata <- tt_load("2022-01-11")
colony <- tuesdata$colony

# ----------------------------------------------------------
# Identify top 6 states with highest colony losses (AVERAGE)
# ----------------------------------------------------------
top_states <- colony %>%
  filter(
    months == "January-March",
    state != "United States"
  ) %>%
  group_by(state) %>%
  summarise(
    avg_loss = mean(colony_lost_pct, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_loss)) %>%
  slice_head(n = 6)

states_to_plot <- top_states$state

# ----------------------------------------------------------
# Prepare plotting data
# ----------------------------------------------------------
bee_yearly <- colony %>%
  filter(
    months == "January-March",
    state %in% states_to_plot
  ) %>%
  mutate(
    season = paste0(year, "-", substr(year + 1, 3, 4))
  ) %>%
  select(state, season, colony_lost_pct) %>%
  drop_na()

# ----------------------------------------------------------
# Create x/y positions
# ----------------------------------------------------------
bee_hex <- bee_yearly %>%
  mutate(
    season = factor(season, levels = sort(unique(season))),
    state  = factor(state, levels = rev(states_to_plot))
  ) %>%
  mutate(
    x = as.numeric(season) * 2,
    y = as.numeric(state) * 2
  )

# ----------------------------------------------------------
# Hexagon function
# ----------------------------------------------------------
hexagon <- function(cx, cy, size = 0.75) {
  tibble(
    x = cx + size * cos(seq(0, 2 * pi, length.out = 7)),
    y = cy + size * sin(seq(0, 2 * pi, length.out = 7))
  )
}

# ----------------------------------------------------------
# Build hexagon polygons
# ----------------------------------------------------------
hex_polygons <- map_dfr(
  seq_len(nrow(bee_hex)),
  function(i) {
    hexagon(bee_hex$x[i], bee_hex$y[i]) %>%
      mutate(
        state = bee_hex$state[i],
        season = bee_hex$season[i],
        colony_lost_pct = bee_hex$colony_lost_pct[i]
      )
  }
)

# ----------------------------------------------------------
# Plot
# ----------------------------------------------------------
ggplot(
  hex_polygons,
  aes(
    x, y,
    group = interaction(state, season),
    fill = colony_lost_pct
  )
) +
  
  # Hexagons
  geom_polygon(
    color = "white",
    linewidth = 1.2
  ) +
  
  # Color scale (stronger contrast)
  scale_fill_gradientn(
    colors = c("#FDF0D5", "#F4A261", "#9C2D1C"),
    limits = c(5, 50),
    oob = scales::squish,
    name = "Colony Loss (%)\nDarker = More Bees Lost"
  ) +
  
  # Y-axis: state names
  scale_y_continuous(
    breaks = unique(bee_hex$y),
    labels = levels(bee_hex$state),
    expand = expansion(mult = c(0.05, 0.05))
  ) +
  
  # X-axis: winter seasons
  scale_x_continuous(
    breaks = unique(bee_hex$x),
    labels = levels(bee_hex$season),
    expand = expansion(mult = c(0.05, 0.05))
  ) +
  
  coord_equal(clip = "off") +
  
  labs(
    title = "Top 6 U.S. States With Highest Honeybee Colony Losses",
    subtitle = "States on Y-axis, Winter Seasons on X-axis",
    x = NULL,
    y = NULL
  ) +
  
  theme_void() +
  
  theme(
    plot.title = element_text(size = 22, face = "bold"),
    plot.subtitle = element_text(size = 13),
    axis.text.x = element_text(
      size = 11,
      face = "bold",
      margin = margin(t = 10)
    ),
    axis.text.y = element_text(
      size = 12,
      face = "bold",
      margin = margin(r = 10)
    ),
    legend.position = "right",
    plot.margin = margin(40, 70, 40, 70)
  )


# ----------------------------------------------------------
# Load libraries
# ----------------------------------------------------------
library(tidyverse)
library(tidytuesdayR)
library(scales)

# ----------------------------------------------------------
# Load data
# ----------------------------------------------------------
tuesdata <- tt_load("2022-01-11")
colony <- tuesdata$colony

# ----------------------------------------------------------
# Identify top 6 states with highest colony losses (AVERAGE)
# ----------------------------------------------------------
top_states <- colony %>%
  filter(
    months == "January-March",
    state != "United States"
  ) %>%
  group_by(state) %>%
  summarise(
    avg_loss = mean(colony_lost_pct, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_loss)) %>%
  slice_head(n = 6)

states_to_plot <- top_states$state

# ----------------------------------------------------------
# Prepare plotting data
# ----------------------------------------------------------
bee_yearly <- colony %>%
  filter(
    months == "January-March",
    state %in% states_to_plot
  ) %>%
  mutate(
    season = paste0(year, "-", substr(year + 1, 3, 4))
  ) %>%
  select(state, season, colony_lost_pct) %>%
  drop_na()

# ----------------------------------------------------------
# Create x/y positions
# ----------------------------------------------------------
bee_hex <- bee_yearly %>%
  mutate(
    season = factor(season, levels = sort(unique(season))),
    state  = factor(state, levels = rev(states_to_plot))
  ) %>%
  mutate(
    x = as.numeric(season) * 2,
    y = as.numeric(state) * 2
  )

# ----------------------------------------------------------
# Hexagon function
# ----------------------------------------------------------
hexagon <- function(cx, cy, size = 0.75) {
  tibble(
    x = cx + size * cos(seq(0, 2 * pi, length.out = 7)),
    y = cy + size * sin(seq(0, 2 * pi, length.out = 7))
  )
}

# ----------------------------------------------------------
# Build hexagon polygons
# ----------------------------------------------------------
hex_polygons <- map_dfr(
  seq_len(nrow(bee_hex)),
  function(i) {
    hexagon(bee_hex$x[i], bee_hex$y[i]) %>%
      mutate(
        state = bee_hex$state[i],
        season = bee_hex$season[i],
        colony_lost_pct = bee_hex$colony_lost_pct[i]
      )
  }
)

# ----------------------------------------------------------
# Identify extreme values for labeling (min & max per state)
# ----------------------------------------------------------
label_data <- bee_hex %>%
  group_by(state) %>%
  filter(
    colony_lost_pct == max(colony_lost_pct) |
      colony_lost_pct == min(colony_lost_pct)
  )

# ----------------------------------------------------------
# Plot
# ----------------------------------------------------------
ggplot(
  hex_polygons,
  aes(
    x, y,
    group = interaction(state, season),
    fill = colony_lost_pct
  )
) +
  
  # Hexagons
  geom_polygon(
    color = "white",
    linewidth = 1.2
  ) +
  
  # Numeric labels (extremes only)
  geom_text(
    data = label_data,
    aes(x = x, y = y, label = paste0(round(colony_lost_pct), "%")),
    inherit.aes = FALSE,
    size = 3.5,
    fontface = "bold",
    color = "white"
  ) +
  
  # Color scale
  scale_fill_gradientn(
    colors = c("#FDF0D5", "#F4A261", "#9C2D1C"),
    limits = c(5, 50),
    oob = scales::squish,
    name = "Colony Loss (%)\nDarker = Higher Loss"
  ) +
  
  # Y-axis: states
  scale_y_continuous(
    breaks = unique(bee_hex$y),
    labels = levels(bee_hex$state),
    expand = expansion(mult = c(0.05, 0.05))
  ) +
  
  # X-axis: seasons
  scale_x_continuous(
    breaks = unique(bee_hex$x),
    labels = levels(bee_hex$season),
    expand = expansion(mult = c(0.05, 0.05))
  ) +
  
  coord_equal(clip = "off") +
  
  labs(
    title = "Top 6 U.S. States With Highest Honeybee Colony Losses",
    subtitle = "Winter colony loss percentages by state (January–March)",
    caption = "Source: USDA / Bee Informed Partnership (via TidyTuesday, 2022)\nStates shown are those with the highest average winter losses across all years.",
    x = NULL,
    y = NULL
  ) +
  
  theme_void() +
  
  theme(
    plot.title = element_text(size = 22, face = "bold"),
    plot.subtitle = element_text(size = 13),
    axis.text.x = element_text(
      size = 11,
      face = "bold",
      margin = margin(t = 10)
    ),
    axis.text.y = element_text(
      size = 12,
      face = "bold",
      margin = margin(r = 10)
    ),
    legend.position = "right",
    plot.caption = element_text(
      size = 10,
      color = "gray40",
      margin = margin(t = 20)
    ),
    plot.margin = margin(40, 70, 40, 70)
  )


# ----------------------------------------------------------
# Load libraries
# ----------------------------------------------------------
library(tidyverse)
library(tidytuesdayR)
library(scales)

# ----------------------------------------------------------
# Load data
# ----------------------------------------------------------
tuesdata <- tt_load("2022-01-11")
colony <- tuesdata$colony

# ----------------------------------------------------------
# Identify top 6 states with highest colony losses (AVERAGE)
# ----------------------------------------------------------
top_states <- colony %>%
  filter(
    months == "January-March",
    state != "United States"
  ) %>%
  group_by(state) %>%
  summarise(
    avg_loss = mean(colony_lost_pct, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_loss)) %>%
  slice_head(n = 6)

states_to_plot <- top_states$state

# ----------------------------------------------------------
# Prepare plotting data
# ----------------------------------------------------------
bee_yearly <- colony %>%
  filter(
    months == "January-March",
    state %in% states_to_plot
  ) %>%
  mutate(
    season = paste0(year, "-", substr(year + 1, 3, 4))
  ) %>%
  select(state, season, colony_lost_pct) %>%
  drop_na()

# ----------------------------------------------------------
# Create x/y positions
# ----------------------------------------------------------
bee_hex <- bee_yearly %>%
  mutate(
    season = factor(season, levels = sort(unique(season))),
    state  = factor(state, levels = rev(states_to_plot))
  ) %>%
  mutate(
    x = as.numeric(season) * 2,
    y = as.numeric(state) * 2
  )

# ----------------------------------------------------------
# Hexagon function
# ----------------------------------------------------------
hexagon <- function(cx, cy, size = 0.75) {
  tibble(
    x = cx + size * cos(seq(0, 2 * pi, length.out = 7)),
    y = cy + size * sin(seq(0, 2 * pi, length.out = 7))
  )
}

# ----------------------------------------------------------
# Build hexagon polygons
# ----------------------------------------------------------
hex_polygons <- map_dfr(
  seq_len(nrow(bee_hex)),
  function(i) {
    hexagon(bee_hex$x[i], bee_hex$y[i]) %>%
      mutate(
        state = bee_hex$state[i],
        season = bee_hex$season[i],
        colony_lost_pct = bee_hex$colony_lost_pct[i]
      )
  }
)

# ----------------------------------------------------------
# Identify extreme values for labeling (min & max per state)
# ----------------------------------------------------------
label_data <- bee_hex %>%
  group_by(state) %>%
  filter(
    colony_lost_pct == max(colony_lost_pct) |
      colony_lost_pct == min(colony_lost_pct)
  )

# ----------------------------------------------------------
# Plot
# ----------------------------------------------------------
ggplot(
  hex_polygons,
  aes(
    x, y,
    group = interaction(state, season),
    fill = colony_lost_pct
  )
) +
  
  # Hexagons
  geom_polygon(
    color = "white",
    linewidth = 1.2
  ) +
  
  # Numeric labels (extremes only)
  geom_text(
    data = label_data,
    aes(x = x, y = y, label = paste0(round(colony_lost_pct), "%")),
    inherit.aes = FALSE,
    size = 3.6,
    fontface = "bold",
    color = "white"
  ) +
  
  # Stronger, nonlinear color scale for urgency
  scale_fill_gradientn(
    colors = c(
      "#FFF3E0",  # very low loss
      "#F4A261",  # moderate loss
      "#C44536",  # high loss
      "#6A1B0A"   # extreme loss (urgent)
    ),
    values = scales::rescale(c(5, 20, 35, 50)),
    limits = c(5, 50),
    oob = scales::squish,
    name = "Colony Loss (%)\nDarker = Higher Loss"
  ) +
  
  # Y-axis: states
  scale_y_continuous(
    breaks = unique(bee_hex$y),
    labels = levels(bee_hex$state),
    expand = expansion(mult = c(0.05, 0.05))
  ) +
  
  # X-axis: seasons
  scale_x_continuous(
    breaks = unique(bee_hex$x),
    labels = levels(bee_hex$season),
    expand = expansion(mult = c(0.05, 0.05))
  ) +
  
  coord_equal(clip = "off") +
  
  labs(
    title = "Top 6 U.S. States With Highest Honeybee Colony Losses",
    subtitle = "Winter colony loss percentages by state (January–March)",
    caption = "Source: USDA / Bee Informed Partnership (via TidyTuesday, 2022)\nStates shown are those with the highest average winter losses across all years.",
    x = NULL,
    y = NULL
  ) +
  
  theme_void() +
  
  theme(
    plot.title = element_text(size = 22, face = "bold"),
    plot.subtitle = element_text(size = 13),
    axis.text.x = element_text(
      size = 11,
      face = "bold",
      margin = margin(t = 10)
    ),
    axis.text.y = element_text(
      size = 12,
      face = "bold",
      margin = margin(r = 10)
    ),
    legend.position = "right",
    plot.caption = element_text(
      size = 10,
      color = "gray40",
      margin = margin(t = 20)
    ),
    plot.margin = margin(40, 70, 40, 70)
  )


# ----------------------------------------------------------
# Load libraries
# ----------------------------------------------------------
library(tidyverse)
library(tidytuesdayR)
library(scales)

# ----------------------------------------------------------
# Load data
# ----------------------------------------------------------
tuesdata <- tt_load("2022-01-11")
colony <- tuesdata$colony

# ----------------------------------------------------------
# Identify top 6 states with highest colony losses (AVERAGE)
# ----------------------------------------------------------
top_states <- colony %>%
  filter(
    months == "January-March",
    state != "United States"
  ) %>%
  group_by(state) %>%
  summarise(
    avg_loss = mean(colony_lost_pct, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_loss)) %>%
  slice_head(n = 6)

states_to_plot <- top_states$state

# ----------------------------------------------------------
# Prepare plotting data
# ----------------------------------------------------------
bee_yearly <- colony %>%
  filter(
    months == "January-March",
    state %in% states_to_plot
  ) %>%
  mutate(
    season = paste0(year, "-", substr(year + 1, 3, 4))
  ) %>%
  select(state, season, colony_lost_pct) %>%
  drop_na()

# ----------------------------------------------------------
# Create x/y positions
# ----------------------------------------------------------
bee_hex <- bee_yearly %>%
  mutate(
    season = factor(season, levels = sort(unique(season))),
    state  = factor(state, levels = rev(states_to_plot))
  ) %>%
  mutate(
    x = as.numeric(season) * 2,
    y = as.numeric(state) * 2
  )

# ----------------------------------------------------------
# Hexagon function
# ----------------------------------------------------------
hexagon <- function(cx, cy, size = 0.75) {
  tibble(
    x = cx + size * cos(seq(0, 2 * pi, length.out = 7)),
    y = cy + size * sin(seq(0, 2 * pi, length.out = 7))
  )
}

# ----------------------------------------------------------
# Build hexagon polygons
# ----------------------------------------------------------
hex_polygons <- map_dfr(
  seq_len(nrow(bee_hex)),
  function(i) {
    hexagon(bee_hex$x[i], bee_hex$y[i]) %>%
      mutate(
        state = bee_hex$state[i],
        season = bee_hex$season[i],
        colony_lost_pct = bee_hex$colony_lost_pct[i]
      )
  }
)

# ----------------------------------------------------------
# Label ONLY higher-loss values (>= 30%)
# ----------------------------------------------------------
label_data <- bee_hex %>%
  filter(colony_lost_pct >= 30)

# ----------------------------------------------------------
# Plot
# ----------------------------------------------------------
ggplot(
  hex_polygons,
  aes(
    x, y,
    group = interaction(state, season),
    fill = colony_lost_pct
  )
) +
  
  # Hexagons
  geom_polygon(
    color = "white",
    linewidth = 1.2
  ) +
  
  # Numeric labels for high loss only
  geom_text(
    data = label_data,
    aes(x = x, y = y, label = paste0(round(colony_lost_pct), "%")),
    inherit.aes = FALSE,
    size = 3.6,
    fontface = "bold",
    color = "white"
  ) +
  
  # Strong, nonlinear color scale (urgent)
  scale_fill_gradientn(
    colors = c(
      "#FFF3E0",  # very low loss
      "#F4A261",  # moderate loss
      "#C44536",  # high loss
      "#6A1B0A"   # extreme loss
    ),
    values = scales::rescale(c(5, 20, 35, 50)),
    limits = c(5, 50),
    oob = scales::squish,
    name = "Colony Loss (%)\nDarker = Higher Loss"
  ) +
  
  # Y-axis: states
  scale_y_continuous(
    breaks = unique(bee_hex$y),
    labels = levels(bee_hex$state),
    expand = expansion(mult = c(0.05, 0.05))
  ) +
  
  # X-axis: winter seasons
  scale_x_continuous(
    breaks = unique(bee_hex$x),
    labels = levels(bee_hex$season),
    expand = expansion(mult = c(0.05, 0.05))
  ) +
  
  coord_equal(clip = "off") +
  
  labs(
    title = "Top 6 U.S. States With Highest Honeybee Colony Losses",
    subtitle = "Winter colony loss percentages by state (January–March)",
    caption = "Source: USDA / Bee Informed Partnership (via TidyTuesday, 2022)\nStates shown are those with the highest average winter losses across all years.",
    x = NULL,
    y = NULL
  ) +
  
  theme_void() +
  
  theme(
    plot.title = element_text(size = 22, face = "bold"),
    plot.subtitle = element_text(size = 13),
    axis.text.x = element_text(
      size = 11,
      face = "bold",
      margin = margin(t = 10)
    ),
    axis.text.y = element_text(
      size = 12,
      face = "bold",
      margin = margin(r = 10)
    ),
    legend.position = "right",
    plot.caption = element_text(
      size = 10,
      color = "gray40",
      margin = margin(t = 20)
    ),
    plot.margin = margin(40, 70, 40, 70)
  )





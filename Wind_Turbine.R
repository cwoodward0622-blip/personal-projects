
# ---------------------------
# Load Libraries
# ---------------------------
library(tidyverse)
library(readxl)
library(stringr)

# ---------------------------
# Load Data
# ---------------------------
df <- read_excel("Wind_Turbine_Database_en (2).xlsx")

# ---------------------------
# Clean Commissioning Year
# ---------------------------
df <- df %>%
  mutate(
    Commissioning = as.character(Commissioning),
    Commissioning_year = str_extract(Commissioning, "\\d{4}") %>%
      as.numeric()
  ) %>%
  filter(!is.na(Commissioning_year))

# ---------------------------
# Create Recommended Time Periods
# ---------------------------
df <- df %>%
  mutate(
    Period = case_when(
      Commissioning_year >= 2000 & Commissioning_year <= 2005 ~ "2000–2005",
      Commissioning_year >= 2006 & Commissioning_year <= 2010 ~ "2006–2010",
      Commissioning_year >= 2011 & Commissioning_year <= 2015 ~ "2011–2015",
      Commissioning_year >= 2016 & Commissioning_year <= 2020 ~ "2016–2020",
      Commissioning_year >= 2021 & Commissioning_year <= 2025 ~ "2021–2025",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Period))

# ---------------------------
# Order Periods Chronologically
# ---------------------------
df$Period <- factor(
  df$Period,
  levels = c("2000–2005", "2006–2010", "2011–2015",
             "2016–2020", "2021–2025")
)

# ---------------------------
# Compute Period Totals for Labels
# ---------------------------
period_totals <- df %>%
  count(Period, name = "total_turbines")

df <- df %>%
  left_join(period_totals, by = "Period") %>%
  mutate(
    Period_label = paste0(Period, "\n(n = ", total_turbines, ")")
  )

# ---------------------------
# Summarize Turbines by Period + Province
# ---------------------------
period_summary <- df %>%
  group_by(Period, Period_label, Province_Territory) %>%
  summarise(num_turbines = n(), .groups = "drop") %>%
  group_by(Period, Period_label) %>%
  mutate(
    percent_exact = num_turbines / sum(num_turbines) * 100,
    percent_floor = floor(percent_exact),
    remainder = 100 - sum(percent_floor)
  ) %>%
  arrange(Period, desc(percent_exact)) %>%
  mutate(
    percent_adjusted = percent_floor +
      ifelse(row_number() <= remainder[1], 1, 0)
  ) %>%
  ungroup()

# ---------------------------
# Expand to 100-Tile Waffle Grid
# ---------------------------
grid_data <- period_summary %>%
  group_by(Period, Period_label) %>%
  uncount(percent_adjusted) %>%
  mutate(
    tile_id = row_number(),
    row = ceiling(tile_id / 10),
    col = ifelse(tile_id %% 10 == 0, 10, tile_id %% 10),
    row = 11 - row   # build bottom-left upward
  ) %>%
  ungroup()

# ---------------------------
# Ensure Facet Order Correct
# ---------------------------
grid_data$Period_label <- factor(
  grid_data$Period_label,
  levels = unique(df$Period_label[order(df$Period)])
)

# ---------------------------
# Order Provinces by Overall Contribution
# (Improves Legend Readability)
# ---------------------------
province_order <- df %>%
  count(Province_Territory, name = "total") %>%
  arrange(desc(total)) %>%
  pull(Province_Territory)

grid_data$Province_Territory <- factor(
  grid_data$Province_Territory,
  levels = province_order
)

# ---------------------------
# Canadian-Inspired Color Palette 🇨🇦
# ---------------------------
province_colors <- c(
  "Alberta" = "#D71920",
  "British Columbia" = "#1B4F72",
  "Manitoba" = "#7D3C98",
  "New Brunswick" = "#A93226",
  "Newfoundland and Labrador" = "#2E86C1",
  "Nova Scotia" = "#566573",
  "Ontario" = "#196F3D",
  "Prince Edward Island" = "#F4D03F",
  "Quebec" = "#922B21",
  "Saskatchewan" = "#CA6F1E",
  "Yukon" = "#117A65",
  "Northwest Territories" = "#34495E"
)

province_colors <- province_colors[
  names(province_colors) %in% unique(grid_data$Province_Territory)
]

# ---------------------------
# Plot
# ---------------------------
ggplot(grid_data, aes(x = col, y = row, fill = Province_Territory)) +
  geom_tile(color = "white", size = 0.3) +
  coord_equal() +
  facet_wrap(~ Period_label, ncol = 3) +
  scale_fill_manual(values = province_colors) +
  labs(
    title = "Provincial Distribution of Newly Commissioned Wind Turbines in Canada (2000–2025)",
    subtitle = "Each panel represents 100% of turbines commissioned within the period; each square ≈1% of installations",
    fill = "Province / Territory",
    caption = "Data Source: Canadian Wind Turbine Database."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    strip.background = element_rect(fill = "#F4F4F4", color = NA),
    strip.text = element_text(face = "bold", size = 12),
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 12),
    legend.position = "right"
  )





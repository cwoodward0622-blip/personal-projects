
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





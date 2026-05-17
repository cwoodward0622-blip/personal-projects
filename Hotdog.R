library(ggplot2)
library(dplyr)
library(readr)

# -----------------------------
# Load and Clean Data
# -----------------------------
data <- read_csv("hot-dog-contest-winners.csv") %>%
  rename(DogsEaten = `Dogs eaten`)

# Order years so most recent appears at top
data$Year <- factor(data$Year, levels = sort(unique(data$Year)))

# -----------------------------
# Create Helper Positions
# -----------------------------
year_levels <- levels(data$Year)

kobayashi_years <- which(year_levels %in% as.character(2001:2006))
chestnut_years  <- which(year_levels %in% as.character(2007:2010))

# Better bracket positioning
bracket_x <- 73
tick_x    <- 71.8
label_x   <- 75

# -----------------------------
# Key Highlight Years
# -----------------------------
highlight_years <- data %>%
  filter(Year %in% c("1980", "2001", "2007", "2009"))

# -----------------------------
# Plot
# -----------------------------
ggplot(data, aes(x = DogsEaten, y = Year)) +
  
  # Bun layer
  geom_col(fill = "#F17C0B", height = 0.55) +
  
  # Mustard highlight
  geom_col(fill = "#FFD23F", height = 0.35) +
  
  # Ketchup stripe
  geom_segment(
    aes(
      x = 0,
      xend = DogsEaten,
      y = as.numeric(Year),
      yend = as.numeric(Year)
    ),
    color = "#C8102E",
    linewidth = 3,
    lineend = "round"
  ) +
  
  # Highlight Key Years
  geom_text(
    data = highlight_years,
    aes(label = DogsEaten),
    nudge_x = 2,
    fontface = "bold",
    size = 4,
    color = "#C8102E"
  ) +
  
  # =============================
# KOBAYASHI BRACKET
# =============================
geom_segment(
  aes(x = bracket_x, xend = bracket_x,
      y = min(kobayashi_years),
      yend = max(kobayashi_years)),
  linewidth = 1.8,
  lineend = "round",
  color = "#444444"
) +
  geom_segment(
    aes(x = bracket_x, xend = tick_x,
        y = min(kobayashi_years),
        yend = min(kobayashi_years)),
    linewidth = 1.8,
    lineend = "round",
    color = "#444444"
  ) +
  geom_segment(
    aes(x = bracket_x, xend = tick_x,
        y = max(kobayashi_years),
        yend = max(kobayashi_years)),
    linewidth = 1.8,
    lineend = "round",
    color = "#444444"
  ) +
  annotate(
    "text",
    x = label_x,
    y = mean(kobayashi_years),
    label = "Takeru Kobayashi (2001–2006)",
    hjust = 0,
    fontface = "bold",
    size = 4.2,
    color = "#444444"
  ) +
  
  # =============================
# CHESTNUT BRACKET
# =============================
geom_segment(
  aes(x = bracket_x, xend = bracket_x,
      y = min(chestnut_years),
      yend = max(chestnut_years)),
  linewidth = 1.8,
  lineend = "round",
  color = "#8B0000"
) +
  geom_segment(
    aes(x = bracket_x, xend = tick_x,
        y = min(chestnut_years),
        yend = min(chestnut_years)),
    linewidth = 1.8,
    lineend = "round",
    color = "#8B0000"
  ) +
  geom_segment(
    aes(x = bracket_x, xend = tick_x,
        y = max(chestnut_years),
        yend = max(chestnut_years)),
    linewidth = 1.8,
    lineend = "round",
    color = "#8B0000"
  ) +
  annotate(
    "text",
    x = label_x,
    y = mean(chestnut_years),
    label = "Joey Chestnut (2007–2010)",
    hjust = 0,
    fontface = "bold",
    size = 4.2,
    color = "#8B0000"
  ) +
  
  # -----------------------------
# Labels and Theme
# -----------------------------
labs(
  title = "The Expanding Limits of Competitive Eating",
  subtitle = "Winning Hot Dog Totals by Year",
  x = "Hot Dogs Eaten",
  y = "Year",
  caption = "Source: Nathan's Famous International Hot Dog Eating Contest"
) +
  
  scale_x_continuous(limits = c(0, 90)) +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 14),
    axis.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(size = 10)
  )



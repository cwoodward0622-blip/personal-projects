


# =========================
# LOAD LIBRARIES
# =========================
library(tidyverse)

# =========================
# LOAD DATA
# =========================
golf_data <- read.csv("golf_stats.csv")

# =========================
# CLEAN DATA
# =========================
golf_data <- golf_data %>%
  mutate(DRIVING_AVG = as.numeric(DRIVING_AVG)) %>%
  arrange(desc(DRIVING_AVG))

# =========================
# SELECT PLAYERS
# =========================
top_players <- golf_data %>% slice_head(n = 5)
bottom_players <- golf_data %>% slice_tail(n = 5)

mid_index <- floor(nrow(golf_data) / 2)
middle_players <- golf_data %>%
  slice((mid_index - 2):(mid_index + 2))

plot_df <- bind_rows(top_players, middle_players, bottom_players)

# =========================
# ADD POSITION + SCALE
# =========================
plot_df <- plot_df %>%
  arrange(desc(DRIVING_AVG)) %>%
  mutate(
    id = row_number(),
    value = DRIVING_AVG - 260
  )

# =========================
# GRID BREAKS (UPDATED)
# =========================
grid_breaks <- c(20, 40, 60, 80)
grid_labels <- c("280", "300", "320", "340")

outer_limit <- max(c(plot_df$value, 80))  # ensures 340 ring shows

# =========================
# BUILD PLOT
# =========================
plt <- ggplot(plot_df, aes(x = factor(id), y = value)) +
  
  # GRIDLINES
  geom_hline(
    data = data.frame(y = grid_breaks),
    aes(yintercept = y),
    color = "grey70",
    linewidth = 0.6,
    inherit.aes = FALSE
  ) +
  
  # BARS
  geom_col(
    aes(fill = DRIVING_AVG),
    alpha = 0.9,
    width = 0.9
  ) +
  
  # FAINT DOTTED LINES
  geom_segment(
    aes(
      x = factor(id),
      xend = factor(id),
      y = 0,
      yend = outer_limit
    ),
    linetype = "dashed",
    color = "black",
    alpha = 0.35,
    linewidth = 0.6
  ) +
  
  # PLAYER LABELS
  geom_text(
    aes(
      x = factor(id),
      y = outer_limit + 12,
      label = PLAYER
    ),
    angle = 0,
    hjust = 0.5,
    size = 3
  ) +
  
  # RING LABELS
  annotate(
    "text",
    x = length(unique(plot_df$id)) + 0.8,
    y = grid_breaks,
    label = grid_labels,
    size = 3
  ) +
  
  coord_polar()

# =========================
# DONUT HOLE
# =========================
plt <- plt +
  annotate(
    "rect",
    xmin = -Inf, xmax = Inf,
    ymin = -Inf, ymax = 0,
    fill = "white"
  )

# =========================
# STYLE
# =========================
plt <- plt +
  scale_fill_gradientn(
    name = "Driving Distance",
    colours = c(
      "#c8e6c9",
      "#81c784",
      "#4caf50",
      "#2e7d32",
      "#1b5e20"
    )
  ) +
  scale_y_continuous(
    limits = c(-15, outer_limit + 20)
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom"
  ) +
  labs(
    title = "Driving Distance: Top vs Middle vs Short Hitters",
    subtitle = "Distances scaled to highlight differences (yards)",
    caption = "Data: PGA Tour"
  )

# =========================
# SHOW
# =========================
plt






# =========================
# LOAD LIBRARIES
# =========================
library(tidyverse)

# =========================
# LOAD DATA
# =========================
golf_data <- read.csv("golf_stats.csv", check.names = FALSE)

# =========================
# CLEAN COLUMN NAMES
# =========================
colnames(golf_data) <- tolower(colnames(golf_data))

# =========================
# CLEAN VARIABLES
# =========================
golf_data <- golf_data %>%
  mutate(
    fairway_pct = as.numeric(gsub("%", "", `fairway_%`)),
    driving_avg = as.numeric(driving_avg)
  )

# =========================
# SELECT YOUR 15 PLAYERS
# =========================
selected_players <- c(
  "Aldrich Potgieter",
  "Gary Woodland",
  "Michael Brennan",
  "Chris Gotterup",
  "Jesper Svensson",
  "Harris English",
  "Chad Ramey",
  "Davis Riley",
  "Ryo Hisatsune",
  "Robert MacIntyre",
  "Lucas Glover",
  "Brandt Snedeker",
  "Matt Kuchar",
  "Andrew Putnam",
  "Brian Campbell"
)

plot_df <- golf_data %>%
  filter(player %in% selected_players)

# =========================
# ORDER BY DRIVING DISTANCE
# =========================
plot_df <- plot_df %>%
  arrange(driving_avg) %>%
  mutate(
    y = seq(-120, 120, length.out = n())
  )

# =========================
# NORMALIZE FAIRWAY %
# =========================
plot_df <- plot_df %>%
  mutate(
    acc = (fairway_pct - min(fairway_pct)) /
      (max(fairway_pct) - min(fairway_pct))
  )

# =========================
# FAIRWAY SHAPE
# =========================
theta <- seq(0, 2*pi, length.out = 300)

fairway <- data.frame(
  x = 35 * cos(theta),
  y = 180 * sin(theta)
)

# =========================
# X POSITION
# =========================
set.seed(1)

plot_df <- plot_df %>%
  mutate(
    x = rnorm(n(), mean = 0, sd = (1 - acc) * 40 + 5)
  )

# =========================
# MANUAL ADJUSTMENTS
# =========================
plot_df <- plot_df %>%
  mutate(
    x = case_when(
      player == "Davis Riley" ~ ifelse(x > 0, x + 40, x - 40),
      player == "Brandt Snedeker" ~ x * 0.4,
      player == "Chad Ramey" ~ ifelse(x > 0, x + 20, x - 20),
      player == "Jesper Svensson" ~ ifelse(x > 0, 33, -33),
      TRUE ~ x
    )
  )

# =========================
# PLOT
# =========================
ggplot() +
  
  # ROUGH
  annotate("rect",
           xmin = -110, xmax = 140,
           ymin = -200, ymax = 200,
           fill = "#dfe8df") +
  
  # FAIRWAY
  geom_polygon(
    data = fairway,
    aes(x = x, y = y),
    fill = "#81c784",
    alpha = 0.6
  ) +
  
  # BACK LAYER (SUBTLE SHADOW)
  geom_point(
    data = plot_df,
    aes(x = x, y = y),
    size = 6,
    color = "black",
    alpha = 0.15
  ) +
  
  # MAIN POINTS WITH THINNER WHITE OUTLINE
  geom_point(
    data = plot_df,
    aes(x = x, y = y, fill = fairway_pct),
    size = 4.5,
    shape = 21,
    color = "white",
    stroke = 0.6   # 🔥 thinner outline
  ) +
  
  # LABELS
  geom_text(
    data = plot_df,
    aes(x = x, y = y, label = player),
    size = 3,
    vjust = -1,
    check_overlap = TRUE
  ) +
  
  # COLOR SCALE
  scale_fill_gradientn(
    name = "Fairway %",
    colours = c("#c8e6c9", "#81c784", "#4caf50", "#1b5e20")
  ) +
  
  # DISTANCE ARROW
  annotate("segment",
           x = 110, xend = 110,
           y = -120, yend = 120,
           arrow = arrow(ends = "both", length = unit(0.2, "cm")),
           color = "grey40", linewidth = 0.6, alpha = 0.5) +
  
  annotate("text",
           x = 118, y = 90,
           label = "Longer hitters",
           angle = 90,
           size = 3,
           color = "grey40") +
  
  annotate("text",
           x = 118, y = -90,
           label = "Shorter hitters",
           angle = 90,
           size = 3,
           color = "grey40") +
  
  coord_fixed(xlim = c(-100, 140), ylim = c(-200, 200)) +
  
  theme_void() +
  
  labs(
    title = "Fairway Accuracy vs Driving Distance",
    subtitle = "Bottom = Shortest hitters | Top = Longest hitters",
    caption = "Data: PGA Tour"
  )





# =========================
# LOAD LIBRARIES
# =========================
library(tidyverse)
library(scales)

# =========================
# LOAD DATA
# =========================
golf_data <- read.csv("golf_stats.csv", check.names = FALSE)
colnames(golf_data) <- tolower(colnames(golf_data))

# =========================
# CLEAN PROXIMITY DATA
# =========================
golf_data <- golf_data %>%
  mutate(
    feet = as.numeric(str_extract(proximity_avg, "^[0-9]+")),
    inches = as.numeric(str_extract(proximity_avg, "(?<=')[ ]*[0-9]+")),
    proximity_ft = feet + (inches / 12)
  ) %>%
  filter(!is.na(proximity_ft))

# =========================
# SCALE TO TARGET RADII
# =========================
golf_data <- golf_data %>%
  mutate(
    prox_scaled = rescale(proximity_ft, to = c(14, 38))
  )

# =========================
# RANDOM SCATTER
# =========================
set.seed(123)

golf_data <- golf_data %>%
  mutate(
    angle = runif(n(), 0, 2*pi),
    x = prox_scaled * cos(angle),
    y = prox_scaled * sin(angle)
  )

# =========================
# MOVE ONLY POINTS NEAR LABELS
# =========================
label_x_positions <- c(12.5, 20.5, 28.5, 36.5)

golf_data <- golf_data %>%
  rowwise() %>%
  mutate(
    too_close = any(abs(x - label_x_positions) < 2 & abs(y) < 3),
    y = ifelse(too_close,
               y + sample(c(-4, 4), 1),
               y)
  ) %>%
  ungroup()

# =========================
# CREATE RINGS WITH DEPTH
# =========================
theta <- seq(0, 2*pi, length.out = 500)

rings <- c(14, 22, 30, 38)

ring_df <- map_dfr(rings, function(r) {
  data.frame(
    x = r * cos(theta),
    y = r * sin(theta),
    r = r
  )
}) %>%
  mutate(
    alpha_level = case_when(
      r == 14 ~ 0.9,
      r == 22 ~ 0.7,
      r == 30 ~ 0.5,
      r == 38 ~ 0.4
    )
  )

# =========================
# LABEL POSITIONS
# =========================
label_df <- data.frame(
  x = c(12.5, 20.5, 28.5, 36.5),
  y = 0,
  label = c("35", "38", "41", "44")
)

# =========================
# PLOT
# =========================
ggplot() +
  
  # RINGS
  geom_path(
    data = ring_df,
    aes(x = x, y = y, group = r, alpha = alpha_level),
    color = "#81c784",
    linewidth = 1
  ) +
  scale_alpha_identity() +
  
  # PLAYER POINTS
  geom_point(
    data = golf_data,
    aes(x = x, y = y),
    color = "#2e7d32",
    size = 2.2,
    alpha = 0.75
  ) +
  
  # CENTER "HOLE"
  geom_point(aes(x = 0, y = 0), size = 5, color = "black", alpha = 0.1) +
  geom_point(aes(x = 0, y = 0), size = 3, color = "black") +
  
  # CROSSHAIR
  geom_segment(
    aes(x = -45, xend = 45, y = 0, yend = 0),
    linetype = "dashed",
    color = "grey75",
    linewidth = 0.5
  ) +
  
  geom_segment(
    aes(x = 0, xend = 0, y = -45, yend = 45),
    linetype = "dashed",
    color = "grey75",
    linewidth = 0.5
  ) +
  
  # RING LABELS
  geom_text(
    data = label_df,
    aes(x = x, y = y, label = label),
    color = "black",
    size = 4
  ) +
  
  # CENTER LABEL
  annotate("text", x = 0, y = 0, label = "0", vjust = 2, size = 4) +
  
  coord_fixed(xlim = c(-45, 45), ylim = c(-45, 45)) +
  
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 12)
  ) +
  
  labs(
    title = "Approach Shot Proximity (PGA Tour)",
    subtitle = "Each dot represents a player's average distance to the hole",
    caption = "Closer to center = better iron play | Data: PGA Tour"
  )





# =========================
# LOAD LIBRARIES
# =========================
library(ggplot2)
library(dplyr)
library(stringr)

# =========================
# LOAD DATA
# =========================
df <- read.csv("golf_stats.csv", check.names = FALSE)

# =========================
# CLEAN SCRAMBLING %
# =========================
df <- df %>%
  mutate(
    scramble_pct = as.numeric(str_remove(`SCRAMBLE_%`, "%"))
  ) %>%
  filter(!is.na(scramble_pct))

# =========================
# CALCULATE TOUR AVERAGE
# =========================
tour_avg <- mean(df$scramble_pct, na.rm = TRUE)
avg_label <- paste0("PGA Tour Average (", round(tour_avg, 1), "%)")

# =========================
# CREATE PLOT
# =========================
ggplot(df, aes(x = scramble_pct)) +
  
  # Density curve
  geom_density(
    fill = "#4CAF50",
    alpha = 0.35,
    color = "#2E7D32",
    size = 1.2
  ) +
  
  # Average line
  geom_vline(
    xintercept = tour_avg,
    linetype = "dashed",
    color = "black",
    size = 1.2
  ) +
  
  # Average label
  annotate(
    "text",
    x = tour_avg + 0.5,
    y = 0.06,
    label = avg_label,
    size = 5,
    fontface = "bold"
  ) +
  
  # Labels
  labs(
    title = "Scrambling Percentage (PGA Tour)",
    subtitle = "Ability to save par or better after missing the green",
    x = "Scrambling (%)",
    y = "Distribution of Players",
    caption = "Higher = better short game | Data: PGA Tour"
  ) +
  
  # Theme
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", size = 22),
    plot.subtitle = element_text(size = 16),
    axis.title = element_text(size = 14),
    panel.grid.major = element_line(color = "grey85"),
    panel.grid.minor = element_blank()
  )




# ---------------------------
# Load libraries
# ---------------------------
library(ggplot2)
library(dplyr)
library(readr)
library(stringr)

# ---------------------------
# LOAD DATA
# ---------------------------
df <- read_csv("golf_stats.csv")

# ---------------------------
# CLEAN THREE PUTT %
# ---------------------------
df <- df %>%
  mutate(
    three_putt_pct = as.numeric(str_remove(`THREE_PUTT_%`, "%"))
  ) %>%
  filter(!is.na(three_putt_pct))

# ---------------------------
# CREATE BINS (LEFT-CLOSED)
# ---------------------------
breaks <- seq(0.5, 6.5, by = 0.5)

labels <- paste0(
  "[",
  format(head(breaks, -1), nsmall = 1),
  "–",
  format(tail(breaks, -1), nsmall = 1),
  ")"
)

df <- df %>%
  mutate(
    bin = cut(
      three_putt_pct,
      breaks = breaks,
      labels = labels,
      include.lowest = TRUE,
      right = FALSE
    )
  )

# ---------------------------
# COUNT PLAYERS PER BIN
# ---------------------------
bin_counts <- df %>%
  count(bin) %>%
  mutate(
    bin = factor(bin, levels = labels)
  )

# ---------------------------
# PLOT (GOLF THEMED)
# ---------------------------
ggplot(bin_counts, aes(x = bin, y = n)) +
  
  # --- Tee stem (thicker, rounded) ---
  geom_segment(
    aes(x = bin, xend = bin, y = 0, yend = n),
    color = "#2E7D32",     # deep golf green
    linewidth = 2,
    lineend = "round"
  ) +
  
  # --- Tee base (bottom nub) ---
  geom_point(
    aes(y = 0),
    size = 2.5,
    color = "#1B5E20"
  ) +
  
  # --- Golf ball (white with outline) ---
  geom_point(
    size = 6,
    shape = 21,                 # allows fill + border
    fill = "white",             # golf ball color
    color = "#333333",          # outline
    stroke = 1.2
  ) +
  
  # --- Labels ---
  labs(
    title = "3-Putt Percentage Distribution (PGA Tour)",
    subtitle = "Number of players within each 0.5% interval",
    x = "3-Putt Percentage Range (%)",
    y = "Number of Players",
    caption = "Data: PGA Tour"
  ) +
  
  # ---------------------------
# THEME (Golf aesthetic)
# ---------------------------
theme_minimal(base_size = 14) +
  theme(
    # softer grid (like fairway stripes)
    panel.grid.major.y = element_line(color = "#E0E0E0"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    
    # background tint
    plot.background = element_rect(fill = "#F4F8F2", color = NA),
    panel.background = element_rect(fill = "#F4F8F2", color = NA),
    
    # text
    axis.text.x = element_text(size = 11),
    plot.title = element_text(face = "bold", size = 22),
    plot.subtitle = element_text(size = 14)
  )









# ---------------------------
# Load libraries
# ---------------------------
library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)

# ---------------------------
# LOAD DATA
# ---------------------------
df <- read_csv("golf_stats.csv")

# ---------------------------
# CALCULATE AVERAGES
# ---------------------------
summary_df <- df %>%
  summarise(
    Approach = mean(APPROACH_SG_AVG, na.rm = TRUE),
    `Off the Tee` = mean(SG_OTT_AVG, na.rm = TRUE),
    `Around Green` = mean(SG_ATG_AVG, na.rm = TRUE),
    Putting = mean(SG_PUTTING_AVG, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(),
               names_to = "Category",
               values_to = "Avg_SG") %>%
  arrange(desc(Avg_SG))

# ---------------------------
# DEFINE CUSTOM COLORS
# ---------------------------
color_map <- c(
  "Putting" = "#1B5E20",        # darkest
  "Around Green" = "#2E7D32",
  "Approach" = "#66BB6A",
  "Off the Tee" = "#A5D6A7"     # lightest
)

# ---------------------------
# PLOT
# ---------------------------
ggplot(summary_df, aes(x = reorder(Category, Avg_SG), y = Avg_SG, fill = Category)) +
  
  # Bars
  geom_col(width = 0.6) +
  
  # Apply manual colors
  scale_fill_manual(values = color_map) +
  
  # Labels
  geom_text(
    aes(label = round(Avg_SG, 3)),
    hjust = ifelse(summary_df$Avg_SG > 0, -0.2, 1.2),
    size = 4,
    fontface = "bold"
  ) +
  
  # Flip
  coord_flip() +
  
  # Axis scaling
  scale_y_continuous(
    limits = c(-0.04, 0.03),
    breaks = seq(-0.04, 0.03, by = 0.01)
  ) +
  
  # Zero line
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  
  # Titles
  labs(
    title = "What Actually Impacts Scoring (PGA Tour)",
    subtitle = "Darker green indicates greater impact on strokes gained",
    x = "",
    y = "Average Strokes Gained (All Players)",
    caption = "Data: PGA Tour"
  ) +
  
  # Theme
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 22),
    plot.subtitle = element_text(size = 14),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )






# -----------------------------
# LOAD LIBRARIES
# -----------------------------
library(dplyr)
library(stringr)

# -----------------------------
# LOAD DATA
# -----------------------------
df <- read.csv("golf_stats.csv", check.names = FALSE, stringsAsFactors = FALSE)

# -----------------------------
# CLEAN + CONVERT PROXIMITY_AVG
# -----------------------------
df_clean <- df %>%
  mutate(
    # Extract feet
    feet = as.numeric(str_extract(PROXIMITY_AVG, "^[0-9]+")),
    
    # Extract inches (if missing, replace with 0)
    inches = as.numeric(str_extract(PROXIMITY_AVG, "(?<=')[0-9]+")),
    inches = ifelse(is.na(inches), 0, inches),
    
    # Convert to total feet
    proximity_ft = feet + (inches / 12)
  )

# -----------------------------
# REMOVE BAD ROWS
# -----------------------------
df_clean <- df_clean %>%
  filter(!is.na(proximity_ft))

# -----------------------------
# CALCULATE AVERAGE
# -----------------------------
avg_proximity <- mean(df_clean$proximity_ft)

# -----------------------------
# PRINT RESULT
# -----------------------------
cat("Average Proximity to Hole:", round(avg_proximity, 2), "feet\n")



# Load necessary libraries
library(dplyr)

# -----------------------------
# LOAD DATA
# -----------------------------
df <- read.csv("golf_stats.csv", check.names = FALSE)

# -----------------------------
# CLEAN DRIVING DISTANCE
# -----------------------------
df <- df %>%
  mutate(
    DRIVING_AVG = as.numeric(DRIVING_AVG)
  ) %>%
  filter(!is.na(DRIVING_AVG))

# -----------------------------
# CALCULATE AVERAGE
# -----------------------------
avg_driving_distance <- mean(df$DRIVING_AVG)

# -----------------------------
# PRINT RESULT
# -----------------------------
print(avg_driving_distance)

# Optional: cleaner output
cat("Average PGA Tour Driving Distance:", round(avg_driving_distance, 1), "yards\n")



# =========================
# LOAD LIBRARIES
# =========================
library(tidyverse)

# =========================
# LOAD DATA
# =========================
golf_data <- read.csv("golf_stats.csv", check.names = FALSE)

# =========================
# CLEAN COLUMN NAMES
# =========================
colnames(golf_data) <- tolower(colnames(golf_data))

# =========================
# CLEAN VARIABLES
# =========================
golf_data <- golf_data %>%
  mutate(
    fairway_pct = as.numeric(gsub("%", "", `fairway_%`)),
    driving_avg = as.numeric(driving_avg)
  ) %>%
  arrange(desc(driving_avg))   # sort longest → shortest

# =========================
# DEFINE GROUPS
# =========================
long_hitters <- golf_data %>% slice_head(n = 5)
short_hitters <- golf_data %>% slice_tail(n = 5)

# =========================
# CALCULATE AVERAGES
# =========================
long_avg <- mean(long_hitters$fairway_pct, na.rm = TRUE)
short_avg <- mean(short_hitters$fairway_pct, na.rm = TRUE)

# =========================
# DIFFERENCE
# =========================
difference <- short_avg - long_avg

# =========================
# PRINT RESULTS
# =========================
cat("Average Fairway % (Long Hitters):", round(long_avg, 2), "%\n")
cat("Average Fairway % (Short Hitters):", round(short_avg, 2), "%\n")
cat("Difference (Short - Long):", round(difference, 2), "%\n")




# =========================
# LOAD LIBRARIES
# =========================
library(dplyr)
library(stringr)

# =========================
# LOAD DATA
# =========================
df <- read.csv("golf_stats.csv", check.names = FALSE)

# =========================
# CLEAN SCRAMBLING %
# =========================
df <- df %>%
  mutate(
    scramble_pct = as.numeric(str_remove(`SCRAMBLE_%`, "%"))
  ) %>%
  filter(!is.na(scramble_pct))

# =========================
# FIND CLUSTER (MIDDLE 75%)
# =========================
q_low <- quantile(df$scramble_pct, 0.125, na.rm = TRUE)
q_high <- quantile(df$scramble_pct, 0.875, na.rm = TRUE)

# =========================
# PRINT RESULT
# =========================
cat("Most players (75%) fall between:",
    round(q_low, 1), "% and", round(q_high, 1), "%\n")


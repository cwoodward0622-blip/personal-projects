# Load libraries
library(ggplot2)
library(readr)
library(dplyr)

# Import data
data <- read_csv("hot-dog-contest-winners.csv")

# Rename for convenience
data <- data %>%
  rename(
    Year = Year,
    DogsEaten = `Dogs eaten`
  )

# Make sure Year is treated as factor for bar ordering
data$Year <- factor(data$Year)

# Create hot dog themed bar chart
ggplot(data, aes(x = DogsEaten, y = Year)) +
  
  # Bun layer (slightly bigger)
  geom_col(fill = "#D9A13A", height = 0.7) +
  
  # Sausage layer (slightly smaller stacked on top)
  geom_col(fill = "#F17C0B", height = 0.5) +
  
  # Mustard stripe
  geom_segment(aes(x = 0,
                   xend = DogsEaten,
                   y = as.numeric(Year),
                   yend = as.numeric(Year)),
               color = "gold",
               linewidth = 2) +
  
  labs(
    title = "The Expanding Limits of Competitive Eating",
    subtitle = "Winning Hot Dog Totals by Year",
    x = "Hot Dogs Eaten",
    y = "Year",
    caption = "Source: Nathan's Famous International Hot Dog Eating Contest"
  ) +
  
  theme_minimal(base_size = 14) +
  
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 14),
    axis.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )


library(ggplot2)
library(readr)
library(dplyr)

# Load data
data <- read_csv("hot-dog-contest-winners.csv") %>%
  rename(DogsEaten = `Dogs eaten`)

# Order years descending (latest on top)
data$Year <- factor(data$Year, levels = rev(sort(unique(data$Year))))

ggplot(data, aes(x = DogsEaten, y = Year)) +
  
  # Bun layer (wider + lighter)
  geom_col(
    fill = "#D9A13A",
    height = 0.55
  ) +
  
  # Sausage layer (slightly smaller)
  geom_col(
    fill = "#F17C0B",
    height = 0.35
  ) +
  
  # Mustard stripe (rounded ends)
  geom_segment(
    aes(
      x = 0,
      xend = DogsEaten,
      y = as.numeric(Year),
      yend = as.numeric(Year)
    ),
    color = "gold",
    linewidth = 3,
    lineend = "round"
  ) +
  
  labs(
    title = "The Expanding Limits of Competitive Eating",
    subtitle = "Winning Hot Dog Totals by Year",
    x = "Hot Dogs Eaten",
    y = "Year",
    caption = "Source: Nathan's Famous International Hot Dog Eating Contest"
  ) +
  
  theme_minimal(base_size = 14) +
  
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 14),
    axis.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )


library(ggplot2)
library(readr)
library(dplyr)

# Load data
data <- read_csv("hot-dog-contest-winners.csv") %>%
  rename(DogsEaten = `Dogs eaten`)

# Order years so latest appears at top
data$Year <- factor(data$Year, levels = sort(unique(data$Year)))

ggplot(data, aes(x = DogsEaten, y = Year)) +
  
  # NEW bun color (more red)
  geom_col(
    fill = "#F17C0B",   # red/orange bun
    height = 0.55
  ) +
  
  # NEW sausage color (yellow)
  geom_col(
    fill = "#FFD23F",   # yellow sausage
    height = 0.35
  ) +
  
  # Mustard stripe
  geom_segment(
    aes(
      x = 0,
      xend = DogsEaten,
      y = as.numeric(Year),
      yend = as.numeric(Year)
    ),
    color = "#B8860B",
    linewidth = 3,
    lineend = "round"
  ) +
  
  labs(
    title = "The Expanding Limits of Competitive Eating",
    subtitle = "Winning Hot Dog Totals by Year",
    x = "Hot Dogs Eaten",
    y = "Year",
    caption = "Source: Nathan's Famous International Hot Dog Eating Contest"
  ) +
  
  theme_minimal(base_size = 14) +
  
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 14),
    axis.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

library(ggplot2)
library(readr)
library(dplyr)

# Load data
data <- read_csv("hot-dog-contest-winners.csv") %>%
  rename(DogsEaten = `Dogs eaten`)

# Order years so latest appears at top
data$Year <- factor(data$Year, levels = sort(unique(data$Year)))

ggplot(data, aes(x = DogsEaten, y = Year)) +
  
  geom_col(
    fill = "#F17C0B",
    height = 0.55
  ) +
  
  geom_col(
    fill = "#FFD23F",
    height = 0.35
  ) +
  
  geom_segment(
    aes(
      x = 0,
      xend = DogsEaten,
      y = as.numeric(Year),
      yend = as.numeric(Year)
    ),
    color = "#B8860B",
    linewidth = 3,
    lineend = "round"
  ) +
  
  labs(
    title = "The Expanding Limits of Competitive Eating",
    subtitle = "Winning Hot Dog Totals by Year",
    x = "Hot Dogs Eaten",
    y = "Year",
    caption = "Source: Nathan's Famous International Hot Dog Eating Contest"
  ) +
  
  scale_x_continuous(limits = c(0, 80)) +   # <-- expanded to 80
  
  theme_minimal(base_size = 14) +
  
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 14),
    axis.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )


library(ggplot2)
library(dplyr)
library(readr)
data <- read_csv("Desktop/DTA 350/hot-dog-contest-winners.csv") %>%
  rename(DogsEaten = `Dogs eaten`)


# Load data
#data <- read_csv("hot-dog-contest-winners.csv") %>%
#  rename(DogsEaten = `Dogs eaten`)

# Order years so latest appears at top
data$Year <- factor(data$Year, levels = sort(unique(data$Year)))

ggplot(data, aes(x = DogsEaten, y = Year)) +
  
  # Base bun layer
  geom_col(
    fill = "#F17C0B",
    height = 0.55
  ) +
  
  # Mustard highlight layer
  geom_col(
    fill = "#FFD23F",
    height = 0.35
  ) +
  
  # Ketchup line (changed from mustard to red)
  geom_segment(
    aes(
      x = 0,
      xend = DogsEaten,
      y = as.numeric(Year),
      yend = as.numeric(Year)
    ),
    color = "#C8102E",   # Ketchup red
    linewidth = 3,
    lineend = "round"
  ) +
  
  labs(
    title = "The Expanding Limits of Competitive Eating",
    subtitle = "Winning Hot Dog Totals by Year",
    x = "Hot Dogs Eaten",
    y = "Year",
    caption = "Source: Nathan's Famous International Hot Dog Eating Contest"
  ) +
  
  scale_x_continuous(limits = c(0, 80)) +
  
  theme_minimal(base_size = 14) +
  
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 14),
    axis.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

library(ggplot2)
library(dplyr)
library(readr)

data <- read_csv("Desktop/DTA 350/hot-dog-contest-winners.csv") %>%
  rename(DogsEaten = `Dogs eaten`)

# Order years so latest appears at top
data$Year <- factor(data$Year, levels = sort(unique(data$Year)))

ggplot(data, aes(x = DogsEaten, y = Year)) +
  
  # Base bun layer
  geom_col(
    fill = "#F17C0B",
    height = 0.55
  ) +
  
  # Mustard highlight
  geom_col(
    fill = "#FFD23F",
    height = 0.35
  ) +
  
  # Ketchup line
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
  
  # -----------------------------
# Joey Chestnut Era (2007–2010)
# -----------------------------
annotate(
  "text",
  x = 72,
  y = which(levels(data$Year) %in% as.character(2007:2010)),
  label = "Joey Chestnut Era",
  hjust = 0,
  size = 4.5,
  fontface = "bold",
  color = "#8B0000"
) +
  
  # -----------------------------
# Takeru Kobayashi Era (2001–2006)
# -----------------------------
annotate(
  "text",
  x = 72,
  y = which(levels(data$Year) %in% as.character(2001:2006)),
  label = "Takeru Kobayashi Era",
  hjust = 0,
  size = 4.5,
  fontface = "bold",
  color = "#1A1A1A"
) +
  
  labs(
    title = "The Expanding Limits of Competitive Eating",
    subtitle = "Winning Hot Dog Totals by Year",
    x = "Hot Dogs Eaten",
    y = "Year",
    caption = "Source: Nathan's Famous International Hot Dog Eating Contest"
  ) +
  
  scale_x_continuous(limits = c(0, 85)) +   # extended slightly for text
  
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 14),
    axis.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )


library(ggplot2)
library(dplyr)
library(readr)

data <- read_csv("Desktop/DTA 350/hot-dog-contest-winners.csv") %>%
  rename(DogsEaten = `Dogs eaten`)

data$Year <- factor(data$Year, levels = sort(unique(data$Year)))

# Helper positions
year_levels <- levels(data$Year)
kobayashi_years <- which(year_levels %in% as.character(2001:2006))
chestnut_years  <- which(year_levels %in% as.character(2007:2010))

ggplot(data, aes(x = DogsEaten, y = Year)) +
  
  geom_col(fill = "#F17C0B", height = 0.55) +
  geom_col(fill = "#FFD23F", height = 0.35) +
  
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
  
  # -----------------------------
# Kobayashi bracket
# -----------------------------
geom_segment(
  aes(x = 75, xend = 75,
      y = min(kobayashi_years),
      yend = max(kobayashi_years)),
  linewidth = 1.2
) +
  geom_segment(
    aes(x = 75, xend = 73.5,
        y = min(kobayashi_years),
        yend = min(kobayashi_years)),
    linewidth = 1.2
  ) +
  geom_segment(
    aes(x = 75, xend = 73.5,
        y = max(kobayashi_years),
        yend = max(kobayashi_years)),
    linewidth = 1.2
  ) +
  annotate(
    "text",
    x = 77,
    y = mean(kobayashi_years),
    label = "Takeru Kobayashi Era",
    hjust = 0,
    fontface = "bold",
    size = 4.5
  ) +
  
  # -----------------------------
# Chestnut bracket
# -----------------------------
geom_segment(
  aes(x = 75, xend = 75,
      y = min(chestnut_years),
      yend = max(chestnut_years)),
  linewidth = 1.2,
  color = "#8B0000"
) +
  geom_segment(
    aes(x = 75, xend = 73.5,
        y = min(chestnut_years),
        yend = min(chestnut_years)),
    linewidth = 1.2,
    color = "#8B0000"
  ) +
  geom_segment(
    aes(x = 75, xend = 73.5,
        y = max(chestnut_years),
        yend = max(chestnut_years)),
    linewidth = 1.2,
    color = "#8B0000"
  ) +
  annotate(
    "text",
    x = 77,
    y = mean(chestnut_years),
    label = "Joey Chestnut Era",
    hjust = 0,
    fontface = "bold",
    size = 4.5,
    color = "#8B0000"
  ) +
  
  labs(
    title = "The Expanding Limits of Competitive Eating",
    subtitle = "Winning Hot Dog Totals by Year",
    x = "Hot Dogs Eaten",
    y = "Year",
    caption = "Source: Nathan's Famous International Hot Dog Eating Contest"
  ) +
  
  scale_x_continuous(limits = c(0, 85)) +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 14),
    axis.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )


library(ggplot2)
library(dplyr)
library(readr)

# -----------------------------
# Load and Clean Data
# -----------------------------
data <- read_csv("Desktop/DTA 350/hot-dog-contest-winners.csv") %>%
  rename(DogsEaten = `Dogs eaten`)

# Order years so most recent appears at top
data$Year <- factor(data$Year, levels = sort(unique(data$Year)))

# -----------------------------
# Create Helper Positions
# -----------------------------
year_levels <- levels(data$Year)

kobayashi_years <- which(year_levels %in% as.character(2001:2006))
chestnut_years  <- which(year_levels %in% as.character(2007:2010))

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
  
  # =============================
# KOBAYASHI BRACKET (2001–2006)
# =============================
geom_segment(
  aes(x = 75, xend = 75,
      y = min(kobayashi_years),
      yend = max(kobayashi_years)),
  linewidth = 1.2
) +
  geom_segment(
    aes(x = 75, xend = 73.5,
        y = min(kobayashi_years),
        yend = min(kobayashi_years)),
    linewidth = 1.2
  ) +
  geom_segment(
    aes(x = 75, xend = 73.5,
        y = max(kobayashi_years),
        yend = max(kobayashi_years)),
    linewidth = 1.2
  ) +
  annotate(
    "text",
    x = 77,
    y = mean(kobayashi_years),
    label = "Takeru Kobayashi (2001–2006)",
    hjust = 0,
    fontface = "bold",
    size = 4.5
  ) +
  
  # =============================
# CHESTNUT BRACKET (2007–2010)
# =============================
geom_segment(
  aes(x = 75, xend = 75,
      y = min(chestnut_years),
      yend = max(chestnut_years)),
  linewidth = 1.2,
  color = "#8B0000"
) +
  geom_segment(
    aes(x = 75, xend = 73.5,
        y = min(chestnut_years),
        yend = min(chestnut_years)),
    linewidth = 1.2,
    color = "#8B0000"
  ) +
  geom_segment(
    aes(x = 75, xend = 73.5,
        y = max(chestnut_years),
        yend = max(chestnut_years)),
    linewidth = 1.2,
    color = "#8B0000"
  ) +
  annotate(
    "text",
    x = 77,
    y = mean(chestnut_years),
    label = "Joey Chestnut (2007–2010)",
    hjust = 0,
    fontface = "bold",
    size = 4.5,
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
  
  scale_x_continuous(limits = c(0, 85)) +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 14),
    axis.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )


library(ggplot2)
library(dplyr)
library(readr)

# -----------------------------
# Load and Clean Data
# -----------------------------
data <- read_csv("Desktop/DTA 350/hot-dog-contest-winners.csv") %>%
  rename(DogsEaten = `Dogs eaten`)

# Order years so most recent appears at top
data$Year <- factor(data$Year, levels = sort(unique(data$Year)))

# -----------------------------
# Create Helper Positions
# -----------------------------
year_levels <- levels(data$Year)

kobayashi_years <- which(year_levels %in% as.character(2001:2006))
chestnut_years  <- which(year_levels %in% as.character(2007:2010))

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
  
  # =============================
# KOBAYASHI BRACKET (2001–2006)
# =============================
geom_segment(
  aes(x = 72, xend = 72,
      y = min(kobayashi_years),
      yend = max(kobayashi_years)),
  linewidth = 1.2
) +
  geom_segment(
    aes(x = 72, xend = 70.5,
        y = min(kobayashi_years),
        yend = min(kobayashi_years)),
    linewidth = 1.2
  ) +
  geom_segment(
    aes(x = 72, xend = 70.5,
        y = max(kobayashi_years),
        yend = max(kobayashi_years)),
    linewidth = 1.2
  ) +
  annotate(
    "text",
    x = 74,
    y = mean(kobayashi_years),
    label = "Takeru Kobayashi (2001–2006)",
    hjust = 0,
    fontface = "bold",
    size = 4.5
  ) +
  
  # =============================
# CHESTNUT BRACKET (2007–2010)
# =============================
geom_segment(
  aes(x = 72, xend = 72,
      y = min(chestnut_years),
      yend = max(chestnut_years)),
  linewidth = 1.2,
  color = "#8B0000"
) +
  geom_segment(
    aes(x = 72, xend = 70.5,
        y = min(chestnut_years),
        yend = min(chestnut_years)),
    linewidth = 1.2,
    color = "#8B0000"
  ) +
  geom_segment(
    aes(x = 72, xend = 70.5,
        y = max(chestnut_years),
        yend = max(chestnut_years)),
    linewidth = 1.2,
    color = "#8B0000"
  ) +
  annotate(
    "text",
    x = 74,
    y = mean(chestnut_years),
    label = "Joey Chestnut (2007–2010)",
    hjust = 0,
    fontface = "bold",
    size = 4.5,
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
  
  # Slightly extended axis so nothing clips
  scale_x_continuous(limits = c(0, 88)) +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 14),
    axis.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )


library(ggplot2)
library(dplyr)
library(readr)

# -----------------------------
# Load and Clean Data
# -----------------------------
data <- read_csv("Desktop/DTA 350/hot-dog-contest-winners.csv") %>%
  rename(DogsEaten = `Dogs eaten`)

# Order years so most recent appears at top
data$Year <- factor(data$Year, levels = sort(unique(data$Year)))

# -----------------------------
# Create Helper Positions
# -----------------------------
year_levels <- levels(data$Year)

kobayashi_years <- which(year_levels %in% as.character(2001:2006))
chestnut_years  <- which(year_levels %in% as.character(2007:2010))

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
  
  # =============================
# Highlight Key Moment Labels
# =============================
geom_text(
  data = highlight_years,
  aes(label = DogsEaten),
  nudge_x = 2,
  fontface = "bold",
  size = 5,
  color = "#C8102E"
) +
  
  # =============================
# KOBAYASHI BRACKET (2001–2006)
# =============================
geom_segment(
  aes(x = 72, xend = 72,
      y = min(kobayashi_years),
      yend = max(kobayashi_years)),
  linewidth = 1.2
) +
  geom_segment(
    aes(x = 72, xend = 70.5,
        y = min(kobayashi_years),
        yend = min(kobayashi_years)),
    linewidth = 1.2
  ) +
  geom_segment(
    aes(x = 72, xend = 70.5,
        y = max(kobayashi_years),
        yend = max(kobayashi_years)),
    linewidth = 1.2
  ) +
  annotate(
    "text",
    x = 74,
    y = mean(kobayashi_years),
    label = "Takeru Kobayashi (2001–2006)",
    hjust = 0,
    fontface = "bold",
    size = 4.5
  ) +
  
  # =============================
# CHESTNUT BRACKET (2007–2010)
# =============================
geom_segment(
  aes(x = 72, xend = 72,
      y = min(chestnut_years),
      yend = max(chestnut_years)),
  linewidth = 1.2,
  color = "#8B0000"
) +
  geom_segment(
    aes(x = 72, xend = 70.5,
        y = min(chestnut_years),
        yend = min(chestnut_years)),
    linewidth = 1.2,
    color = "#8B0000"
  ) +
  geom_segment(
    aes(x = 72, xend = 70.5,
        y = max(chestnut_years),
        yend = max(chestnut_years)),
    linewidth = 1.2,
    color = "#8B0000"
  ) +
  annotate(
    "text",
    x = 74,
    y = mean(chestnut_years),
    label = "Joey Chestnut (2007–2010)",
    hjust = 0,
    fontface = "bold",
    size = 4.5,
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
  
  scale_x_continuous(limits = c(0, 88)) +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 14),
    axis.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

library(ggplot2)
library(dplyr)
library(readr)

# -----------------------------
# Load and Clean Data
# -----------------------------
data <- read_csv("Desktop/DTA 350/hot-dog-contest-winners.csv") %>%
  rename(DogsEaten = `Dogs eaten`)

# Order years so most recent appears at top
data$Year <- factor(data$Year, levels = sort(unique(data$Year)))

# -----------------------------
# Create Helper Positions
# -----------------------------
year_levels <- levels(data$Year)

kobayashi_years <- which(year_levels %in% as.character(2001:2006))
chestnut_years  <- which(year_levels %in% as.character(2007:2010))

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
  
  # =============================
# Highlight Key Moment Labels (SMALLER)
# =============================
geom_text(
  data = highlight_years,
  aes(label = DogsEaten),
  nudge_x = 2,
  fontface = "bold",
  size = 4,              # ⬅️ was 5 — now smaller
  color = "#C8102E"
) +
  
  # =============================
# KOBAYASHI BRACKET (SHIFTED RIGHT)
# =============================
geom_segment(
  aes(x = 76, xend = 76,
      y = min(kobayashi_years),
      yend = max(kobayashi_years)),
  linewidth = 1.2
) +
  geom_segment(
    aes(x = 76, xend = 74.5,
        y = min(kobayashi_years),
        yend = min(kobayashi_years)),
    linewidth = 1.2
  ) +
  geom_segment(
    aes(x = 76, xend = 74.5,
        y = max(kobayashi_years),
        yend = max(kobayashi_years)),
    linewidth = 1.2
  ) +
  annotate(
    "text",
    x = 78,
    y = mean(kobayashi_years),
    label = "Takeru Kobayashi (2001–2006)",
    hjust = 0,
    fontface = "bold",
    size = 4.5
  ) +
  
  # =============================
# CHESTNUT BRACKET (SHIFTED RIGHT)
# =============================
geom_segment(
  aes(x = 76, xend = 76,
      y = min(chestnut_years),
      yend = max(chestnut_years)),
  linewidth = 1.2,
  color = "#8B0000"
) +
  geom_segment(
    aes(x = 76, xend = 74.5,
        y = min(chestnut_years),
        yend = min(chestnut_years)),
    linewidth = 1.2,
    color = "#8B0000"
  ) +
  geom_segment(
    aes(x = 76, xend = 74.5,
        y = max(chestnut_years),
        yend = max(chestnut_years)),
    linewidth = 1.2,
    color = "#8B0000"
  ) +
  annotate(
    "text",
    x = 78,
    y = mean(chestnut_years),
    label = "Joey Chestnut (2007–2010)",
    hjust = 0,
    fontface = "bold",
    size = 4.5,
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
    panel.grid.minor = element_blank()
  )


library(ggplot2)
library(dplyr)
library(readr)

# -----------------------------
# Load and Clean Data
# -----------------------------
data <- read_csv("Desktop/DTA 350/hot-dog-contest-winners.csv") %>%
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








library(ggplot2)
library(dplyr)
library(readr)

# -----------------------------
# Load and Clean Data
# -----------------------------
data <- read_csv("Desktop/DTA 350/hot-dog-contest-winners.csv") %>%
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



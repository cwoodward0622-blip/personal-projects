# ---------------------------
# Load Libraries
# ---------------------------
library(tidyverse)

# ---------------------------
# Load Data
# ---------------------------
nobel_peace <- read_csv("nobel_peace.csv")

# Extract unique names
names <- nobel_peace %>%
  distinct(name) %>%
  pull(name)

n <- length(names)
mid <- floor(n / 2)

left_names  <- names[1:mid]
right_names <- names[(mid + 1):n]

# ---------------------------
# Create Curved Book Shape
# ---------------------------

# Left page
left_df <- tibble(
  name = left_names,
  index = 1:length(left_names)
) %>%
  mutate(
    y = 1 - (index / max(index)),
    curve = sqrt(pmax(0, 1 - (y - 0.5)^2 * 4)),
    x = - (0.6 - 0.45 * curve),
    hjust = 1
  )

# Right page
right_df <- tibble(
  name = right_names,
  index = 1:length(right_names)
) %>%
  mutate(
    y = 1 - (index / max(index)),
    curve = sqrt(pmax(0, 1 - (y - 0.5)^2 * 4)),
    x = (0.6 - 0.45 * curve),
    hjust = 0
  )

book_df <- bind_rows(left_df, right_df)

# ---------------------------
# Plot
# ---------------------------

ggplot(book_df, aes(x = x, y = y, label = name, hjust = hjust)) +
  geom_text(size = 3) +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = 1),
               inherit.aes = FALSE) +  # Spine
  coord_cartesian(xlim = c(-0.9, 0.9), ylim = c(0, 1)) +
  theme_void() +
  labs(title = "Nobel Peace Prize Winners",
       subtitle = "Book-Shaped Visualization")

# ---------------------------
# Load Libraries
# ---------------------------
library(tidyverse)

# ---------------------------
# Load Data
# ---------------------------
nobel_peace <- read_csv("nobel_peace.csv")

# Extract unique names
names <- nobel_peace %>%
  distinct(name) %>%
  arrange(name) %>%
  pull(name)

n <- length(names)
mid <- floor(n / 2)

left_names  <- names[1:mid]
right_names <- names[(mid + 1):n]

# ---------------------------
# Create Curved Book Text Layout
# ---------------------------

# Left page
left_df <- tibble(
  name = left_names,
  index = 1:length(left_names)
) %>%
  mutate(
    y = 1 - (index / max(index)),
    curve = sqrt(pmax(0, 1 - (y - 0.5)^2 * 4)),
    x = - (0.55 - 0.35 * curve),  # tighter margin
    hjust = 1
  )

# Right page
right_df <- tibble(
  name = right_names,
  index = 1:length(right_names)
) %>%
  mutate(
    y = 1 - (index / max(index)),
    curve = sqrt(pmax(0, 1 - (y - 0.5)^2 * 4)),
    x = (0.55 - 0.35 * curve),   # tighter margin
    hjust = 0
  )

book_df <- bind_rows(left_df, right_df)

# ---------------------------
# Create Curved Page Edges
# ---------------------------

page_curve <- tibble(
  y = seq(0, 1, length.out = 300)
) %>%
  mutate(
    left_edge  = -0.85 + 0.05 * sin(pi * y),
    right_edge =  0.85 - 0.05 * sin(pi * y)
  )

# ---------------------------
# Plot
# ---------------------------

ggplot() +
  
  # Page background (left)
  annotate("rect",
           xmin = -0.85, xmax = -0.02,
           ymin = 0, ymax = 1,
           fill = "#f8f4e8",
           color = "gray40") +
  
  # Page background (right)
  annotate("rect",
           xmin = 0.02, xmax = 0.85,
           ymin = 0, ymax = 1,
           fill = "#f8f4e8",
           color = "gray40") +
  
  # Subtle spine shadow
  annotate("rect",
           xmin = -0.02, xmax = 0.02,
           ymin = 0, ymax = 1,
           fill = "gray70",
           alpha = 0.5) +
  
  # Curved page edges
  geom_line(data = page_curve,
            aes(x = left_edge, y = y),
            color = "gray35",
            linewidth = 0.8) +
  
  geom_line(data = page_curve,
            aes(x = right_edge, y = y),
            color = "gray35",
            linewidth = 0.8) +
  
  # Spine line
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = 1),
               linewidth = 1.4) +
  
  # Text
  geom_text(data = book_df,
            aes(x = x, y = y, label = name, hjust = hjust),
            size = 3,
            family = "serif") +
  
  coord_cartesian(xlim = c(-0.95, 0.95),
                  ylim = c(0, 1),
                  clip = "off") +
  
  theme_void() +
  
  theme(
    plot.background = element_rect(fill = "#e5dfd0", color = NA),
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    plot.margin = margin(20, 20, 20, 20)
  ) +
  
  labs(
    title = "Nobel Peace Prize Winners",
    subtitle = "A Book-Shaped Visualization"
  )


# ---------------------------
# Load Libraries
# ---------------------------
library(tidyverse)

# ---------------------------
# Load & Prepare Data
# ---------------------------
nobel_peace <- read_csv("nobel_peace.csv")

# Sort by year for narrative flow
nobel_sorted <- nobel_peace %>%
  distinct(year, name) %>%
  arrange(year)

names <- nobel_sorted$name

n <- length(names)
mid <- floor(n / 2)

left_names  <- names[1:mid]
right_names <- names[(mid + 1):n]

# ---------------------------
# Create Curved Text Layout
# ---------------------------

create_page <- function(name_vector, side = "left") {
  
  df <- tibble(
    name = name_vector,
    index = 1:length(name_vector)
  ) %>%
    mutate(
      y = 1 - (index / max(index)),
      curve = sqrt(pmax(0, 1 - (y - 0.5)^2 * 4))
    )
  
  if (side == "left") {
    df <- df %>%
      mutate(
        x = - (0.65 - 0.45 * curve),
        hjust = 1
      )
  } else {
    df <- df %>%
      mutate(
        x = (0.65 - 0.45 * curve),
        hjust = 0
      )
  }
  
  df
}

left_df  <- create_page(left_names, "left")
right_df <- create_page(right_names, "right")

book_df <- bind_rows(left_df, right_df)

# ---------------------------
# Curved Page Outer Edges
# ---------------------------

page_curve <- tibble(
  y = seq(0, 1, length.out = 400)
) %>%
  mutate(
    left_edge  = -0.9 + 0.08 * sin(pi * y),
    right_edge =  0.9 - 0.08 * sin(pi * y)
  )

# ---------------------------
# Plot
# ---------------------------

ggplot() +
  
  # Drop shadow (offset slightly)
  annotate("rect",
           xmin = -0.93, xmax = 0.93,
           ymin = -0.04, ymax = 1.02,
           fill = "gray30", alpha = 0.15) +
  
  # Hard cover background
  annotate("rect",
           xmin = -0.95, xmax = 0.95,
           ymin = -0.05, ymax = 1.05,
           fill = "#5c4a3d") +
  
  # Left page
  annotate("rect",
           xmin = -0.9, xmax = -0.02,
           ymin = 0, ymax = 1,
           fill = "#f8f4e8",
           color = "#3a3a3a") +
  
  # Right page
  annotate("rect",
           xmin = 0.02, xmax = 0.9,
           ymin = 0, ymax = 1,
           fill = "#f8f4e8",
           color = "#3a3a3a") +
  
  # Spine shadow
  annotate("rect",
           xmin = -0.02, xmax = 0.02,
           ymin = 0, ymax = 1,
           fill = "#3a3a3a") +
  
  # Outer curved edges
  geom_line(data = page_curve,
            aes(x = left_edge, y = y),
            color = "#3a3a3a",
            linewidth = 1) +
  
  geom_line(data = page_curve,
            aes(x = right_edge, y = y),
            color = "#3a3a3a",
            linewidth = 1) +
  
  # Winner names
  geom_text(data = book_df,
            aes(x = x, y = y, label = name, hjust = hjust),
            size = 2.7,
            family = "serif",
            color = "#222222") +
  
  # Page numbers
  annotate("text", x = -0.45, y = -0.02,
           label = "1", family = "serif", size = 5, color = "white") +
  
  annotate("text", x = 0.45, y = -0.02,
           label = "2", family = "serif", size = 5, color = "white") +
  
  coord_cartesian(xlim = c(-1, 1),
                  ylim = c(-0.05, 1.08),
                  clip = "off") +
  
  theme_void() +
  
  theme(
    plot.background = element_rect(fill = "#c9c1ad", color = NA),
    plot.title = element_text(size = 28, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 16, hjust = 0.5),
    plot.margin = margin(30, 30, 30, 30)
  ) +
  
  labs(
    title = "Nobel Peace Prize Winners",
    subtitle = "A Chronological Book of Laureates"
  )


# ---------------------------
# Load Libraries
# ---------------------------
library(tidyverse)

# ---------------------------
# Load & Prepare Data
# ---------------------------
nobel_peace <- read_csv("nobel_peace.csv")

nobel_sorted <- nobel_peace %>%
  distinct(year, name) %>%
  arrange(year)

names <- nobel_sorted$name

n <- length(names)
mid <- floor(n / 2)

left_names  <- names[1:mid]
right_names <- names[(mid + 1):n]

# ---------------------------
# Create Curved Text Layout
# (Adjusted vertical spacing to prevent clipping)
# ---------------------------

create_page <- function(name_vector, side = "left") {
  
  total <- length(name_vector)
  
  df <- tibble(
    name = name_vector,
    index = 1:total
  ) %>%
    mutate(
      # Add vertical buffer space
      y = 0.96 - (index - 1) * (0.90 / (total - 1)),
      curve = sqrt(pmax(0, 1 - ((y - 0.5) / 0.46)^2))
    )
  
  if (side == "left") {
    df <- df %>%
      mutate(
        x = - (0.68 - 0.48 * curve),
        hjust = 1
      )
  } else {
    df <- df %>%
      mutate(
        x = (0.68 - 0.48 * curve),
        hjust = 0
      )
  }
  
  df
}

left_df  <- create_page(left_names, "left")
right_df <- create_page(right_names, "right")

book_df <- bind_rows(left_df, right_df)

# ---------------------------
# Curved Page Outer Edges
# ---------------------------

page_curve <- tibble(
  y = seq(0, 1, length.out = 400)
) %>%
  mutate(
    left_edge  = -0.9 + 0.08 * sin(pi * y),
    right_edge =  0.9 - 0.08 * sin(pi * y)
  )

# ---------------------------
# Plot
# ---------------------------

ggplot() +
  
  # Drop shadow
  annotate("rect",
           xmin = -0.93, xmax = 0.93,
           ymin = -0.04, ymax = 1.02,
           fill = "black", alpha = 0.12) +
  
  # Hard cover
  annotate("rect",
           xmin = -0.95, xmax = 0.95,
           ymin = -0.05, ymax = 1.05,
           fill = "#5c4a3d") +
  
  # Stacked page illusion (LEFT side)
  annotate("rect",
           xmin = -0.91, xmax = -0.03,
           ymin = 0.01, ymax = 0.99,
           fill = "#efe9db",
           color = NA, alpha = 0.7) +
  
  annotate("rect",
           xmin = -0.92, xmax = -0.04,
           ymin = 0.02, ymax = 1.00,
           fill = "#f3ede0",
           color = NA, alpha = 0.7) +
  
  # Stacked page illusion (RIGHT side)
  annotate("rect",
           xmin = 0.03, xmax = 0.91,
           ymin = 0.01, ymax = 0.99,
           fill = "#efe9db",
           color = NA, alpha = 0.7) +
  
  annotate("rect",
           xmin = 0.04, xmax = 0.92,
           ymin = 0.02, ymax = 1.00,
           fill = "#f3ede0",
           color = NA, alpha = 0.7) +
  
  # Left page (top page)
  annotate("rect",
           xmin = -0.9, xmax = -0.02,
           ymin = 0.04, ymax = 0.96,
           fill = "#f8f4e8",
           color = "#3a3a3a") +
  
  # Right page (top page)
  annotate("rect",
           xmin = 0.02, xmax = 0.9,
           ymin = 0.04, ymax = 0.96,
           fill = "#f8f4e8",
           color = "#3a3a3a") +
  
  # Spine
  annotate("rect",
           xmin = -0.02, xmax = 0.02,
           ymin = 0.04, ymax = 0.96,
           fill = "#3a3a3a") +
  
  # Curved edges
  geom_line(data = page_curve,
            aes(x = left_edge, y = y),
            color = "#3a3a3a",
            linewidth = 1) +
  
  geom_line(data = page_curve,
            aes(x = right_edge, y = y),
            color = "#3a3a3a",
            linewidth = 1) +
  
  # Text
  geom_text(data = book_df,
            aes(x = x, y = y, label = name, hjust = hjust),
            size = 2.6,
            family = "serif",
            color = "#222222") +
  
  coord_cartesian(xlim = c(-1, 1),
                  ylim = c(-0.05, 1.08),
                  clip = "off") +
  
  theme_void() +
  
  theme(
    plot.background = element_rect(fill = "#c9c1ad", color = NA),
    plot.title = element_text(size = 28, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 16, hjust = 0.5),
    plot.margin = margin(30, 30, 30, 30)
  ) +
  
  labs(
    title = "Nobel Peace Prize Winners",
    subtitle = "A Chronological Book of Laureates"
  )


# ---------------------------
# Load Libraries
# ---------------------------
library(tidyverse)

# ---------------------------
# Load & Sort Data
# ---------------------------
nobel_peace <- read_csv("nobel_peace.csv")

nobel_sorted <- nobel_peace %>%
  distinct(year, name) %>%
  arrange(year)

names <- nobel_sorted$name

n <- length(names)
mid <- floor(n / 2)

left_names  <- names[1:mid]
right_names <- names[(mid + 1):n]

# ---------------------------
# Improved Page Layout
# ---------------------------

create_page <- function(name_vector, side = "left") {
  
  total <- length(name_vector)
  
  # Define page interior bounds
  top    <- 0.92
  bottom <- 0.08
  
  df <- tibble(
    name = name_vector,
    index = 1:total
  ) %>%
    mutate(
      y = seq(top, bottom, length.out = total),
      curve_strength = 0.15,   # subtle curvature
      
      ifelse(side == "left",
             x <- -0.75 + curve_strength * (y - 0.5)^2,
             x <-  0.75 - curve_strength * (y - 0.5)^2),
      
      hjust = ifelse(side == "left", 1, 0)
    )
  
  if (side == "left") {
    df <- df %>%
      mutate(x = -0.75 + 0.12 * ((y - 0.5)^2))
  } else {
    df <- df %>%
      mutate(x = 0.75 - 0.12 * ((y - 0.5)^2))
  }
  
  df
}

left_df  <- create_page(left_names, "left")
right_df <- create_page(right_names, "right")

book_df <- bind_rows(left_df, right_df)

# ---------------------------
# Plot
# ---------------------------

ggplot() +
  
  # Drop shadow
  annotate("rect",
           xmin = -0.93, xmax = 0.93,
           ymin = 0.02, ymax = 0.98,
           fill = "black", alpha = 0.10) +
  
  # Hard cover
  annotate("rect",
           xmin = -0.96, xmax = 0.96,
           ymin = 0, ymax = 1,
           fill = "#5a4636") +
  
  # Back stacked pages
  annotate("rect",
           xmin = -0.86, xmax = -0.04,
           ymin = 0.06, ymax = 0.94,
           fill = "#efe9db", alpha = 0.9) +
  
  annotate("rect",
           xmin = 0.04, xmax = 0.86,
           ymin = 0.06, ymax = 0.94,
           fill = "#efe9db", alpha = 0.9) +
  
  # Main pages
  annotate("rect",
           xmin = -0.82, xmax = -0.02,
           ymin = 0.08, ymax = 0.92,
           fill = "#f8f4e8",
           color = "#3e3e3e") +
  
  annotate("rect",
           xmin = 0.02, xmax = 0.82,
           ymin = 0.08, ymax = 0.92,
           fill = "#f8f4e8",
           color = "#3e3e3e") +
  
  # Spine
  annotate("rect",
           xmin = -0.02, xmax = 0.02,
           ymin = 0.08, ymax = 0.92,
           fill = "#3e3e3e") +
  
  # Subtle center fold shading
  annotate("rect",
           xmin = -0.10, xmax = 0.10,
           ymin = 0.08, ymax = 0.92,
           fill = "black", alpha = 0.05) +
  
  # Text
  geom_text(data = book_df,
            aes(x = x, y = y, label = name, hjust = hjust),
            size = 2.5,
            family = "serif",
            color = "#1f1f1f") +
  
  coord_cartesian(xlim = c(-1, 1),
                  ylim = c(0, 1),
                  clip = "off") +
  
  theme_void() +
  
  theme(
    plot.background = element_rect(fill = "#c9c1ad", color = NA),
    plot.title = element_text(size = 28, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 16, hjust = 0.5),
    plot.margin = margin(30, 30, 30, 30)
  ) +
  
  labs(
    title = "Nobel Peace Prize Winners",
    subtitle = "A Chronological Book of Laureates"
  )


# ---------------------------
# Load Libraries
# ---------------------------
library(tidyverse)

# ---------------------------
# Load & Sort Data
# ---------------------------
nobel_peace <- read_csv("nobel_peace.csv")

nobel_sorted <- nobel_peace %>%
  distinct(year, name) %>%
  arrange(year)

names <- nobel_sorted$name

n <- length(names)
mid <- floor(n / 2)

left_names  <- names[1:mid]
right_names <- names[(mid + 1):n]

# ---------------------------
# Curved Text Layout (Balanced & Contained)
# ---------------------------

create_page <- function(name_vector, side = "left") {
  
  total <- length(name_vector)
  
  # Strict vertical boundaries inside page
  top    <- 0.90
  bottom <- 0.10
  
  df <- tibble(
    name = name_vector,
    index = 1:total
  ) %>%
    mutate(
      y = seq(top, bottom, length.out = total),
      
      # Smooth centered curvature
      curve = sqrt(pmax(0, 1 - ((y - 0.5) / 0.45)^2)),
      
      hjust = ifelse(side == "left", 1, 0)
    )
  
  if (side == "left") {
    df <- df %>%
      mutate(x = -0.70 + 0.35 * curve)
  } else {
    df <- df %>%
      mutate(x = 0.70 - 0.35 * curve)
  }
  
  df
}

left_df  <- create_page(left_names, "left")
right_df <- create_page(right_names, "right")

book_df <- bind_rows(left_df, right_df)

# ---------------------------
# Plot
# ---------------------------

ggplot() +
  
  # Drop shadow
  annotate("rect",
           xmin = -0.94, xmax = 0.94,
           ymin = 0.03, ymax = 0.97,
           fill = "black", alpha = 0.12) +
  
  # Hard cover
  annotate("rect",
           xmin = -0.96, xmax = 0.96,
           ymin = 0, ymax = 1,
           fill = "#5a4636") +
  
  # Back stacked pages
  annotate("rect",
           xmin = -0.86, xmax = -0.04,
           ymin = 0.07, ymax = 0.93,
           fill = "#efe9db") +
  
  annotate("rect",
           xmin = 0.04, xmax = 0.86,
           ymin = 0.07, ymax = 0.93,
           fill = "#efe9db") +
  
  # Main pages
  annotate("rect",
           xmin = -0.82, xmax = -0.02,
           ymin = 0.08, ymax = 0.92,
           fill = "#f8f4e8",
           color = "#3e3e3e") +
  
  annotate("rect",
           xmin = 0.02, xmax = 0.82,
           ymin = 0.08, ymax = 0.92,
           fill = "#f8f4e8",
           color = "#3e3e3e") +
  
  # Spine
  annotate("rect",
           xmin = -0.02, xmax = 0.02,
           ymin = 0.08, ymax = 0.92,
           fill = "#3e3e3e") +
  
  # Subtle center fold shading
  annotate("rect",
           xmin = -0.08, xmax = 0.08,
           ymin = 0.08, ymax = 0.92,
           fill = "black", alpha = 0.05) +
  
  # Names (curved layout restored)
  geom_text(data = book_df,
            aes(x = x, y = y, label = name, hjust = hjust),
            size = 2.6,
            family = "serif",
            color = "#1f1f1f") +
  
  coord_cartesian(xlim = c(-1, 1),
                  ylim = c(0, 1),
                  clip = "off") +
  
  theme_void() +
  
  theme(
    plot.background = element_rect(fill = "#c9c1ad", color = NA),
    plot.title = element_text(size = 28, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 16, hjust = 0.5),
    plot.margin = margin(30, 30, 30, 30)
  ) +
  
  labs(
    title = "Nobel Peace Prize Winners",
    subtitle = "A Chronological Book of Laureates"
  )


# ---------------------------
# Load Libraries
# ---------------------------
library(tidyverse)

# ---------------------------
# Load & Sort Data
# ---------------------------
nobel_peace <- read_csv("nobel_peace.csv")

nobel_sorted <- nobel_peace %>%
  distinct(year, name) %>%
  arrange(year)

names <- nobel_sorted$name

n <- length(names)
mid <- floor(n / 2)

left_names  <- names[1:mid]
right_names <- names[(mid + 1):n]

# ---------------------------
# Curved Text Layout
# ---------------------------

create_page <- function(name_vector, side = "left") {
  
  total <- length(name_vector)
  
  # Strict page interior bounds
  top    <- 0.90
  bottom <- 0.10
  
  df <- tibble(
    name = name_vector,
    index = 1:total
  ) %>%
    mutate(
      y = seq(top, bottom, length.out = total),
      curve = sqrt(pmax(0, 1 - ((y - 0.5) / 0.45)^2)),
      hjust = ifelse(side == "left", 1, 0)
    )
  
  if (side == "left") {
    df <- df %>%
      mutate(x = -0.70 + 0.35 * curve)
  } else {
    df <- df %>%
      mutate(x = 0.70 - 0.35 * curve)
  }
  
  df
}

left_df  <- create_page(left_names, "left")
right_df <- create_page(right_names, "right")

book_df <- bind_rows(left_df, right_df)

# ---------------------------
# Plot
# ---------------------------

ggplot() +
  
  # Drop shadow
  annotate("rect",
           xmin = -0.94, xmax = 0.94,
           ymin = 0.03, ymax = 0.97,
           fill = "black", alpha = 0.12) +
  
  # Hard cover background
  annotate("rect",
           xmin = -0.96, xmax = 0.96,
           ymin = 0, ymax = 1,
           fill = "#5a4636") +
  
  # Back stacked pages
  annotate("rect",
           xmin = -0.86, xmax = -0.04,
           ymin = 0.07, ymax = 0.93,
           fill = "#efe9db") +
  
  annotate("rect",
           xmin = 0.04, xmax = 0.86,
           ymin = 0.07, ymax = 0.93,
           fill = "#efe9db") +
  
  # Main pages
  annotate("rect",
           xmin = -0.82, xmax = -0.02,
           ymin = 0.08, ymax = 0.92,
           fill = "#f8f4e8",
           color = "#3e3e3e") +
  
  annotate("rect",
           xmin = 0.02, xmax = 0.82,
           ymin = 0.08, ymax = 0.92,
           fill = "#f8f4e8",
           color = "#3e3e3e") +
  
  # Spine
  annotate("rect",
           xmin = -0.02, xmax = 0.02,
           ymin = 0.08, ymax = 0.92,
           fill = "#3e3e3e") +
  
  # Subtle center fold shading
  annotate("rect",
           xmin = -0.08, xmax = 0.08,
           ymin = 0.08, ymax = 0.92,
           fill = "black", alpha = 0.05) +
  
  # Winner Names (Curved Layout)
  geom_text(data = book_df,
            aes(x = x, y = y, label = name, hjust = hjust),
            size = 2.6,
            family = "serif",
            color = "#1f1f1f") +
  
  coord_cartesian(xlim = c(-1, 1),
                  ylim = c(0, 1),
                  clip = "off") +
  
  theme_void() +
  
  theme(
    plot.background = element_rect(fill = "#c9c1ad", color = NA),
    plot.title = element_text(size = 28, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 16, hjust = 0.5, lineheight = 1.4),
    plot.margin = margin(30, 30, 30, 30)
  ) +
  
  labs(
    title = "Nobel Peace Prize Winners",
    subtitle = "A Chronological Book of Laureates\n1901–2025"
  )


# ---------------------------
# Load Libraries
# ---------------------------
library(tidyverse)

# ---------------------------
# Load & Sort Data
# ---------------------------
nobel_peace <- read_csv("nobel_peace.csv")

nobel_sorted <- nobel_peace %>%
  distinct(year, name) %>%
  arrange(year)

names <- nobel_sorted$name

n <- length(names)
mid <- floor(n / 2)

left_names  <- names[1:mid]
right_names <- names[(mid + 1):n]

# ---------------------------
# Curved Text Layout (Expanded)
# ---------------------------

create_page <- function(name_vector, side = "left") {
  
  total <- length(name_vector)
  
  # Use more of the page height
  top    <- 0.94
  bottom <- 0.06
  
  df <- tibble(
    name = name_vector,
    index = 1:total
  ) %>%
    mutate(
      y = seq(top, bottom, length.out = total),
      
      # Slightly wider curvature
      curve = sqrt(pmax(0, 1 - ((y - 0.5) / 0.48)^2)),
      hjust = ifelse(side == "left", 1, 0)
    )
  
  if (side == "left") {
    df <- df %>%
      mutate(x = -0.73 + 0.40 * curve)
  } else {
    df <- df %>%
      mutate(x = 0.73 - 0.40 * curve)
  }
  
  df
}

left_df  <- create_page(left_names, "left")
right_df <- create_page(right_names, "right")

book_df <- bind_rows(left_df, right_df)

# ---------------------------
# Plot
# ---------------------------

ggplot() +
  
  # Drop shadow
  annotate("rect",
           xmin = -0.94, xmax = 0.94,
           ymin = 0.03, ymax = 0.97,
           fill = "black", alpha = 0.12) +
  
  # Hard cover
  annotate("rect",
           xmin = -0.96, xmax = 0.96,
           ymin = 0, ymax = 1,
           fill = "#5a4636") +
  
  # Back stacked pages
  annotate("rect",
           xmin = -0.86, xmax = -0.04,
           ymin = 0.07, ymax = 0.93,
           fill = "#efe9db") +
  
  annotate("rect",
           xmin = 0.04, xmax = 0.86,
           ymin = 0.07, ymax = 0.93,
           fill = "#efe9db") +
  
  # Main pages
  annotate("rect",
           xmin = -0.82, xmax = -0.02,
           ymin = 0.06, ymax = 0.94,
           fill = "#f8f4e8",
           color = "#3e3e3e") +
  
  annotate("rect",
           xmin = 0.02, xmax = 0.82,
           ymin = 0.06, ymax = 0.94,
           fill = "#f8f4e8",
           color = "#3e3e3e") +
  
  # Spine
  annotate("rect",
           xmin = -0.02, xmax = 0.02,
           ymin = 0.06, ymax = 0.94,
           fill = "#3e3e3e") +
  
  # Subtle fold shading
  annotate("rect",
           xmin = -0.08, xmax = 0.08,
           ymin = 0.06, ymax = 0.94,
           fill = "black", alpha = 0.05) +
  
  # Names
  geom_text(data = book_df,
            aes(x = x, y = y, label = name, hjust = hjust),
            size = 2.6,
            family = "serif",
            color = "#1f1f1f") +
  
  coord_cartesian(xlim = c(-1, 1),
                  ylim = c(0, 1),
                  clip = "off") +
  
  theme_void() +
  
  theme(
    plot.background = element_rect(fill = "#c9c1ad", color = NA),
    plot.title = element_text(size = 28, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 16, hjust = 0.5, lineheight = 1.4),
    plot.margin = margin(30, 30, 30, 30)
  ) +
  
  labs(
    title = "Nobel Peace Prize Winners",
    subtitle = "A Chronological Book of Laureates\n1901–2025"
  )


# ---------------------------
# Load Libraries
# ---------------------------
library(tidyverse)

# ---------------------------
# Load & Sort Data
# ---------------------------
nobel_peace <- read_csv("nobel_peace.csv")

nobel_sorted <- nobel_peace %>%
  distinct(year, name) %>%
  arrange(year)

names <- nobel_sorted$name

n <- length(names)
mid <- floor(n / 2)

left_names  <- names[1:mid]
right_names <- names[(mid + 1):n]

# ---------------------------
# Create Straight Layout
# ---------------------------

create_page <- function(name_vector, side = "left") {
  
  total <- length(name_vector)
  
  # Fill most of the page vertically
  top    <- 0.94
  bottom <- 0.06
  
  df <- tibble(
    name = name_vector,
    y = seq(top, bottom, length.out = total)
  )
  
  if (side == "left") {
    df <- df %>%
      mutate(
        x = -0.72,
        hjust = 1
      )
  } else {
    df <- df %>%
      mutate(
        x = 0.72,
        hjust = 0
      )
  }
  
  df
}

left_df  <- create_page(left_names, "left")
right_df <- create_page(right_names, "right")

book_df <- bind_rows(left_df, right_df)

# ---------------------------
# Plot
# ---------------------------

ggplot() +
  
  # Drop shadow
  annotate("rect",
           xmin = -0.94, xmax = 0.94,
           ymin = 0.03, ymax = 0.97,
           fill = "black", alpha = 0.12) +
  
  # Hard cover
  annotate("rect",
           xmin = -0.96, xmax = 0.96,
           ymin = 0, ymax = 1,
           fill = "#5a4636") +
  
  # Back stacked pages
  annotate("rect",
           xmin = -0.86, xmax = -0.04,
           ymin = 0.07, ymax = 0.93,
           fill = "#efe9db") +
  
  annotate("rect",
           xmin = 0.04, xmax = 0.86,
           ymin = 0.07, ymax = 0.93,
           fill = "#efe9db") +
  
  # Main pages
  annotate("rect",
           xmin = -0.82, xmax = -0.02,
           ymin = 0.06, ymax = 0.94,
           fill = "#f8f4e8",
           color = "#3e3e3e") +
  
  annotate("rect",
           xmin = 0.02, xmax = 0.82,
           ymin = 0.06, ymax = 0.94,
           fill = "#f8f4e8",
           color = "#3e3e3e") +
  
  # Spine
  annotate("rect",
           xmin = -0.02, xmax = 0.02,
           ymin = 0.06, ymax = 0.94,
           fill = "#3e3e3e") +
  
  # Subtle fold shading
  annotate("rect",
           xmin = -0.08, xmax = 0.08,
           ymin = 0.06, ymax = 0.94,
           fill = "black", alpha = 0.05) +
  
  # Names (Straight & Evenly Spread)
  geom_text(data = book_df,
            aes(x = x, y = y, label = name, hjust = hjust),
            size = 2.6,
            family = "serif",
            color = "#1f1f1f") +
  
  coord_cartesian(xlim = c(-1, 1),
                  ylim = c(0, 1),
                  clip = "off") +
  
  theme_void() +
  
  theme(
    plot.background = element_rect(fill = "#c9c1ad", color = NA),
    plot.title = element_text(size = 28, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 16, hjust = 0.5, lineheight = 1.4),
    plot.margin = margin(30, 30, 30, 30)
  ) +
  
  labs(
    title = "Nobel Peace Prize Winners",
    subtitle = "A Chronological Book of Laureates\n1901–2025"
  )


# ---------------------------
# Load Libraries
# ---------------------------
library(tidyverse)

# ---------------------------
# Load & Sort Data
# ---------------------------
nobel_peace <- read_csv("nobel_peace.csv")

nobel_sorted <- nobel_peace %>%
  distinct(year, name) %>%
  arrange(year)

names <- nobel_sorted$name

n <- length(names)
mid <- floor(n / 2)

left_names  <- names[1:mid]
right_names <- names[(mid + 1):n]

# ---------------------------
# Function to create 2-column layout per page
# ---------------------------

create_page <- function(name_vector, side = "left") {
  
  total <- length(name_vector)
  columns <- 2
  rows <- ceiling(total / columns)
  
  df <- tibble(
    name = name_vector,
    col = rep(1:columns, each = rows)[1:total],
    row = rep(1:rows, times = columns)[1:total]
  )
  
  # Spread vertically across page
  df <- df %>%
    mutate(
      y = 0.94 - (row - 1) * ((0.88) / (rows - 1))
    )
  
  if (side == "left") {
    df <- df %>%
      mutate(
        x = ifelse(col == 1, -0.75, -0.58),
        hjust = 0
      )
  } else {
    df <- df %>%
      mutate(
        x = ifelse(col == 1, 0.58, 0.75),
        hjust = 0
      )
  }
  
  df
}

left_df  <- create_page(left_names, "left")
right_df <- create_page(right_names, "right")

book_df <- bind_rows(left_df, right_df)

# ---------------------------
# Plot
# ---------------------------

ggplot() +
  
  # Drop shadow
  annotate("rect",
           xmin = -0.94, xmax = 0.94,
           ymin = 0.03, ymax = 0.97,
           fill = "black", alpha = 0.12) +
  
  # Hard cover
  annotate("rect",
           xmin = -0.96, xmax = 0.96,
           ymin = 0, ymax = 1,
           fill = "#5a4636") +
  
  # Back pages
  annotate("rect",
           xmin = -0.86, xmax = -0.04,
           ymin = 0.07, ymax = 0.93,
           fill = "#efe9db") +
  
  annotate("rect",
           xmin = 0.04, xmax = 0.86,
           ymin = 0.07, ymax = 0.93,
           fill = "#efe9db") +
  
  # Main pages
  annotate("rect",
           xmin = -0.82, xmax = -0.02,
           ymin = 0.06, ymax = 0.94,
           fill = "#f8f4e8",
           color = "#3e3e3e") +
  
  annotate("rect",
           xmin = 0.02, xmax = 0.82,
           ymin = 0.06, ymax = 0.94,
           fill = "#f8f4e8",
           color = "#3e3e3e") +
  
  # Spine
  annotate("rect",
           xmin = -0.02, xmax = 0.02,
           ymin = 0.06, ymax = 0.94,
           fill = "#3e3e3e") +
  
  # Fold shading
  annotate("rect",
           xmin = -0.08, xmax = 0.08,
           ymin = 0.06, ymax = 0.94,
           fill = "black", alpha = 0.05) +
  
  # Names (Now Properly Spread in Columns)
  geom_text(data = book_df,
            aes(x = x, y = y, label = name),
            size = 2.6,
            family = "serif",
            color = "#1f1f1f",
            hjust = 0) +
  
  coord_cartesian(xlim = c(-1, 1),
                  ylim = c(0, 1),
                  clip = "off") +
  
  theme_void() +
  
  theme(
    plot.background = element_rect(fill = "#c9c1ad", color = NA),
    plot.title = element_text(size = 28, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 16, hjust = 0.5, lineheight = 1.4),
    plot.margin = margin(30, 30, 30, 30)
  ) +
  
  labs(
    title = "Nobel Peace Prize Winners",
    subtitle = "A Chronological Book of Laureates\n1901–2025"
  )


# ---------------------------
# Load Libraries
# ---------------------------
library(tidyverse)
library(ggrepel)

set.seed(42)  # for reproducibility

# ---------------------------
# Load & Sort Data
# ---------------------------
nobel_peace <- read_csv("nobel_peace.csv")

nobel_sorted <- nobel_peace %>%
  distinct(year, name) %>%
  arrange(year)

names <- nobel_sorted$name

n <- length(names)
mid <- floor(n / 2)

left_names  <- names[1:mid]
right_names <- names[(mid + 1):n]

# ---------------------------
# Generate Random Positions
# ---------------------------

left_df <- tibble(
  name = left_names,
  x = runif(length(left_names), -0.80, -0.05),
  y = runif(length(left_names), 0.08, 0.92)
)

right_df <- tibble(
  name = right_names,
  x = runif(length(right_names), 0.05, 0.80),
  y = runif(length(right_names), 0.08, 0.92)
)

book_df <- bind_rows(left_df, right_df)

# ---------------------------
# Plot
# ---------------------------

ggplot() +
  
  # Drop shadow
  annotate("rect",
           xmin = -0.94, xmax = 0.94,
           ymin = 0.03, ymax = 0.97,
           fill = "black", alpha = 0.12) +
  
  # Hard cover
  annotate("rect",
           xmin = -0.96, xmax = 0.96,
           ymin = 0, ymax = 1,
           fill = "#5a4636") +
  
  # Back pages
  annotate("rect",
           xmin = -0.86, xmax = -0.04,
           ymin = 0.07, ymax = 0.93,
           fill = "#efe9db") +
  
  annotate("rect",
           xmin = 0.04, xmax = 0.86,
           ymin = 0.07, ymax = 0.93,
           fill = "#efe9db") +
  
  # Main pages
  annotate("rect",
           xmin = -0.82, xmax = -0.02,
           ymin = 0.06, ymax = 0.94,
           fill = "#f8f4e8",
           color = "#3e3e3e") +
  
  annotate("rect",
           xmin = 0.02, xmax = 0.82,
           ymin = 0.06, ymax = 0.94,
           fill = "#f8f4e8",
           color = "#3e3e3e") +
  
  # Spine
  annotate("rect",
           xmin = -0.02, xmax = 0.02,
           ymin = 0.06, ymax = 0.94,
           fill = "#3e3e3e") +
  
  # Fold shading
  annotate("rect",
           xmin = -0.08, xmax = 0.08,
           ymin = 0.06, ymax = 0.94,
           fill = "black", alpha = 0.05) +
  
  # Random Spread Names (Non-overlapping)
  geom_text_repel(
    data = book_df,
    aes(x = x, y = y, label = name),
    size = 2.5,
    family = "serif",
    color = "#1f1f1f",
    box.padding = 0.2,
    point.padding = 0.2,
    segment.color = NA,
    max.overlaps = Inf
  ) +
  
  coord_cartesian(xlim = c(-1, 1),
                  ylim = c(0, 1),
                  clip = "off") +
  
  theme_void() +
  
  theme(
    plot.background = element_rect(fill = "#c9c1ad", color = NA),
    plot.title = element_text(size = 28, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 16, hjust = 0.5, lineheight = 1.4),
    plot.margin = margin(30, 30, 30, 30)
  ) +
  
  labs(
    title = "Nobel Peace Prize Winners",
    subtitle = "A Chronological Book of Laureates\n1901–2025"
  )


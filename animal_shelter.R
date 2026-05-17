library(tidyverse)
library(lubridate)

df <- read_csv("animal-shelter-intakes-and-outcomes.csv")

names(df)

df <- df |>
  rename(
    animal_type   = "Animal Type",
    intake_reason = "Reason for Intake",
    intake_date   = "Intake Date"
  )

df <- df |>
  mutate(
    intake_date = ymd(intake_date),
    intake_year = year(intake_date)
  )

plot_data <- df |>
  select(animal_type, intake_reason, intake_year)


# Load required libraries
library(tidyverse)
library(lubridate)

# Read in the data
df <- read_csv("animal-shelter-intakes-and-outcomes.csv")

# View original column names
names(df)

# Rename columns for easier use
df <- df |>
  rename(
    animal_type   = "Animal Type",
    intake_reason = "Reason for Intake",
    intake_date   = "Intake Date"
  )

# Convert intake_date to Date format and extract year
df <- df |>
  mutate(
    intake_date = ymd(intake_date),
    intake_year = year(intake_date)
  )

# Select relevant columns and remove NULL / missing values
plot_data <- df |>
  select(animal_type, intake_reason, intake_year) |>
  filter(
    !is.na(animal_type),
    !is.na(intake_reason),
    !is.na(intake_year),
    animal_type != "NULL",
    intake_reason != "NULL",
    animal_type != "",
    intake_reason != ""
  )

# Quick check of cleaned data
summary(plot_data)



# Load required libraries
library(tidyverse)
library(lubridate)

# Read in the data
df <- read_csv("animal-shelter-intakes-and-outcomes.csv")

# Rename columns for easier use
df <- df |>
  rename(
    animal_type   = "Animal Type",
    intake_reason = "Reason for Intake",
    intake_date   = "Intake Date"
  )

# Convert intake_date to Date format and extract year
df <- df |>
  mutate(
    intake_date = ymd(intake_date),
    intake_year = year(intake_date)
  )

# Select relevant columns and remove NULL / missing values
plot_data <- df |>
  select(animal_type, intake_reason, intake_year) |>
  filter(
    !is.na(animal_type),
    !is.na(intake_reason),
    !is.na(intake_year),
    animal_type != "NULL",
    intake_reason != "NULL",
    animal_type != "",
    intake_reason != ""
  )

# Quick check of cleaned data
summary(plot_data)

# Create a bar plot: Number of intakes by animal type
plot_data |>
  count(animal_type) |>
  ggplot(aes(x = animal_type, y = n)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Number of Animal Shelter Intakes by Animal Type",
    x = "Animal Type",
    y = "Number of Intakes"
  ) +
  theme_minimal()

# Load required libraries
library(tidyverse)
library(lubridate)

# Read in the data
df <- read_csv("animal-shelter-intakes-and-outcomes.csv")

# Rename columns for easier use
df <- df |>
  rename(
    animal_type   = "Animal Type",
    intake_reason = "Reason for Intake",
    intake_date   = "Intake Date"
  )

# Convert intake_date to Date format and extract year
df <- df |>
  mutate(
    intake_date = ymd(intake_date),
    intake_year = year(intake_date)
  )

# Select relevant columns and remove NULL / missing values
plot_data <- df |>
  select(animal_type, intake_reason, intake_year) |>
  filter(
    !is.na(animal_type),
    !is.na(intake_reason),
    !is.na(intake_year),
    animal_type != "NULL",
    intake_reason != "NULL",
    animal_type != "",
    intake_reason != ""
  )

# Create a stylish bar plot
plot_data |>
  count(animal_type) |>
  mutate(animal_type = reorder(animal_type, n)) |>
  ggplot(aes(x = animal_type, y = n, fill = animal_type)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = n), hjust = -0.1, size = 4) +
  coord_flip() +
  labs(
    title = "Animal Shelter Intakes by Animal Type",
    subtitle = "Total number of recorded intakes",
    x = "Animal Type",
    y = "Number of Intakes"
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray40"),
    panel.grid.major.y = element_blank()
  )

# Load required libraries
library(tidyverse)
library(lubridate)

# Read in the data
df <- read_csv("animal-shelter-intakes-and-outcomes.csv")

# Rename columns for easier use
df <- df |>
  rename(
    animal_type   = "Animal Type",
    intake_reason = "Reason for Intake",
    intake_date   = "Intake Date"
  )

# Convert intake_date to Date format and extract year
df <- df |>
  mutate(
    intake_date = ymd(intake_date),
    intake_year = year(intake_date)
  )

# Select relevant columns and remove NULL / missing values
plot_data <- df |>
  select(animal_type, intake_reason, intake_year) |>
  filter(
    !is.na(animal_type),
    !is.na(intake_reason),
    !is.na(intake_year),
    animal_type != "NULL",
    intake_reason != "NULL",
    animal_type != "",
    intake_reason != ""
  )

# Filter for comparison years (2017 and 2025)
plot_data_filtered <- plot_data |>
  filter(intake_year %in% c(2017, 2025))

# Create faceted bar plot comparing years
plot_data_filtered |>
  count(intake_year, animal_type) |>
  ggplot(aes(x = animal_type, y = n, fill = animal_type)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = n), vjust = -0.3, size = 4) +
  facet_wrap(~ intake_year) +
  labs(
    title = "Animal Shelter Intakes by Animal Type",
    subtitle = "Comparison between 2017 and 2025",
    x = "Animal Type",
    y = "Number of Intakes"
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray40"),
    panel.grid.major.x = element_blank()
  )


# Load required libraries
library(tidyverse)
library(lubridate)

# Read in the data
df <- read_csv("animal-shelter-intakes-and-outcomes.csv")

# Rename columns for easier use
df <- df |>
  rename(
    animal_type   = "Animal Type",
    intake_reason = "Reason for Intake",
    intake_date   = "Intake Date"
  )

# Convert intake_date to Date format and extract year
df <- df |>
  mutate(
    intake_date = ymd(intake_date),
    intake_year = year(intake_date)
  )

# Select relevant columns and clean data
plot_data <- df |>
  select(animal_type, intake_reason, intake_year) |>
  filter(
    !is.na(animal_type),
    !is.na(intake_reason),
    !is.na(intake_year),
    animal_type != "NULL",
    intake_reason != "NULL",
    animal_type != "",
    intake_reason != ""
  )

# Filter for years of interest
plot_data_filtered <- plot_data |>
  filter(intake_year %in% c(2017, 2025))

# Create stacked bar plot accounting for intake reason
plot_data_filtered |>
  count(intake_year, animal_type, intake_reason) |>
  ggplot(aes(x = animal_type, y = n, fill = intake_reason)) +
  geom_col() +
  facet_wrap(~ intake_year) +
  labs(
    title = "Animal Shelter Intakes by Animal Type and Reason",
    subtitle = "Comparison of intake reasons in 2017 and 2025",
    x = "Animal Type",
    y = "Number of Intakes",
    fill = "Reason for Intake"
  ) +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray40"),
    axis.text.x = element_text(angle = 30, hjust = 1),
    panel.grid.major.x = element_blank()
  )


# Load required libraries
library(tidyverse)
library(lubridate)

# Read in the data
df <- read_csv("animal-shelter-intakes-and-outcomes.csv")

# Rename columns for easier use
df <- df |>
  rename(
    animal_type   = "Animal Type",
    intake_reason = "Reason for Intake",
    intake_date   = "Intake Date"
  )

# Convert intake_date to Date format and extract year
df <- df |>
  mutate(
    intake_date = ymd(intake_date),
    intake_year = year(intake_date)
  )

# Select relevant columns and clean data
plot_data <- df |>
  select(animal_type, intake_reason, intake_year) |>
  filter(
    !is.na(animal_type),
    !is.na(intake_reason),
    !is.na(intake_year),
    animal_type != "NULL",
    intake_reason != "NULL",
    animal_type != "",
    intake_reason != ""
  )

# Filter for years of interest
plot_data_filtered <- plot_data |>
  filter(intake_year %in% c(2017, 2025))

# Identify intake reasons that bring in the MOST animals (top 5)
top_reasons <- plot_data_filtered |>
  count(intake_reason, sort = TRUE) |>
  slice_head(n = 5) |>
  pull(intake_reason)

# Keep only the top intake reasons
plot_data_top_reasons <- plot_data_filtered |>
  filter(intake_reason %in% top_reasons)

# Create stacked bar plot using only top intake reasons
plot_data_top_reasons |>
  count(intake_year, animal_type, intake_reason) |>
  ggplot(aes(x = animal_type, y = n, fill = intake_reason)) +
  geom_col() +
  facet_wrap(~ intake_year) +
  labs(
    title = "Animal Shelter Intakes by Animal Type and Primary Intake Reasons",
    subtitle = "Only the most common intake reasons shown (2017 vs 2025)",
    x = "Animal Type",
    y = "Number of Intakes",
    fill = "Reason for Intake"
  ) +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray40"),
    axis.text.x = element_text(angle = 30, hjust = 1),
    panel.grid.major.x = element_blank()
  )


# Load required libraries
library(tidyverse)
library(lubridate)

# Read in the data
df <- read_csv("animal-shelter-intakes-and-outcomes.csv")

# Rename columns for easier use
df <- df |>
  rename(
    animal_type   = "Animal Type",
    intake_reason = "Reason for Intake",
    intake_date   = "Intake Date"
  )

# Convert intake_date to Date format and extract year
df <- df |>
  mutate(
    intake_date = ymd(intake_date),
    intake_year = year(intake_date)
  )

# Select relevant columns and clean data
plot_data <- df |>
  select(animal_type, intake_reason, intake_year) |>
  filter(
    !is.na(animal_type),
    !is.na(intake_reason),
    !is.na(intake_year),
    animal_type != "NULL",
    intake_reason != "NULL",
    animal_type != "",
    intake_reason != ""
  )

# Filter for years of interest
plot_data_filtered <- plot_data |>
  filter(intake_year %in% c(2017, 2025))

# Identify the top 5 intake reasons across BOTH years
top_reasons <- plot_data_filtered |>
  count(intake_reason, sort = TRUE) |>
  slice_head(n = 5) |>
  pull(intake_reason)

# Keep only the top intake reasons
plot_data_top_reasons <- plot_data_filtered |>
  filter(intake_reason %in% top_reasons)

# Create stacked bar plots:
# - Each animal type gets its own panel
# - 2017 vs 2025 compared within each animal
# - Bars stacked by intake reason
plot_data_top_reasons |>
  count(animal_type, intake_year, intake_reason) |>
  mutate(intake_year = factor(intake_year)) |>
  ggplot(aes(x = intake_year, y = n, fill = intake_reason)) +
  geom_col() +
  facet_wrap(~ animal_type, scales = "free_y") +
  labs(
    title = "Animal Shelter Intakes by Animal Type and Intake Reason",
    subtitle = "Top 5 intake reasons only, comparing 2017 and 2025",
    x = "Year",
    y = "Number of Intakes",
    fill = "Reason for Intake"
  ) +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray40"),
    strip.text = element_text(face = "bold"),
    panel.grid.major.x = element_blank()
  )


# -------------------------------
# Load required libraries
# -------------------------------
library(tidyverse)
library(lubridate)

# -------------------------------
# Read in the data
# -------------------------------
df <- read_csv("animal-shelter-intakes-and-outcomes.csv")

# -------------------------------
# Rename columns for easier use
# -------------------------------
df <- df |>
  rename(
    animal_type   = "Animal Type",
    intake_reason = "Reason for Intake",
    intake_date   = "Intake Date"
  )

# -------------------------------
# Convert intake date and extract year
# -------------------------------
df <- df |>
  mutate(
    intake_date = ymd(intake_date),
    intake_year = year(intake_date)
  )

# -------------------------------
# Clean and select relevant data
# -------------------------------
plot_data <- df |>
  select(animal_type, intake_reason, intake_year) |>
  filter(
    intake_year %in% c(2017, 2025),
    !is.na(animal_type),
    !is.na(intake_reason),
    animal_type != "NULL",
    intake_reason != "NULL",
    animal_type != "",
    intake_reason != ""
  )

# -------------------------------
# Identify the top 5 intake reasons across BOTH years
# -------------------------------
top_reasons <- plot_data |>
  count(intake_reason, sort = TRUE) |>
  slice_head(n = 5) |>
  pull(intake_reason)

# -------------------------------
# Keep only the top 5 reasons
# -------------------------------
plot_data_top_reasons <- plot_data |>
  filter(intake_reason %in% top_reasons)

# -------------------------------
# Create organized side-by-side bar chart
# -------------------------------
plot_data_top_reasons |>
  count(animal_type, intake_year, intake_reason) |>
  mutate(
    intake_year = factor(intake_year),
    intake_reason = fct_reorder(intake_reason, n)
  ) |>
  ggplot(
    aes(
      x = n,
      y = intake_reason,
      fill = intake_year
    )
  ) +
  geom_col(position = position_dodge(width = 0.8)) +
  facet_wrap(~ animal_type, scales = "free_x") +
  labs(
    title = "Animal Shelter Intakes by Animal Type and Intake Reason",
    subtitle = "Top 5 intake reasons per animal, comparing 2017 and 2025",
    x = "Number of Intakes",
    y = "Reason for Intake",
    fill = "Year"
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray40"),
    strip.text = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    legend.position = "top"
  )





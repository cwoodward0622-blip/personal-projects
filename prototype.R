library(tidyverse)

df <- read_csv("plot_data.csv")

names(df)

# prototype

# perform some data manipulation first

prototype_df <- df |>
  filter(intake_reason != "NULL") |>
  filter(intake_year == 2017 & animal_type == "CAT") |>
  group_by(intake_reason) |>
  summarize(count = n()) |>
  arrange(desc(count)) |>
  head(5)

#create a basic bar plot

ggplot(
  data = prototype_df,
  mapping = aes(y = fct_reorder(intake_reason, count), intake_type, x = count)
) +
  geom_col(fill = "blue") +
  labs(
    x = "Count",
    y = ""
  ) +
  theme_minimal()

df |>
  filter(intake_reason != "NULL") |>
  filter(intake_year == 2017 | intake_year == 2025) |>
  

plot_df <- df |>
  filter(intake_reason != "NULL") |>
  filter(intake_year == 2017 | intake_year == 2025) |>
  group_by(intake_reason, animal_type, intake_year) |>
  summarize(count = n())

ggplot(
  data = plot_df,
  mapping
)

  
  
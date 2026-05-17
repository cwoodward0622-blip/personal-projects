
# ---------------------------------------------------------
# Load libraries
# ---------------------------------------------------------
library(tidyverse)
library(plotly)
library(htmlwidgets)
library(zoo)
library(countrycode)

# ---------------------------------------------------------
# Load + Clean Data
# ---------------------------------------------------------
df <- read_csv("doctors.csv") %>%
  arrange(Entity, Year)

df <- df %>%
  filter(!str_detect(Entity, "\\(WB\\)")) %>%
  filter(!Entity %in% c(
    "World",
    "High-income countries",
    "Low-income countries",
    "Lower-middle-income countries",
    "Upper-middle-income countries",
    "Channel Islands",
    "Micronesia (country)",
    "Middle East, North Africa",
    "European Union (27)"
  ))

# Add ISO codes
df <- df %>%
  mutate(Code = countrycode(Entity, "country.name", "iso3c"))

df$Code[df$Entity == "United States"] <- "USA"
df$Code[df$Entity == "Russia"] <- "RUS"

df <- df %>% filter(!is.na(Code))

# ---------------------------------------------------------
# Fill Missing Years
# ---------------------------------------------------------
all_years <- 1960:2023

df_full <- df %>%
  group_by(Entity, Code) %>%
  complete(Year = all_years) %>%
  arrange(Entity, Year) %>%
  mutate(value = zoo::na.locf(`Physicians (per 1,000 people)`, na.rm = FALSE)) %>%
  ungroup()

# ---------------------------------------------------------
# Handle Missing Data
# ---------------------------------------------------------
df_full <- df_full %>%
  mutate(value_plot = ifelse(is.na(value), -1, value))

# ---------------------------------------------------------
# Build Map
# ---------------------------------------------------------
fig <- plot_ly(
  data = df_full,
  type = "choropleth",
  locations = ~Code,
  z = ~value_plot,
  frame = ~Year,
  
  text = ~paste(
    "Country:", Entity,
    "<br>Year:", Year,
    "<br>Physicians per 1,000:", round(value, 2)
  ),
  hoverinfo = "text",
  
  colorscale = list(
    c(0, "#d9d9d9"),
    c(0.001, "#440154"),
    c(0.25, "#3b528b"),
    c(0.5, "#21918c"),
    c(0.75, "#5ec962"),
    c(1, "#fde725")
  ),
  
  zmin = -1,
  zmax = 8,
  
  colorbar = list(
    title = "Physicians<br>per 1,000"
  ),
  
  marker = list(
    line = list(color = "white", width = 0.5)
  )
)

# ---------------------------------------------------------
# Animation (RAISED CONTROLS)
# ---------------------------------------------------------
fig <- fig %>%
  animation_opts(
    frame = 600,
    transition = 0,
    redraw = TRUE
  ) %>%
  animation_button(
    x = 0.1,
    y = 0.12,   # 🔥 raised from 0.05 → 0.12
    xanchor = "left",
    yanchor = "bottom"
  ) %>%
  animation_slider(
    x = 0.1,
    y = 0.08,   # 🔥 explicitly raise slider
    len = 0.9
  )

# ---------------------------------------------------------
# Layout
# ---------------------------------------------------------
fig <- fig %>%
  layout(
    title = list(
      text = paste0(
        "<b>Global Physician Availability</b><br>",
        "Physicians per 1,000 People (1960–2023)<br>",
        "<i>Darker colors indicate lower physician availability (greater healthcare scarcity)</i>"
      ),
      x = 0.5
    ),
    
    geo = list(
      scope = "world",
      projection = list(type = "natural earth")
    ),
    
    annotations = list(
      
      list(
        text = "<b>Spain Healthcare Expansion (1991)</b><br>
        Spain’s spike reflects earlier growth in medical training and healthcare expansion,
        leading to a surge of new doctors entering the workforce in the early 1990s.",
        x = 0.5,
        y = 0.13,
        xref = "paper",
        yref = "paper",
        showarrow = FALSE,
        align = "center",
        font = list(size = 10),
        bgcolor = "rgba(255,255,255,0.95)",
        bordercolor = "black",
        borderwidth = 1
      ),
      
      list(
        text = "<b>Italy Physician Surge (2002)</b><br>
        Around 2002, Italy experienced a structural oversupply of physicians due to decades
        of high medical school enrollment.",
        x = 0.5,
        y = 0.04,
        xref = "paper",
        yref = "paper",
        showarrow = FALSE,
        align = "center",
        font = list(size = 10),
        bgcolor = "rgba(255,255,255,0.95)",
        bordercolor = "black",
        borderwidth = 1
      ),
      
      list(
        text = "<i>Gray = No data available</i>",
        x = 0.02,
        y = 0.02,
        xref = "paper",
        yref = "paper",
        showarrow = FALSE,
        align = "left",
        font = list(size = 10, color = "gray40")
      )
    ),
    
    # keep margin
    margin = list(r = 0, t = 100, l = 0, b = 80)
  )

# ---------------------------------------------------------
# Show + Save
# ---------------------------------------------------------
fig
saveWidget(fig, "global_doctor_density_map.html")














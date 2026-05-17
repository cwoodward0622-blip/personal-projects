# ---------------------------------------------------------
# Global Doctor Density Animated Choropleth (1960–2023)
# ---------------------------------------------------------

# Install packages (only run once)
install.packages("tidyverse")
install.packages("plotly")
install.packages("htmlwidgets")
install.packages("zoo")

# Load libraries
library(tidyverse)
library(plotly)
library(htmlwidgets)
library(zoo)

# ---------------------------------------------------------
# Load Data
# ---------------------------------------------------------

df <- read_csv("doctors.csv")

# Sort data
df <- df %>%
  arrange(Entity, Year)

# ---------------------------------------------------------
# Fill Missing Years (Carry Last Value Forward)
# ---------------------------------------------------------

all_years <- 1960:2023

df_full <- df %>%
  
  group_by(Entity, Code) %>%
  
  complete(Year = all_years) %>%
  
  arrange(Entity, Year) %>%
  
  mutate(
    `Physicians (per 1,000 people)` =
      zoo::na.locf(`Physicians (per 1,000 people)`, na.rm = FALSE)
  ) %>%
  
  ungroup()

# Remove remaining NA values
df_full <- df_full %>%
  filter(!is.na(`Physicians (per 1,000 people)`))

# ---------------------------------------------------------
# Create Animated Global Map
# ---------------------------------------------------------

fig <- plot_ly(
  
  data = df_full,
  
  type = "choropleth",
  
  locations = ~Code,
  
  z = ~`Physicians (per 1,000 people)`,
  
  text = ~paste(
    "Country:", Entity,
    "<br>Year:", Year,
    "<br>Physicians per 1,000:", round(`Physicians (per 1,000 people)`,2)
  ),
  
  frame = ~Year,
  
  colorscale = "Tealgrn",
  
  zmin = 0,
  zmax = 8,
  
  marker = list(
    line = list(color = "white", width = 0.5)
  )
  
) %>%
  
  layout(
    
    title = list(
      text = "<b>The Global Evolution of Medical Care</b><br>Physicians per 1,000 People (1960–2023)",
      y = 0.95,
      x = 0.5,
      xanchor = "center",
      yanchor = "top",
      font = list(size = 20, family = "Arial")
    ),
    
    geo = list(
      scope = "world",
      showframe = FALSE,
      showcoastlines = TRUE,
      projection = list(type = "natural earth")
    ),
    
    margin = list(r = 0, t = 100, l = 0, b = 0)
  )

# ---------------------------------------------------------
# Display Map
# ---------------------------------------------------------

fig

# ---------------------------------------------------------
# Save Interactive Map
# ---------------------------------------------------------

saveWidget(fig, "global_doctor_density_map.html")







# Load libraries
library(tidyverse)
library(plotly)
library(htmlwidgets)
library(zoo)

# ---------------------------------------------------------
# Load Data
# ---------------------------------------------------------

df <- read_csv("doctors.csv")

df <- df %>%
  arrange(Entity, Year)

# ---------------------------------------------------------
# Fill Missing Years (carry previous value forward)
# ---------------------------------------------------------

all_years <- 1960:2023

df_full <- df %>%
  
  group_by(Entity, Code) %>%
  
  complete(Year = all_years) %>%
  
  arrange(Entity, Year) %>%
  
  mutate(
    `Physicians (per 1,000 people)` =
      zoo::na.locf(`Physicians (per 1,000 people)`, na.rm = FALSE)
  ) %>%
  
  ungroup()

df_full <- df_full %>%
  filter(!is.na(`Physicians (per 1,000 people)`))

# ---------------------------------------------------------
# Create Animated Global Map
# ---------------------------------------------------------

fig <- plot_ly(
  
  data = df_full,
  
  type = "choropleth",
  
  locations = ~Code,
  
  z = ~`Physicians (per 1,000 people)`,
  
  text = ~paste(
    "Country:", Entity,
    "<br>Year:", Year,
    "<br>Physicians per 1,000:", round(`Physicians (per 1,000 people)`,2)
  ),
  
  frame = ~Year,
  
  colorscale = "RdBu",     # red to blue scale
  reversescale = TRUE,     # makes LOW values red
  
  zmin = 0,
  zmax = 8,
  
  marker = list(
    line = list(color = "white", width = 0.5)
  )
  
) %>%
  
  layout(
    
    title = list(
      text = "<b>Global Physician Availability</b><br>Physicians per 1,000 People (1960–2023)",
      y = 0.95,
      x = 0.5,
      xanchor = "center",
      yanchor = "top",
      font = list(size = 20, family = "Arial")
    ),
    
    geo = list(
      scope = "world",
      showframe = FALSE,
      showcoastlines = TRUE,
      projection = list(type = "natural earth")
    ),
    
    margin = list(r = 0, t = 100, l = 0, b = 0)
  )

# ---------------------------------------------------------
# Display Map
# ---------------------------------------------------------

fig

# ---------------------------------------------------------
# Save Interactive Map
# ---------------------------------------------------------

saveWidget(fig, "global_doctor_density_map.html")







# Load libraries
library(tidyverse)
library(plotly)
library(htmlwidgets)
library(zoo)
library(countrycode)

# ---------------------------------------------------------
# Load Data
# ---------------------------------------------------------

df <- read_csv("doctors.csv") %>%
  arrange(Entity, Year)

# ---------------------------------------------------------
# Remove NON-COUNTRIES
# ---------------------------------------------------------

non_countries <- c(
  "World",
  "High-income countries",
  "Low-income countries",
  "Lower-middle-income countries",
  "Upper-middle-income countries",
  "Europe and Central Asia (WB)",
  "East Asia and Pacific (WB)",
  "Latin America and Caribbean (WB)",
  "Middle East and North Africa",
  "South Asia (WB)",
  "Sub-Saharan Africa (WB)",
  "North America (WB)",
  "European Union (27)"
)

df <- df %>%
  filter(!Entity %in% non_countries)

# ---------------------------------------------------------
# Add Country Codes
# ---------------------------------------------------------

df <- df %>%
  mutate(Code = countrycode(Entity, "country.name", "iso3c"))

# Fix mismatches
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
  ungroup() %>%
  filter(!is.na(value))

# ---------------------------------------------------------
# CREATE ANNOTATION COLUMN (KEY TRICK)
# ---------------------------------------------------------

df_full <- df_full %>%
  mutate(
    note = ifelse(
      Year == 2002,
      paste(
        "<b>Italy Physician Surge (2002)</b><br>",
        "• Overproduction of doctors from past decades<br>",
        "• ~336,000 physicians by late 1990s<br>",
        "• ~39,000 unemployed doctors<br>",
        "• Structural imbalance in healthcare system"
      ),
      ""
    )
  )

# ---------------------------------------------------------
# Create Animated Plot (STABLE METHOD)
# ---------------------------------------------------------

fig <- plot_ly(
  data = df_full,
  type = "choropleth",
  locations = ~Code,
  z = ~value,
  frame = ~Year,
  
  text = ~paste(
    "Country:", Entity,
    "<br>Year:", Year,
    "<br>Physicians per 1,000:", round(value, 2)
  ),
  
  colorscale = "RdBu",
  reversescale = TRUE,
  zmin = 0,
  zmax = 8,
  
  marker = list(line = list(color = "white", width = 0.5))
)

# ---------------------------------------------------------
# Layout WITH CONDITIONAL ANNOTATION
# ---------------------------------------------------------

fig <- fig %>%
  layout(
    title = list(
      text = "<b>Global Physician Availability</b><br>Physicians per 1,000 People (1960–2023)",
      x = 0.5
    ),
    
    geo = list(
      scope = "world",
      projection = list(type = "natural earth")
    ),
    
    margin = list(r = 0, t = 80, l = 0, b = 0),
    
    # This makes annotation update with frames
    annotations = list(
      list(
        text = "",
        x = 0.5,
        y = 0.15,
        xref = "paper",
        yref = "paper",
        showarrow = FALSE,
        bgcolor = "rgba(255,255,255,0.9)",
        bordercolor = "black"
      )
    )
  ) %>%
  
  animation_opts(
    frame = 800,
    transition = 0,
    redraw = TRUE
  ) %>%
  
  animation_slider(
    currentvalue = list(prefix = "Year: ")
  )

# ---------------------------------------------------------
# Show
# ---------------------------------------------------------

fig

# ---------------------------------------------------------
# Save
# ---------------------------------------------------------

saveWidget(fig, "global_doctor_density_map.html")





# Load libraries
library(tidyverse)
library(plotly)
library(htmlwidgets)
library(zoo)
library(countrycode)

# ---------------------------------------------------------
# Load Data
# ---------------------------------------------------------

df <- read_csv("doctors.csv") %>%
  arrange(Entity, Year)

# ---------------------------------------------------------
# REMOVE NON-COUNTRIES (NO WARNINGS)
# ---------------------------------------------------------

df <- df %>%
  filter(!str_detect(Entity, "\\(WB\\)")) %>%  # remove World Bank regions
  filter(!Entity %in% c(
    "World",
    "High-income countries",
    "Low-income countries",
    "Lower-middle-income countries",
    "Upper-middle-income countries",
    "Channel Islands",
    "Micronesia (country)",
    "Middle East, North Africa"
  ))

# ---------------------------------------------------------
# ADD COUNTRY CODES
# ---------------------------------------------------------

df <- df %>%
  mutate(Code = countrycode(Entity, "country.name", "iso3c"))

# Fix common mismatches
df$Code[df$Entity == "United States"] <- "USA"
df$Code[df$Entity == "Russia"] <- "RUS"

# Remove any remaining NA codes
df <- df %>% filter(!is.na(Code))

# ---------------------------------------------------------
# Fill Missing Years
# ---------------------------------------------------------

all_years <- 1960:2023

df_full <- df %>%
  group_by(Entity, Code) %>%
  complete(Year = all_years) %>%
  arrange(Entity, Year) %>%
  mutate(
    value = zoo::na.locf(`Physicians (per 1,000 people)`, na.rm = FALSE)
  ) %>%
  ungroup() %>%
  filter(!is.na(value))

# ---------------------------------------------------------
# CREATE ANNOTATION TEXT (ONLY FOR 2002)
# ---------------------------------------------------------

df_full <- df_full %>%
  mutate(
    note = ifelse(
      Year == 2002,
      paste(
        "<b>Italy Physician Surge (2002)</b><br>",
        "• Overproduction of doctors from past decades<br>",
        "• ~336,000 physicians by late 1990s<br>",
        "• ~39,000 unemployed doctors<br>",
        "• Structural imbalance in healthcare system"
      ),
      ""
    )
  )

# ---------------------------------------------------------
# CREATE ANIMATED MAP (STABLE METHOD)
# ---------------------------------------------------------

fig <- plot_ly(
  data = df_full,
  type = "choropleth",
  
  locations = ~Code,
  z = ~value,
  frame = ~Year,
  
  text = ~paste(
    "Country:", Entity,
    "<br>Year:", Year,
    "<br>Physicians per 1,000:", round(value, 2)
  ),
  
  colorscale = "RdBu",
  reversescale = TRUE,
  zmin = 0,
  zmax = 8,
  
  marker = list(
    line = list(color = "white", width = 0.5)
  )
)

# ---------------------------------------------------------
# LAYOUT + ANNOTATION BOX
# ---------------------------------------------------------

fig <- fig %>%
  layout(
    title = list(
      text = "<b>Global Physician Availability</b><br>Physicians per 1,000 People (1960–2023)",
      x = 0.5
    ),
    
    geo = list(
      scope = "world",
      projection = list(type = "natural earth")
    ),
    
    margin = list(r = 0, t = 80, l = 0, b = 0),
    
    # Static annotation container (will show text when applicable)
    annotations = list(
      list(
        text = "",
        x = 0.5,
        y = 0.15,
        xref = "paper",
        yref = "paper",
        showarrow = FALSE,
        bgcolor = "rgba(255,255,255,0.9)",
        bordercolor = "black",
        borderwidth = 1
      )
    )
  ) %>%
  
  animation_opts(
    frame = 800,
    transition = 0,
    redraw = TRUE
  ) %>%
  
  animation_slider(
    currentvalue = list(prefix = "Year: ")
  )

# ---------------------------------------------------------
# DISPLAY
# ---------------------------------------------------------

fig

# ---------------------------------------------------------
# SAVE
# ---------------------------------------------------------

saveWidget(fig, "global_doctor_density_map.html")



# Load libraries
library(tidyverse)
library(plotly)
library(htmlwidgets)
library(zoo)
library(countrycode)

# ---------------------------------------------------------
# Load Data
# ---------------------------------------------------------

df <- read_csv("doctors.csv") %>%
  arrange(Entity, Year)

# ---------------------------------------------------------
# REMOVE NON-COUNTRIES (FULL CLEAN - NO WARNINGS)
# ---------------------------------------------------------

df <- df %>%
  filter(!str_detect(Entity, "\\(WB\\)")) %>%  # remove WB regions
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

# ---------------------------------------------------------
# ADD COUNTRY CODES
# ---------------------------------------------------------

df <- df %>%
  mutate(Code = countrycode(Entity, "country.name", "iso3c"))

# Fix common mismatches
df$Code[df$Entity == "United States"] <- "USA"
df$Code[df$Entity == "Russia"] <- "RUS"

# Remove any remaining NA codes
df <- df %>% filter(!is.na(Code))

# ---------------------------------------------------------
# Fill Missing Years
# ---------------------------------------------------------

all_years <- 1960:2023

df_full <- df %>%
  group_by(Entity, Code) %>%
  complete(Year = all_years) %>%
  arrange(Entity, Year) %>%
  mutate(
    value = zoo::na.locf(`Physicians (per 1,000 people)`, na.rm = FALSE)
  ) %>%
  ungroup() %>%
  filter(!is.na(value))

# ---------------------------------------------------------
# CREATE ANNOTATION TEXT (FOR 2002)
# ---------------------------------------------------------

df_full <- df_full %>%
  mutate(
    note = ifelse(
      Year == 2002,
      paste(
        "<b>Italy Physician Surge (2002)</b><br>",
        "• Overproduction of doctors from past decades<br>",
        "• ~336,000 physicians by late 1990s<br>",
        "• ~39,000 unemployed doctors<br>",
        "• Structural imbalance in healthcare system"
      ),
      ""
    )
  )

# ---------------------------------------------------------
# CREATE ANIMATED MAP
# ---------------------------------------------------------

fig <- plot_ly(
  data = df_full,
  type = "choropleth",
  
  locations = ~Code,
  z = ~value,
  frame = ~Year,
  
  text = ~paste(
    "Country:", Entity,
    "<br>Year:", Year,
    "<br>Physicians per 1,000:", round(value, 2)
  ),
  
  colorscale = "RdBu",
  reversescale = TRUE,
  zmin = 0,
  zmax = 8,
  
  marker = list(
    line = list(color = "white", width = 0.5)
  )
)

# ---------------------------------------------------------
# LAYOUT + ANNOTATION BOX
# ---------------------------------------------------------

fig <- fig %>%
  layout(
    title = list(
      text = "<b>Global Physician Availability</b><br>Physicians per 1,000 People (1960–2023)",
      x = 0.5
    ),
    
    geo = list(
      scope = "world",
      projection = list(type = "natural earth")
    ),
    
    margin = list(r = 0, t = 80, l = 0, b = 0),
    
    annotations = list(
      list(
        text = "",
        x = 0.5,
        y = 0.15,
        xref = "paper",
        yref = "paper",
        showarrow = FALSE,
        bgcolor = "rgba(255,255,255,0.9)",
        bordercolor = "black",
        borderwidth = 1
      )
    )
  ) %>%
  
  animation_opts(
    frame = 800,
    transition = 0,
    redraw = TRUE
  ) %>%
  
  animation_slider(
    currentvalue = list(prefix = "Year: ")
  )

# ---------------------------------------------------------
# DISPLAY
# ---------------------------------------------------------

fig

# ---------------------------------------------------------
# SAVE
# ---------------------------------------------------------

saveWidget(fig, "global_doctor_density_map.html")










# Load libraries
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
  ungroup() %>%
  filter(!is.na(value))

# ---------------------------------------------------------
# INITIAL FRAME
# ---------------------------------------------------------

first_year <- min(df_full$Year)
df_init <- df_full %>% filter(Year == first_year)

fig <- plot_ly(
  type = "choropleth",
  locations = df_init$Code,
  z = df_init$value,
  text = paste(
    "Country:", df_init$Entity,
    "<br>Year:", df_init$Year,
    "<br>Physicians per 1,000:", round(df_init$value, 2)
  ),
  colorscale = "RdBu",
  reversescale = TRUE,
  zmin = 0,
  zmax = 8,
  marker = list(line = list(color = "white", width = 0.5))
)

# ---------------------------------------------------------
# BUILD FRAMES (FIXED ANNOTATION LOGIC)
# ---------------------------------------------------------

years <- sort(unique(df_full$Year))

frames <- lapply(years, function(y) {
  
  df_year <- df_full %>% filter(Year == y)
  
  # IMPORTANT: Always define annotation (empty or filled)
  annotation_text <- ""
  
  if (y == 2002) {
    annotation_text <- "<b>Italy Physician Surge (2002)</b><br>
    Around 2002, Italy experienced a structural oversupply of physicians 
    due to decades of high medical school enrollment, which led to one 
    of the highest physician densities globally."
  }
  
  list(
    name = as.character(y),
    
    data = list(
      list(
        locations = df_year$Code,
        z = df_year$value,
        text = paste(
          "Country:", df_year$Entity,
          "<br>Year:", df_year$Year,
          "<br>Physicians per 1,000:", round(df_year$value, 2)
        )
      )
    ),
    
    layout = list(
      annotations = list(
        list(
          text = annotation_text,
          x = 0.5,
          y = 0.12,
          xref = "paper",
          yref = "paper",
          showarrow = FALSE,
          align = "center",
          bgcolor = ifelse(annotation_text == "", "rgba(0,0,0,0)", "rgba(255,255,255,0.95)"),
          bordercolor = "black",
          borderwidth = ifelse(annotation_text == "", 0, 1)
        )
      )
    )
  )
})

fig$x$frames <- frames

# ---------------------------------------------------------
# PLAY BUTTON + WORKING SLIDER
# ---------------------------------------------------------

fig <- fig %>%
  layout(
    updatemenus = list(
      list(
        type = "buttons",
        showactive = FALSE,
        buttons = list(
          list(
            label = "Play",
            method = "animate",
            args = list(
              NULL,
              list(
                frame = list(duration = 800, redraw = TRUE),
                transition = list(duration = 0),
                fromcurrent = TRUE
              )
            )
          )
        )
      )
    ),
    
    sliders = list(
      list(
        active = 0,
        currentvalue = list(prefix = "Year: "),
        steps = lapply(years, function(y) {
          list(
            label = y,
            method = "animate",
            args = list(
              list(as.character(y)),
              list(mode = "immediate",
                   frame = list(duration = 0, redraw = TRUE),
                   transition = list(duration = 0))
            )
          )
        })
      )
    )
  )

# ---------------------------------------------------------
# Layout
# ---------------------------------------------------------

fig <- fig %>%
  layout(
    title = list(
      text = "<b>Global Physician Availability</b><br>Physicians per 1,000 People (1960–2023)",
      x = 0.5
    ),
    geo = list(
      scope = "world",
      projection = list(type = "natural earth")
    ),
    margin = list(r = 0, t = 80, l = 0, b = 0)
  )

# ---------------------------------------------------------
# Show + Save
# ---------------------------------------------------------

fig
saveWidget(fig, "global_doctor_density_map.html")







# Load libraries
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
  ungroup() %>%
  filter(!is.na(value))

# ---------------------------------------------------------
# INITIAL FRAME
# ---------------------------------------------------------

first_year <- min(df_full$Year)
df_init <- df_full %>% filter(Year == first_year)

fig <- plot_ly(
  type = "choropleth",
  locations = df_init$Code,
  z = df_init$value,
  text = paste(
    "Country:", df_init$Entity,
    "<br>Year:", df_init$Year,
    "<br>Physicians per 1,000:", round(df_init$value, 2)
  ),
  colorscale = "RdBu",
  reversescale = TRUE,
  zmin = 0,
  zmax = 8,
  marker = list(line = list(color = "white", width = 0.5))
)

# ---------------------------------------------------------
# BUILD FRAMES (SPAIN + ITALY)
# ---------------------------------------------------------

years <- sort(unique(df_full$Year))

frames <- lapply(years, function(y) {
  
  df_year <- df_full %>% filter(Year == y)
  
  annotation_text <- ""
  
  # Spain 1991
  if (y == 1991) {
    annotation_text <- "<b>Spain Healthcare Expansion (1991)</b><br>
    Spain’s spike reflects earlier growth in medical training and healthcare expansion, 
    leading to a surge of new doctors entering the workforce in the early 1990s."
  }
  
  # Italy 2002
  if (y == 2002) {
    annotation_text <- "<b>Italy Physician Surge (2002)</b><br>
    Around 2002, Italy experienced a structural oversupply of physicians 
    due to decades of high medical school enrollment, which led to one 
    of the highest physician densities globally."
  }
  
  list(
    name = as.character(y),
    
    data = list(
      list(
        locations = df_year$Code,
        z = df_year$value,
        text = paste(
          "Country:", df_year$Entity,
          "<br>Year:", df_year$Year,
          "<br>Physicians per 1,000:", round(df_year$value, 2)
        )
      )
    ),
    
    layout = list(
      annotations = list(
        list(
          text = annotation_text,
          x = 0.5,
          y = 0.12,
          xref = "paper",
          yref = "paper",
          showarrow = FALSE,
          align = "center",
          bgcolor = ifelse(annotation_text == "", "rgba(0,0,0,0)", "rgba(255,255,255,0.95)"),
          bordercolor = "black",
          borderwidth = ifelse(annotation_text == "", 0, 1)
        )
      )
    )
  )
})

fig$x$frames <- frames

# ---------------------------------------------------------
# PLAY BUTTON + SLIDER
# ---------------------------------------------------------

fig <- fig %>%
  layout(
    updatemenus = list(
      list(
        type = "buttons",
        showactive = FALSE,
        buttons = list(
          list(
            label = "Play",
            method = "animate",
            args = list(
              NULL,
              list(
                frame = list(duration = 800, redraw = TRUE),
                transition = list(duration = 0),
                fromcurrent = TRUE
              )
            )
          )
        )
      )
    ),
    
    sliders = list(
      list(
        active = 0,
        currentvalue = list(prefix = "Year: "),
        steps = lapply(years, function(y) {
          list(
            label = y,
            method = "animate",
            args = list(
              list(as.character(y)),
              list(mode = "immediate",
                   frame = list(duration = 0, redraw = TRUE),
                   transition = list(duration = 0))
            )
          )
        })
      )
    )
  )

# ---------------------------------------------------------
# Layout
# ---------------------------------------------------------

fig <- fig %>%
  layout(
    title = list(
      text = "<b>Global Physician Availability</b><br>Physicians per 1,000 People (1960–2023)",
      x = 0.5
    ),
    geo = list(
      scope = "world",
      projection = list(type = "natural earth")
    ),
    margin = list(r = 0, t = 80, l = 0, b = 0)
  )

# ---------------------------------------------------------
# Show + Save
# ---------------------------------------------------------

fig
saveWidget(fig, "global_doctor_density_map.html")






# Load libraries
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
  ungroup() %>%
  filter(!is.na(value))

# ---------------------------------------------------------
# INITIAL FRAME
# ---------------------------------------------------------

first_year <- min(df_full$Year)
df_init <- df_full %>% filter(Year == first_year)

fig <- plot_ly(
  type = "choropleth",
  locations = df_init$Code,
  z = df_init$value,
  text = paste(
    "Country:", df_init$Entity,
    "<br>Year:", df_init$Year,
    "<br>Physicians per 1,000:", round(df_init$value, 2)
  ),
  colorscale = "RdBu",
  reversescale = TRUE,
  zmin = 0,
  zmax = 8,
  marker = list(line = list(color = "white", width = 0.5))
)

# ---------------------------------------------------------
# BUILD FRAMES (ANIMATIONS)
# ---------------------------------------------------------

years <- sort(unique(df_full$Year))

frames <- lapply(years, function(y) {
  
  df_year <- df_full %>% filter(Year == y)
  
  annotation_text <- ""
  
  # Spain 1991
  if (y == 1991) {
    annotation_text <- "<b>Spain Healthcare Expansion (1991)</b><br>
    Spain’s spike reflects earlier growth in medical training and healthcare expansion, 
    leading to a surge of new doctors entering the workforce in the early 1990s."
  }
  
  # Italy 2002
  if (y == 2002) {
    annotation_text <- "<b>Italy Physician Surge (2002)</b><br>
    Around 2002, Italy experienced a structural oversupply of physicians 
    due to decades of high medical school enrollment, leading to one 
    of the highest physician densities globally."
  }
  
  list(
    name = as.character(y),
    
    data = list(
      list(
        locations = df_year$Code,
        z = df_year$value,
        text = paste(
          "Country:", df_year$Entity,
          "<br>Year:", df_year$Year,
          "<br>Physicians per 1,000:", round(df_year$value, 2)
        )
      )
    ),
    
    layout = list(
      annotations = list(
        list(
          text = annotation_text,
          x = 0.5,
          y = 0.12,
          xref = "paper",
          yref = "paper",
          showarrow = FALSE,
          align = "center",
          bgcolor = ifelse(annotation_text == "", "rgba(0,0,0,0)", "rgba(255,255,255,0.95)"),
          bordercolor = "black",
          borderwidth = ifelse(annotation_text == "", 0, 1)
        )
      )
    )
  )
})

fig$x$frames <- frames

# ---------------------------------------------------------
# PLAY BUTTON + SLIDER (FIXED)
# ---------------------------------------------------------

fig <- fig %>%
  layout(
    updatemenus = list(
      list(
        type = "buttons",
        showactive = FALSE,
        x = 0.1,
        y = 0.05,
        xanchor = "left",
        yanchor = "bottom",
        direction = "left",
        
        buttons = list(
          list(
            label = "Play",
            method = "animate",
            args = list(
              NULL,
              list(
                frame = list(duration = 800, redraw = TRUE),
                transition = list(duration = 0),
                fromcurrent = TRUE,
                mode = "immediate"
              )
            )
          )
        )
      )
    ),
    
    sliders = list(
      list(
        active = 0,
        x = 0.1,
        y = 0,
        len = 0.9,
        currentvalue = list(prefix = "Year: "),
        
        steps = lapply(years, function(y) {
          list(
            label = y,
            method = "animate",
            args = list(
              list(as.character(y)),
              list(
                mode = "immediate",
                frame = list(duration = 0, redraw = TRUE),
                transition = list(duration = 0)
              )
            )
          )
        })
      )
    )
  )

# ---------------------------------------------------------
# ENABLE ANIMATION (CRITICAL FIX)
# ---------------------------------------------------------

fig <- fig %>%
  animation_opts(
    frame = 500,        # speed (lower = faster)
    transition = 0,
    redraw = TRUE
  )

# ---------------------------------------------------------
# Layout
# ---------------------------------------------------------

fig <- fig %>%
  layout(
    title = list(
      text = "<b>Global Physician Availability</b><br>Physicians per 1,000 People (1960–2023)",
      x = 0.5
    ),
    geo = list(
      scope = "world",
      projection = list(type = "natural earth")
    ),
    margin = list(r = 0, t = 80, l = 0, b = 0)
  )

# ---------------------------------------------------------
# Show + Save
# ---------------------------------------------------------

fig

saveWidget(fig, "global_doctor_density_map.html")





# Load libraries
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
  ungroup() %>%
  filter(!is.na(value))

# ---------------------------------------------------------
# ADD ANNOTATIONS COLUMN (NEW METHOD)
# ---------------------------------------------------------

df_full <- df_full %>%
  mutate(annotation = case_when(
    Year == 1991 ~ "<b>Spain Healthcare Expansion (1991)</b><br>
    Spain’s spike reflects earlier growth in medical training and healthcare expansion,
    leading to a surge of new doctors entering the workforce in the early 1990s.",
    
    Year == 2002 ~ "<b>Italy Physician Surge (2002)</b><br>
    Around 2002, Italy experienced a structural oversupply of physicians due to decades
    of high medical school enrollment, leading to one of the highest physician densities globally.",
    
    TRUE ~ ""
  ))

# ---------------------------------------------------------
# BUILD ANIMATED MAP (CRITICAL FIX)
# ---------------------------------------------------------

fig <- plot_ly(
  data = df_full,
  type = "choropleth",
  locations = ~Code,
  z = ~value,
  frame = ~Year,   # 🚨 THIS FIXES EVERYTHING
  text = ~paste(
    "Country:", Entity,
    "<br>Year:", Year,
    "<br>Physicians per 1,000:", round(value, 2)
  ),
  hoverinfo = "text",
  colorscale = "RdBu",
  reversescale = TRUE,
  zmin = 0,
  zmax = 8,
  marker = list(line = list(color = "white", width = 0.5))
)

# ---------------------------------------------------------
# ADD DYNAMIC ANNOTATIONS
# ---------------------------------------------------------

years <- sort(unique(df_full$Year))

annotations_list <- lapply(years, function(y) {
  
  text_val <- df_full %>%
    filter(Year == y) %>%
    slice(1) %>%
    pull(annotation)
  
  list(
    text = text_val,
    x = 0.5,
    y = 0.12,
    xref = "paper",
    yref = "paper",
    showarrow = FALSE,
    align = "center",
    bgcolor = ifelse(text_val == "", "rgba(0,0,0,0)", "rgba(255,255,255,0.95)"),
    bordercolor = "black",
    borderwidth = ifelse(text_val == "", 0, 1)
  )
})

# Attach annotations to frames
fig$x$layout$annotations <- annotations_list[[1]]

for (i in seq_along(fig$x$frames)) {
  fig$x$frames[[i]]$layout$annotations <- list(annotations_list[[i]])
}

# ---------------------------------------------------------
# ENABLE ANIMATION (REQUIRED)
# ---------------------------------------------------------

fig <- fig %>%
  animation_opts(
    frame = 500,
    transition = 0,
    redraw = TRUE
  ) %>%
  animation_button(x = 0.1, y = 0.05) %>%
  animation_slider(x = 0.1, len = 0.9)

# ---------------------------------------------------------
# Layout
# ---------------------------------------------------------

fig <- fig %>%
  layout(
    title = list(
      text = "<b>Global Physician Availability</b><br>Physicians per 1,000 People (1960–2023)",
      x = 0.5
    ),
    geo = list(
      scope = "world",
      projection = list(type = "natural earth")
    ),
    margin = list(r = 0, t = 80, l = 0, b = 0)
  )

# ---------------------------------------------------------
# Show + Save
# ---------------------------------------------------------

fig

saveWidget(fig, "global_doctor_density_map.html")







# Load libraries
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
  ungroup() %>%
  filter(!is.na(value))

# ---------------------------------------------------------
# BUILD ANIMATED MAP
# ---------------------------------------------------------

fig <- plot_ly(
  data = df_full,
  type = "choropleth",
  locations = ~Code,
  z = ~value,
  frame = ~Year,
  text = ~paste(
    "Country:", Entity,
    "<br>Year:", Year,
    "<br>Physicians per 1,000:", round(value, 2)
  ),
  hoverinfo = "text",
  colorscale = "RdBu",
  reversescale = TRUE,
  zmin = 0,
  zmax = 8,
  marker = list(line = list(color = "white", width = 0.5))
)

# ---------------------------------------------------------
# ENABLE ANIMATION
# ---------------------------------------------------------

fig <- fig %>%
  animation_opts(
    frame = 500,
    transition = 0,
    redraw = TRUE
  ) %>%
  animation_button(x = 0.1, y = 0.05) %>%
  animation_slider(x = 0.1, len = 0.9)

# ---------------------------------------------------------
# ADD CLEAN, SMALLER ANNOTATIONS
# ---------------------------------------------------------

fig <- fig %>%
  layout(
    title = list(
      text = "<b>Global Physician Availability</b><br>Physicians per 1,000 People (1960–2023)",
      x = 0.5
    ),
    
    geo = list(
      scope = "world",
      projection = list(type = "natural earth")
    ),
    
    annotations = list(
      
      # Spain annotation (smaller + cleaner)
      list(
        text = "<b>Spain Healthcare Expansion (1991)</b><br>
        Spain’s spike reflects earlier growth in medical training and healthcare expansion,
        leading to a surge of new doctors entering the workforce in the early 1990s.",
        x = 0.05,
        y = 0.25,
        xref = "paper",
        yref = "paper",
        showarrow = FALSE,
        align = "left",
        font = list(size = 10),
        bgcolor = "rgba(255,255,255,0.9)",
        bordercolor = "black",
        borderwidth = 1
      ),
      
      # Italy annotation (shortened + cleaner)
      list(
        text = "<b>Italy Physician Surge (2002)</b><br>
        Around 2002, Italy experienced a structural oversupply of physicians due to decades
        of high medical school enrollment.",
        x = 0.05,
        y = 0.12,
        xref = "paper",
        yref = "paper",
        showarrow = FALSE,
        align = "left",
        font = list(size = 10),
        bgcolor = "rgba(255,255,255,0.9)",
        bordercolor = "black",
        borderwidth = 1
      )
      
    ),
    
    margin = list(r = 0, t = 80, l = 0, b = 0)
  )

# ---------------------------------------------------------
# Show + Save
# ---------------------------------------------------------

fig

saveWidget(fig, "global_doctor_density_map.html")








# Load libraries
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
  ungroup() %>%
  filter(!is.na(value))

# ---------------------------------------------------------
# BUILD ANIMATED MAP
# ---------------------------------------------------------

fig <- plot_ly(
  data = df_full,
  type = "choropleth",
  locations = ~Code,
  z = ~value,
  frame = ~Year,
  text = ~paste(
    "Country:", Entity,
    "<br>Year:", Year,
    "<br>Physicians per 1,000:", round(value, 2)
  ),
  hoverinfo = "text",
  colorscale = "RdBu",
  reversescale = TRUE,
  zmin = 0,
  zmax = 8,
  marker = list(line = list(color = "white", width = 0.5))
)

# ---------------------------------------------------------
# ENABLE ANIMATION
# ---------------------------------------------------------

fig <- fig %>%
  animation_opts(
    frame = 500,
    transition = 0,
    redraw = TRUE
  ) %>%
  animation_button(x = 0.1, y = 0.05) %>%
  animation_slider(x = 0.1, len = 0.9)

# ---------------------------------------------------------
# ADD LOWERED, CENTERED ANNOTATIONS (FINAL POSITION)
# ---------------------------------------------------------

fig <- fig %>%
  layout(
    title = list(
      text = "<b>Global Physician Availability</b><br>Physicians per 1,000 People (1960–2023)",
      x = 0.5
    ),
    
    geo = list(
      scope = "world",
      projection = list(type = "natural earth")
    ),
    
    annotations = list(
      
      # Spain (lower again)
      list(
        text = "<b>Spain Healthcare Expansion (1991)</b><br>
        Spain’s spike reflects earlier growth in medical training and healthcare expansion.",
        x = 0.5,
        y = 0.13,   # 👈 lowered again
        xref = "paper",
        yref = "paper",
        showarrow = FALSE,
        align = "center",
        font = list(size = 9),
        bgcolor = "rgba(255,255,255,0.9)",
        bordercolor = "black",
        borderwidth = 1
      ),
      
      # Italy (lower again)
      list(
        text = "<b>Italy Physician Surge (2002)</b><br>
        Structural oversupply due to decades of high medical school enrollment.",
        x = 0.5,
        y = 0.03,   # 👈 lowered again
        xref = "paper",
        yref = "paper",
        showarrow = FALSE,
        align = "center",
        font = list(size = 9),
        bgcolor = "rgba(255,255,255,0.9)",
        bordercolor = "black",
        borderwidth = 1
      )
      
    ),
    
    margin = list(r = 0, t = 80, l = 0, b = 0)
  )

# ---------------------------------------------------------
# Show + Save
# ---------------------------------------------------------

fig

saveWidget(fig, "global_doctor_density_map.html")






# Load libraries
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
  ungroup() %>%
  filter(!is.na(value))

# ---------------------------------------------------------
# BUILD ANIMATED MAP
# ---------------------------------------------------------

fig <- plot_ly(
  data = df_full,
  type = "choropleth",
  locations = ~Code,
  z = ~value,
  frame = ~Year,
  text = ~paste(
    "Country:", Entity,
    "<br>Year:", Year,
    "<br>Physicians per 1,000:", round(value, 2)
  ),
  hoverinfo = "text",
  colorscale = "RdBu",
  reversescale = TRUE,
  zmin = 0,
  zmax = 8,
  marker = list(line = list(color = "white", width = 0.5))
)

# ---------------------------------------------------------
# ENABLE ANIMATION
# ---------------------------------------------------------

fig <- fig %>%
  animation_opts(
    frame = 500,
    transition = 0,
    redraw = TRUE
  ) %>%
  animation_button(x = 0.1, y = 0.05) %>%
  animation_slider(x = 0.1, len = 0.9)

# ---------------------------------------------------------
# ADD FINAL ANNOTATIONS (UPDATED TEXT)
# ---------------------------------------------------------

fig <- fig %>%
  layout(
    title = list(
      text = "<b>Global Physician Availability</b><br>Physicians per 1,000 People (1960–2023)",
      x = 0.5
    ),
    
    geo = list(
      scope = "world",
      projection = list(type = "natural earth")
    ),
    
    annotations = list(
      
      # Spain annotation (updated text)
      list(
        text = "<b>Spain Healthcare Expansion (1991)</b><br>
        Spain’s spike reflects earlier growth in medical training and healthcare expansion,
        leading to a surge of new doctors entering the workforce in the early 1990s.",
        x = 0.5,
        y = 0.11,
        xref = "paper",
        yref = "paper",
        showarrow = FALSE,
        align = "center",
        font = list(size = 9),
        bgcolor = "rgba(255,255,255,0.9)",
        bordercolor = "black",
        borderwidth = 1
      ),
      
      # Italy annotation (updated text)
      list(
        text = "<b>Italy Physician Surge (2002)</b><br>
        Around 2002, Italy experienced a structural oversupply of physicians due to decades
        of high medical school enrollment.",
        x = 0.5,
        y = 0.01,
        xref = "paper",
        yref = "paper",
        showarrow = FALSE,
        align = "center",
        font = list(size = 9),
        bgcolor = "rgba(255,255,255,0.9)",
        bordercolor = "black",
        borderwidth = 1
      )
      
    ),
    
    margin = list(r = 0, t = 80, l = 0, b = 0)
  )

# ---------------------------------------------------------
# Show + Save
# ---------------------------------------------------------

fig

saveWidget(fig, "global_doctor_density_map.html")






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
# Fill Missing Years (forward fill only real observations)
# ---------------------------------------------------------
all_years <- 1960:2023

df_full <- df %>%
  group_by(Entity, Code) %>%
  complete(Year = all_years) %>%
  arrange(Entity, Year) %>%
  mutate(value = zoo::na.locf(`Physicians (per 1,000 people)`, na.rm = FALSE)) %>%
  ungroup()

# ---------------------------------------------------------
# BUILD ANIMATED MAP
# ---------------------------------------------------------
fig <- plot_ly(
  data = df_full,
  type = "choropleth",
  locations = ~Code,
  z = ~value,
  frame = ~Year,
  
  text = ~paste(
    "Country:", Entity,
    "<br>Year:", Year,
    "<br>Physicians per 1,000:", round(value, 2)
  ),
  hoverinfo = "text",
  
  # ✅ CUSTOM RED → BLUE SCALE (keeps urgency storytelling)
  colorscale = list(
    c(0, "#67000d"),   # dark red (low)
    c(0.2, "#cb181d"),
    c(0.4, "#fb6a4a"),
    c(0.6, "#fcae91"),
    c(0.8, "#deebf7"),
    c(1, "#3182bd")    # blue (high)
  ),
  
  zmin = 0,
  zmax = 8,
  
  colorbar = list(
    title = "Physicians<br>per 1,000"
  ),
  
  marker = list(
    line = list(color = "white", width = 0.5)
  )
)

# ---------------------------------------------------------
# ANIMATION SETTINGS
# ---------------------------------------------------------
fig <- fig %>%
  animation_opts(
    frame = 600,
    transition = 0,
    redraw = TRUE
  ) %>%
  animation_button(
    x = 0.1,
    y = 0.05,
    xanchor = "left",
    yanchor = "bottom"
  ) %>%
  animation_slider(
    x = 0.1,
    len = 0.9
  )

# ---------------------------------------------------------
# LAYOUT + ANNOTATIONS
# ---------------------------------------------------------
fig <- fig %>%
  layout(
    title = list(
      text = paste0(
        "<b>Global Physician Availability</b><br>",
        "Physicians per 1,000 People (1960–2023)"
      ),
      x = 0.5
    ),
    
    geo = list(
      scope = "world",
      projection = list(type = "natural earth")
    ),
    
    annotations = list(
      
      # Spain annotation
      list(
        text = "<b>Spain Healthcare Expansion (1991)</b><br>
        Spain’s spike reflects earlier growth in medical training and healthcare expansion,
        leading to a surge of new doctors entering the workforce in the early 1990s.",
        x = 0.5,
        y = 0.11,
        xref = "paper",
        yref = "paper",
        showarrow = FALSE,
        align = "center",
        font = list(size = 9),
        bgcolor = "rgba(255,255,255,0.9)",
        bordercolor = "black",
        borderwidth = 1
      ),
      
      # Italy annotation
      list(
        text = "<b>Italy Physician Surge (2002)</b><br>
        Around 2002, Italy experienced a structural oversupply of physicians due to decades
        of high medical school enrollment.",
        x = 0.5,
        y = 0.01,
        xref = "paper",
        yref = "paper",
        showarrow = FALSE,
        align = "center",
        font = list(size = 9),
        bgcolor = "rgba(255,255,255,0.9)",
        bordercolor = "black",
        borderwidth = 1
      )
    ),
    
    margin = list(r = 0, t = 90, l = 0, b = 0)
  )

# ---------------------------------------------------------
# SHOW + SAVE
# ---------------------------------------------------------
fig

saveWidget(fig, "global_doctor_density_map.html")




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
# Fill Missing Years (forward fill only real observations)
# ---------------------------------------------------------
all_years <- 1960:2023

df_full <- df %>%
  group_by(Entity, Code) %>%
  complete(Year = all_years) %>%
  arrange(Entity, Year) %>%
  mutate(value = zoo::na.locf(`Physicians (per 1,000 people)`, na.rm = FALSE)) %>%
  ungroup()

# ---------------------------------------------------------
# BUILD ANIMATED MAP
# ---------------------------------------------------------
fig <- plot_ly(
  data = df_full,
  type = "choropleth",
  locations = ~Code,
  z = ~value,
  frame = ~Year,
  
  text = ~paste(
    "Country:", Entity,
    "<br>Year:", Year,
    "<br>Physicians per 1,000:", round(value, 2)
  ),
  hoverinfo = "text",
  
  # ✅ COLORBLIND-FRIENDLY SCALE
  colorscale = "Viridis",
  reversescale = FALSE,
  
  # ✅ FIXED SCALE RANGE
  zmin = 0,
  zmax = 8,
  
  # ✅ COLORBAR LABEL
  colorbar = list(
    title = "Physicians<br>per 1,000"
  ),
  
  marker = list(
    line = list(color = "white", width = 0.5)
  )
)

# ---------------------------------------------------------
# ANIMATION SETTINGS
# ---------------------------------------------------------
fig <- fig %>%
  animation_opts(
    frame = 600,
    transition = 0,
    redraw = TRUE
  ) %>%
  animation_button(
    x = 0.1,
    y = 0.05,
    xanchor = "left",
    yanchor = "bottom"
  ) %>%
  animation_slider(
    x = 0.1,
    len = 0.9
  )

# ---------------------------------------------------------
# LAYOUT + ANNOTATIONS
# ---------------------------------------------------------
fig <- fig %>%
  layout(
    title = list(
      text = paste0(
        "<b>Global Physician Availability</b><br>",
        "Physicians per 1,000 People (1960–2023)<br>",
        "<i>Darker colors indicate lower physician availability</i>"
      ),
      x = 0.5
    ),
    
    geo = list(
      scope = "world",
      projection = list(type = "natural earth")
    ),
    
    annotations = list(
      
      # Spain annotation
      list(
        text = "<b>Spain Healthcare Expansion (1991)</b><br>
        Spain’s spike reflects earlier growth in medical training and healthcare expansion,
        leading to a surge of new doctors entering the workforce in the early 1990s.",
        x = 0.5,
        y = 0.11,
        xref = "paper",
        yref = "paper",
        showarrow = FALSE,
        align = "center",
        font = list(size = 9),
        bgcolor = "rgba(255,255,255,0.9)",
        bordercolor = "black",
        borderwidth = 1
      ),
      
      # Italy annotation
      list(
        text = "<b>Italy Physician Surge (2002)</b><br>
        Around 2002, Italy experienced a structural oversupply of physicians due to decades
        of high medical school enrollment.",
        x = 0.5,
        y = 0.01,
        xref = "paper",
        yref = "paper",
        showarrow = FALSE,
        align = "center",
        font = list(size = 9),
        bgcolor = "rgba(255,255,255,0.9)",
        bordercolor = "black",
        borderwidth = 1
      )
    ),
    
    margin = list(r = 0, t = 90, l = 0, b = 0)
  )

# ---------------------------------------------------------
# SHOW + SAVE
# ---------------------------------------------------------
fig

saveWidget(fig, "global_doctor_density_map.html")









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
# Fill Missing Years (forward fill ONLY real values)
# ---------------------------------------------------------
all_years <- 1960:2023

df_full <- df %>%
  group_by(Entity, Code) %>%
  complete(Year = all_years) %>%
  arrange(Entity, Year) %>%
  mutate(value = zoo::na.locf(`Physicians (per 1,000 people)`, na.rm = FALSE)) %>%
  ungroup()

# ---------------------------------------------------------
# HANDLE MISSING DATA (show as gray)
# ---------------------------------------------------------
df_full <- df_full %>%
  mutate(value_plot = ifelse(is.na(value), -1, value))

# ---------------------------------------------------------
# BUILD ANIMATED MAP
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
  
  # Colorblind-friendly + gray for missing
  colorscale = list(
    c(0, "#d9d9d9"),   # gray = missing
    c(0.001, "#440154"),
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
# ANIMATION SETTINGS
# ---------------------------------------------------------
fig <- fig %>%
  animation_opts(
    frame = 600,
    transition = 0,
    redraw = TRUE
  ) %>%
  animation_button(
    x = 0.1,
    y = 0.05,
    xanchor = "left",
    yanchor = "bottom"
  ) %>%
  animation_slider(
    x = 0.1,
    len = 0.9
  )

# ---------------------------------------------------------
# LAYOUT + ANNOTATIONS
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
      
      # Spain annotation
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
      
      # Italy annotation
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
      
      # Missing data note
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
    
    margin = list(r = 0, t = 100, l = 0, b = 0)
  )

# ---------------------------------------------------------
# SHOW + SAVE
# ---------------------------------------------------------
fig

saveWidget(fig, "global_doctor_density_map.html")






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
# Fill Missing Years (forward fill ONLY real values)
# ---------------------------------------------------------
all_years <- 1960:2023

df_full <- df %>%
  group_by(Entity, Code) %>%
  complete(Year = all_years) %>%
  arrange(Entity, Year) %>%
  mutate(value = zoo::na.locf(`Physicians (per 1,000 people)`, na.rm = FALSE)) %>%
  ungroup()

# ---------------------------------------------------------
# HANDLE MISSING DATA (show as gray)
# ---------------------------------------------------------
df_full <- df_full %>%
  mutate(value_plot = ifelse(is.na(value), -1, value))

# ---------------------------------------------------------
# BUILD ANIMATED MAP
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
  
  # ✅ PURE VIRIDIS (BEST PRACTICE)
  colorscale = list(
    c(0, "#d9d9d9"),   # gray for missing
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
# ANIMATION SETTINGS
# ---------------------------------------------------------
fig <- fig %>%
  animation_opts(
    frame = 600,
    transition = 0,
    redraw = TRUE
  ) %>%
  animation_button(
    x = 0.1,
    y = 0.05,
    xanchor = "left",
    yanchor = "bottom"
  ) %>%
  animation_slider(
    x = 0.1,
    len = 0.9
  )

# ---------------------------------------------------------
# LAYOUT + ANNOTATIONS
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
      
      # Spain annotation
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
      
      # Italy annotation
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
      
      # Missing data note
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
    
    margin = list(r = 0, t = 100, l = 0, b = 0)
  )

# ---------------------------------------------------------
# SHOW + SAVE
# ---------------------------------------------------------
fig

saveWidget(fig, "global_doctor_density_map.html")








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














# ---------------------------
# Load Libraries
# ---------------------------
library(ggplot2)
library(dplyr)
library(maps)
library(readr)

# ---------------------------
# Load Walmart Dataset
# ---------------------------
walmart <- read_csv("walmart.csv")

# Ensure numeric coordinates
walmart$lat <- as.numeric(walmart$lat)
walmart$lng <- as.numeric(walmart$lng)

# ---------------------------
# Identify 2010 Stores
# ---------------------------
walmart$year2010 <- walmart$year == 2010

# ---------------------------
# Define U.S. Regions
# ---------------------------
west_states <- c("washington","oregon","california","nevada","idaho",
                 "montana","wyoming","utah","arizona","colorado","new mexico")

midwest_states <- c("north dakota","south dakota","nebraska","kansas",
                    "minnesota","iowa","missouri","wisconsin",
                    "illinois","indiana","michigan","ohio")

south_states <- c("texas","oklahoma","arkansas","louisiana",
                  "kentucky","tennessee","mississippi","alabama",
                  "georgia","florida","south carolina","north carolina",
                  "virginia","west virginia")

north_states <- c("maine","new hampshire","vermont","massachusetts",
                  "rhode island","connecticut","new york",
                  "pennsylvania","new jersey","maryland","delaware")

# ---------------------------
# Load U.S. Map Data
# ---------------------------
us_map <- map_data("state")

# ---------------------------
# Function to Plot Each Region
# ---------------------------
plot_region <- function(region_states, title){
  
  region_map <- us_map %>% 
    filter(region %in% region_states)
  
  ggplot() +
    geom_polygon(
      data = region_map,
      aes(x = long, y = lat, group = group),
      fill = "gray95",
      color = "white"
    ) +
    
    # All stores
    geom_point(
      data = walmart,
      aes(x = lng, y = lat),
      color = "blue",
      alpha = 0.3,
      size = 0.7
    ) +
    
    # 2010 stores
    geom_point(
      data = walmart %>% filter(year2010),
      aes(x = lng, y = lat),
      color = "red",
      size = 2
    ) +
    
    coord_quickmap(
      xlim = range(region_map$long),
      ylim = range(region_map$lat)
    ) +
    
    labs(
      title = paste("Walmart Store Locations -", title),
      subtitle = "Red = Opened in 2010",
      x = "Longitude",
      y = "Latitude"
    ) +
    
    theme_minimal()
}

# ---------------------------
# Create the Four Maps
# ---------------------------
west_map <- plot_region(west_states, "West")
midwest_map <- plot_region(midwest_states, "Midwest")
south_map <- plot_region(south_states, "South")
north_map <- plot_region(north_states, "North")

# ---------------------------
# Display Maps
# ---------------------------
print(west_map)
print(midwest_map)
print(south_map)
print(north_map)


# ---------------------------
# Load Libraries
# ---------------------------
library(ggplot2)
library(dplyr)
library(readr)
library(maps)
library(patchwork)

# ---------------------------
# Load Data
# ---------------------------
walmart <- read_csv("walmart.csv")

# Ensure numeric coordinates
walmart$lat <- as.numeric(walmart$lat)
walmart$lng <- as.numeric(walmart$lng)

# Identify 2010 stores
walmart$year2010 <- walmart$year == 2010

# ---------------------------
# Load US Map
# ---------------------------
us_map <- map_data("state")

# ---------------------------
# Define Regions
# ---------------------------
west_states <- c("washington","oregon","california","nevada","idaho",
                 "montana","wyoming","utah","arizona","colorado","new mexico")

midwest_states <- c("north dakota","south dakota","nebraska","kansas",
                    "minnesota","iowa","missouri","wisconsin",
                    "illinois","indiana","michigan","ohio")

south_states <- c("texas","oklahoma","arkansas","louisiana",
                  "kentucky","tennessee","mississippi","alabama",
                  "georgia","florida","south carolina","north carolina",
                  "virginia","west virginia")

north_states <- c("maine","new hampshire","vermont","massachusetts",
                  "rhode island","connecticut","new york",
                  "pennsylvania","new jersey","maryland","delaware")

# ---------------------------
# Function to Create Map
# ---------------------------
create_region_map <- function(region_states, title){
  
  region_highlight <- us_map %>% 
    mutate(region_group = ifelse(region %in% region_states, "highlight", "other"))
  
  ggplot() +
    
    # Base US map
    geom_polygon(
      data = region_highlight,
      aes(x = long, y = lat, group = group, fill = region_group),
      color = "white",
      size = 0.2
    ) +
    
    scale_fill_manual(
      values = c("highlight" = "#FFD34E", "other" = "gray90"),
      guide = "none"
    ) +
    
    # All stores
    geom_point(
      data = walmart,
      aes(x = lng, y = lat),
      color = "blue",
      alpha = 0.25,
      size = 0.7
    ) +
    
    # 2010 stores
    geom_point(
      data = walmart %>% filter(year2010),
      aes(x = lng, y = lat),
      color = "red",
      size = 1.5
    ) +
    
    coord_fixed(1.3) +
    
    labs(
      title = title,
      subtitle = "Blue = Existing Stores | Red = Opened in 2010",
      x = "Longitude",
      y = "Latitude"
    ) +
    
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank()
    )
}

# ---------------------------
# Create Maps
# ---------------------------
west_map <- create_region_map(west_states, "West Region")
midwest_map <- create_region_map(midwest_states, "Midwest Region")
south_map <- create_region_map(south_states, "South Region")
north_map <- create_region_map(north_states, "North Region")

# ---------------------------
# Combine Into One Panel
# ---------------------------
regional_panel <- (west_map | midwest_map) /
  (south_map | north_map)

# Display panel
regional_panel

full_us_map <- ggplot() +
  
  geom_polygon(
    data = us_map,
    aes(x = long, y = lat, group = group),
    fill = "gray95",
    color = "white"
  ) +
  
  # All stores
  geom_point(
    data = walmart,
    aes(x = lng, y = lat),
    color = "blue",
    alpha = 0.25,
    size = 0.7
  ) +
  
  # 2010 stores
  geom_point(
    data = walmart %>% filter(year2010),
    aes(x = lng, y = lat),
    color = "red",
    size = 2
  ) +
  
  coord_fixed(1.3) +
  
  labs(
    title = "Walmart Store Locations (Highlighting 2010 Openings)",
    subtitle = "Blue = Existing Stores | Red = Opened in 2010",
    x = "Longitude",
    y = "Latitude"
  ) +
  
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )


library(ggplot2)
library(dplyr)
library(readr)
library(maps)
library(patchwork)

# Load data
walmart <- read_csv("walmart.csv")

walmart$lat <- as.numeric(walmart$lat)
walmart$lng <- as.numeric(walmart$lng)

# 2010 stores
stores2010 <- walmart %>% filter(year == 2010)

# US map
us_map <- map_data("state")

# -------------------
# REGION BOUNDARIES
# -------------------

west_bounds <- c(-125, -102, 31, 49)
midwest_bounds <- c(-105, -80, 36, 50)
south_bounds <- c(-105, -75, 24, 37)
north_bounds <- c(-80, -65, 38, 47)

# -------------------
# FUNCTION TO MAKE MAP
# -------------------

make_map <- function(xmin,xmax,ymin,ymax,title){
  
  ggplot() +
    
    geom_polygon(
      data = us_map,
      aes(long, lat, group = group),
      fill="gray95",
      color="white"
    ) +
    
    geom_point(
      data = walmart,
      aes(lng, lat),
      color="blue",
      alpha=.25,
      size=.7
    ) +
    
    geom_point(
      data = stores2010,
      aes(lng, lat),
      color="red",
      size=2
    ) +
    
    coord_quickmap(
      xlim=c(xmin,xmax),
      ylim=c(ymin,ymax)
    ) +
    
    labs(
      title = title,
      subtitle = "Blue = Existing Stores | Red = Opened in 2010"
    ) +
    
    theme_void() +
    theme(
      plot.title = element_text(size=16, face="bold"),
      plot.subtitle = element_text(size=11)
    )
  
}

# -------------------
# CREATE REGIONAL MAPS
# -------------------

west <- make_map(west_bounds[1],west_bounds[2],west_bounds[3],west_bounds[4],"West")

midwest <- make_map(midwest_bounds[1],midwest_bounds[2],midwest_bounds[3],midwest_bounds[4],"Midwest")

south <- make_map(south_bounds[1],south_bounds[2],south_bounds[3],south_bounds[4],"South")

north <- make_map(north_bounds[1],north_bounds[2],north_bounds[3],north_bounds[4],"North")

# -------------------
# COMBINE PANEL
# -------------------

(west | midwest) /
  (south | north)


library(ggplot2)
library(dplyr)
library(readr)
library(maps)
library(patchwork)

# Load data
walmart <- read_csv("walmart.csv")

walmart$lat <- as.numeric(walmart$lat)
walmart$lng <- as.numeric(walmart$lng)

# ---------------------------
# Define Regions by Coordinates
# ---------------------------

west <- walmart %>%
  filter(lng < -102)

midwest <- walmart %>%
  filter(lng >= -102 & lng < -85 & lat > 36)

south <- walmart %>%
  filter(lat <= 36)

north <- walmart %>%
  filter(lng >= -85 & lat > 36)

# 2010 stores in each region
west2010 <- west %>% filter(year == 2010)
midwest2010 <- midwest %>% filter(year == 2010)
south2010 <- south %>% filter(year == 2010)
north2010 <- north %>% filter(year == 2010)

# US map
us_map <- map_data("state")

# ---------------------------
# Plot Function
# ---------------------------

make_region_plot <- function(data, data2010, title){
  
  ggplot() +
    
    geom_polygon(
      data = us_map,
      aes(long, lat, group = group),
      fill = "gray95",
      color = "white"
    ) +
    
    geom_point(
      data = data,
      aes(lng, lat),
      color = "blue",
      alpha = .35,
      size = .8
    ) +
    
    geom_point(
      data = data2010,
      aes(lng, lat),
      color = "red",
      size = 2
    ) +
    
    coord_fixed(1.3) +
    
    labs(
      title = title,
      subtitle = "Blue = Existing Stores | Red = Opened in 2010"
    ) +
    
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank()
    )
  
}

# ---------------------------
# Create Maps
# ---------------------------

west_map <- make_region_plot(west, west2010, "West Region")
midwest_map <- make_region_plot(midwest, midwest2010, "Midwest Region")
south_map <- make_region_plot(south, south2010, "South Region")
north_map <- make_region_plot(north, north2010, "North Region")

# ---------------------------
# Combine Panel
# ---------------------------

(west_map | midwest_map) /
  (south_map | north_map)



library(ggplot2)
library(dplyr)
library(readr)
library(maps)
library(patchwork)

# Load data
walmart <- read_csv("walmart.csv")

walmart$lat <- as.numeric(walmart$lat)
walmart$lng <- as.numeric(walmart$lng)

# 2010 stores
stores2010 <- walmart %>% filter(year == 2010)

# US map
us_map <- map_data("state")

# -------------------
# REGION BOUNDARIES
# -------------------

west_bounds <- c(-125, -102, 31, 49)
midwest_bounds <- c(-102, -85, 36, 49)
south_bounds <- c(-105, -75, 24, 37)
north_bounds <- c(-85, -67, 37, 47)

# -------------------
# FUNCTION TO MAKE MAP
# -------------------

make_map <- function(xmin,xmax,ymin,ymax,title){
  
  ggplot() +
    
    geom_polygon(
      data = us_map,
      aes(long, lat, group = group),
      fill="gray95",
      color="white"
    ) +
    
    geom_point(
      data = walmart,
      aes(lng, lat),
      color="blue",
      alpha=.25,
      size=.7
    ) +
    
    geom_point(
      data = stores2010,
      aes(lng, lat),
      color="red",
      size=2
    ) +
    
    coord_quickmap(
      xlim=c(xmin,xmax),
      ylim=c(ymin,ymax)
    ) +
    
    labs(
      title = title,
      subtitle = "Blue = Existing Stores | Red = Opened in 2010"
    ) +
    
    theme_void() +
    theme(
      plot.title = element_text(size=16, face="bold"),
      plot.subtitle = element_text(size=11)
    )
  
}

# -------------------
# CREATE MAPS
# -------------------

west <- make_map(west_bounds[1],west_bounds[2],west_bounds[3],west_bounds[4],"West")

midwest <- make_map(midwest_bounds[1],midwest_bounds[2],midwest_bounds[3],midwest_bounds[4],"Midwest")

south <- make_map(south_bounds[1],south_bounds[2],south_bounds[3],south_bounds[4],"South")

north <- make_map(north_bounds[1],north_bounds[2],north_bounds[3],north_bounds[4],"North")

# -------------------
# PANEL
# -------------------

(west | midwest) /
  (south | north)


library(ggplot2)
library(dplyr)
library(readr)
library(maps)
library(patchwork)

# Load data
walmart <- read_csv("walmart.csv")

walmart$lat <- as.numeric(walmart$lat)
walmart$lng <- as.numeric(walmart$lng)

stores2010 <- walmart %>% filter(year == 2010)

# US map
us_map <- map_data("state")

# -------------------
# REGION BOUNDARIES
# -------------------

west_bounds <- c(-125, -104, 31, 49)
midwest_bounds <- c(-104, -85, 36, 49)
south_bounds <- c(-105, -75, 24, 37)
north_bounds <- c(-85, -67, 37, 47)

# -------------------
# FUNCTION
# -------------------

make_map <- function(xmin,xmax,ymin,ymax,title){
  
  ggplot() +
    
    geom_polygon(
      data = us_map,
      aes(long, lat, group = group),
      fill="gray95",
      color="white"
    ) +
    
    geom_point(
      data = walmart,
      aes(lng, lat),
      color="blue",
      alpha=.25,
      size=.7
    ) +
    
    geom_point(
      data = stores2010,
      aes(lng, lat),
      color="red",
      size=2
    ) +
    
    coord_quickmap(
      xlim=c(xmin,xmax),
      ylim=c(ymin,ymax)
    ) +
    
    labs(
      title = title,
      subtitle = "Blue = Existing Stores | Red = Opened in 2010"
    ) +
    
    theme_void() +
    theme(
      plot.title = element_text(size=16, face="bold"),
      plot.subtitle = element_text(size=11)
    )
  
}

# -------------------
# CREATE MAPS
# -------------------

west <- make_map(west_bounds[1],west_bounds[2],west_bounds[3],west_bounds[4],"West")

midwest <- make_map(midwest_bounds[1],midwest_bounds[2],midwest_bounds[3],midwest_bounds[4],"Midwest")

south <- make_map(south_bounds[1],south_bounds[2],south_bounds[3],south_bounds[4],"South")

north <- make_map(north_bounds[1],north_bounds[2],north_bounds[3],north_bounds[4],"North")

# -------------------
# PANEL
# -------------------

(west | midwest) /
  (south | north)


library(ggplot2)
library(dplyr)
library(readr)
library(maps)
library(patchwork)

# Load data
walmart <- read_csv("walmart.csv")

walmart$lat <- as.numeric(walmart$lat)
walmart$lng <- as.numeric(walmart$lng)

stores2010 <- walmart %>% filter(year == 2010)

# US map
us_map <- map_data("state")

# -------------------
# REGION BOUNDARIES
# -------------------

west_midwest_bounds <- c(-125, -85, 31, 49)
south_bounds <- c(-105, -75, 24, 37)
north_bounds <- c(-85, -67, 37, 47)

# -------------------
# FUNCTION
# -------------------

make_map <- function(xmin,xmax,ymin,ymax,title){
  
  ggplot() +
    
    geom_polygon(
      data = us_map,
      aes(long, lat, group = group),
      fill="gray95",
      color="white"
    ) +
    
    geom_point(
      data = walmart,
      aes(lng, lat),
      color="blue",
      alpha=.25,
      size=.7
    ) +
    
    geom_point(
      data = stores2010,
      aes(lng, lat),
      color="red",
      size=2
    ) +
    
    coord_quickmap(
      xlim=c(xmin,xmax),
      ylim=c(ymin,ymax)
    ) +
    
    labs(
      title = title,
      subtitle = "Blue = Existing Stores | Red = Opened in 2010"
    ) +
    
    theme_void() +
    theme(
      plot.title = element_text(size=16, face="bold"),
      plot.subtitle = element_text(size=11)
    )
  
}

# -------------------
# CREATE MAPS
# -------------------

west_midwest <- make_map(
  west_midwest_bounds[1],
  west_midwest_bounds[2],
  west_midwest_bounds[3],
  west_midwest_bounds[4],
  "West / Midwest"
)

south <- make_map(
  south_bounds[1],
  south_bounds[2],
  south_bounds[3],
  south_bounds[4],
  "South"
)

north <- make_map(
  north_bounds[1],
  north_bounds[2],
  north_bounds[3],
  north_bounds[4],
  "North"
)

# -------------------
# PANEL (3 maps)
# -------------------

west_midwest /
  (south | north)


library(ggplot2)
library(dplyr)
library(readr)
library(maps)
library(patchwork)

# Load data
walmart <- read_csv("walmart.csv")

walmart$lat <- as.numeric(walmart$lat)
walmart$lng <- as.numeric(walmart$lng)

stores2010 <- walmart %>% filter(year == 2010)

# Map data
us_map <- map_data("state")

# -------------------
# REGION BOUNDARIES
# -------------------

west_midwest_bounds <- c(-125, -85, 31, 49)
south_bounds <- c(-105, -75, 24, 37)
north_bounds <- c(-85, -67, 37, 47)

# -------------------
# MAP FUNCTION
# -------------------

make_map <- function(xmin,xmax,ymin,ymax,title){
  
  ggplot() +
    
    geom_polygon(
      data = us_map,
      aes(long, lat, group = group),
      fill="gray95",
      color="white"
    ) +
    
    geom_point(
      data = walmart,
      aes(lng, lat),
      color="blue",
      alpha=.25,
      size=.7
    ) +
    
    geom_point(
      data = stores2010,
      aes(lng, lat),
      color="red",
      size=2
    ) +
    
    coord_quickmap(
      xlim=c(xmin,xmax),
      ylim=c(ymin,ymax)
    ) +
    
    labs(
      title = title,
      subtitle = "Blue = Existing Stores | Red = Opened in 2010"
    ) +
    
    theme_void() +
    theme(
      plot.title = element_text(size=16, face="bold"),
      plot.subtitle = element_text(size=11)
    )
  
}

# -------------------
# CREATE MAPS
# -------------------

west_midwest <- make_map(
  -125, -85, 31, 49,
  "West / Midwest"
)

south <- make_map(
  -105, -75, 24, 37,
  "South"
)

north <- make_map(
  -85, -67, 37, 47,
  "North"
)

# Empty plot for balance
empty <- plot_spacer()

# -------------------
# PANEL
# -------------------

(west_midwest | empty) /
  (south | north)


# ---------------------------
# Load Libraries
# ---------------------------
library(ggplot2)
library(dplyr)
library(readr)
library(maps)
library(patchwork)

# ---------------------------
# Load Data
# ---------------------------
walmart <- read_csv("walmart.csv")

# Ensure coordinates are numeric
walmart$lat <- as.numeric(walmart$lat)
walmart$lng <- as.numeric(walmart$lng)

# Filter 2010 stores
stores2010 <- walmart %>%
  filter(year == 2010)

# ---------------------------
# Load US Map
# ---------------------------
us_map <- map_data("state")

# ---------------------------
# Map Function
# ---------------------------
make_map <- function(xmin, xmax, ymin, ymax, title){
  
  ggplot() +
    
    geom_polygon(
      data = us_map,
      aes(long, lat, group = group),
      fill = "gray95",
      color = "white"
    ) +
    
    geom_point(
      data = walmart,
      aes(lng, lat),
      color = "blue",
      alpha = 0.25,
      size = 0.7
    ) +
    
    geom_point(
      data = stores2010,
      aes(lng, lat),
      color = "red",
      size = 2
    ) +
    
    coord_quickmap(
      xlim = c(xmin, xmax),
      ylim = c(ymin, ymax)
    ) +
    
    labs(
      title = title,
      subtitle = "Blue = Existing Stores | Red = Opened in 2010"
    ) +
    
    theme_void() +
    
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 11)
    )
}

# ---------------------------
# Create Region Maps
# ---------------------------

west_midwest <- make_map(
  -125, -85, 31, 49,
  "West / Midwest"
)

south <- make_map(
  -105, -75, 24, 37,
  "South"
)

north <- make_map(
  -85, -67, 37, 47,
  "North"
)

# ---------------------------
# Create Empty Spacer
# ---------------------------
empty <- plot_spacer()

# ---------------------------
# Combine Maps (Centered Layout)
# ---------------------------

final_map <- (empty | west_midwest | empty) /
  (south | empty | north)

final_map


# ---------------------------
# Load Libraries
# ---------------------------
library(ggplot2)
library(dplyr)
library(readr)
library(maps)
library(patchwork)

# ---------------------------
# Load Walmart Data
# ---------------------------
walmart <- read_csv("walmart.csv")

# Ensure numeric coordinates
walmart$lat <- as.numeric(walmart$lat)
walmart$lng <- as.numeric(walmart$lng)

# Filter 2010 stores
stores2010 <- walmart %>%
  filter(year == 2010)

# ---------------------------
# Load US Map
# ---------------------------
us_map <- map_data("state")

# ---------------------------
# Map Function
# ---------------------------
make_map <- function(xmin, xmax, ymin, ymax, title){
  
  ggplot() +
    
    geom_polygon(
      data = us_map,
      aes(long, lat, group = group),
      fill = "gray95",
      color = "white"
    ) +
    
    geom_point(
      data = walmart,
      aes(lng, lat),
      color = "blue",
      alpha = 0.25,
      size = 0.7
    ) +
    
    geom_point(
      data = stores2010,
      aes(lng, lat),
      color = "red",
      size = 2
    ) +
    
    coord_quickmap(
      xlim = c(xmin, xmax),
      ylim = c(ymin, ymax)
    ) +
    
    labs(
      title = title,
      subtitle = "Blue = Existing Stores | Red = Opened in 2010"
    ) +
    
    theme_void() +
    
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 11)
    )
}

# ---------------------------
# Create Region Maps
# ---------------------------

west_midwest <- make_map(
  -125, -85, 31, 49,
  "West / Midwest"
)

south <- make_map(
  -105, -75, 24, 37,
  "South"
)

north <- make_map(
  -85, -67, 37, 47,
  "North"
)

# ---------------------------
# Combine Maps
# ---------------------------

final_map <- west_midwest /
  (south | north)

final_map



library(ggplot2)
library(dplyr)
library(readr)
library(maps)
library(patchwork)

# Load data
walmart <- read_csv("walmart.csv")

walmart$lat <- as.numeric(walmart$lat)
walmart$lng <- as.numeric(walmart$lng)

# 2010 stores
stores2010 <- walmart %>% filter(year == 2010)

# US map
us_map <- map_data("state")

# -------------------
# REGION BOUNDARIES
# -------------------

west_bounds <- c(-125, -102, 31, 49)
midwest_bounds <- c(-102, -85, 36, 49)
south_bounds <- c(-105, -75, 24, 37)
north_bounds <- c(-85, -67, 37, 47)

# -------------------
# FUNCTION TO MAKE MAP
# -------------------

make_map <- function(xmin,xmax,ymin,ymax,title){
  
  ggplot() +
    
    geom_polygon(
      data = us_map,
      aes(long, lat, group = group),
      fill="gray95",
      color="white"
    ) +
    
    geom_point(
      data = walmart,
      aes(lng, lat),
      color="blue",
      alpha=.25,
      size=.7
    ) +
    
    geom_point(
      data = stores2010,
      aes(lng, lat),
      color="red",
      size=2
    ) +
    
    coord_quickmap(
      xlim=c(xmin,xmax),
      ylim=c(ymin,ymax)
    ) +
    
    labs(
      title = title,
      subtitle = "Blue = Existing Stores | Red = Opened in 2010"
    ) +
    
    theme_void() +
    
    theme(
      plot.title = element_text(size=16, face="bold"),
      plot.subtitle = element_text(size=11)
    )
}

# -------------------
# CREATE MAPS
# -------------------

# Remove titles for these two
west <- make_map(west_bounds[1],west_bounds[2],west_bounds[3],west_bounds[4],"")

midwest <- make_map(midwest_bounds[1],midwest_bounds[2],midwest_bounds[3],midwest_bounds[4],"")

south <- make_map(south_bounds[1],south_bounds[2],south_bounds[3],south_bounds[4],"South")

north <- make_map(north_bounds[1],north_bounds[2],north_bounds[3],north_bounds[4],"North")

# -------------------
# PANEL
# -------------------

(west | midwest) /
  (south | north) +
  plot_annotation(
    title = "West / Midwest",
    theme = theme(
      plot.title = element_text(size = 20, face = "bold", hjust = 0.5)
    )
  )


library(ggplot2)
library(dplyr)
library(readr)
library(maps)
library(patchwork)

# Load data
walmart <- read_csv("walmart.csv")

walmart$lat <- as.numeric(walmart$lat)
walmart$lng <- as.numeric(walmart$lng)

# 2010 stores
stores2010 <- walmart %>% filter(year == 2010)

# US map
us_map <- map_data("state")

# -------------------
# REGION BOUNDARIES
# -------------------

west_bounds <- c(-125, -102, 31, 49)
midwest_bounds <- c(-102, -85, 36, 49)
south_bounds <- c(-105, -75, 24, 37)
north_bounds <- c(-85, -67, 37, 47)

# -------------------
# FUNCTION TO MAKE MAP
# -------------------

make_map <- function(xmin,xmax,ymin,ymax,title,subtitle=TRUE){
  
  p <- ggplot() +
    
    geom_polygon(
      data = us_map,
      aes(long, lat, group = group),
      fill="gray95",
      color="white"
    ) +
    
    geom_point(
      data = walmart,
      aes(lng, lat),
      color="blue",
      alpha=.25,
      size=.7
    ) +
    
    geom_point(
      data = stores2010,
      aes(lng, lat),
      color="red",
      size=2
    ) +
    
    coord_quickmap(
      xlim=c(xmin,xmax),
      ylim=c(ymin,ymax)
    ) +
    
    theme_void() +
    
    theme(
      plot.title = element_text(size=16, face="bold"),
      plot.subtitle = element_text(size=11)
    )
  
  if(subtitle){
    p <- p + labs(
      title = title,
      subtitle = "Blue = Existing Stores | Red = Opened in 2010"
    )
  } else{
    p <- p + labs(title = title)
  }
  
  return(p)
}

# -------------------
# CREATE MAPS
# -------------------

# West has NO subtitle
west <- make_map(west_bounds[1],west_bounds[2],west_bounds[3],west_bounds[4],"",FALSE)

# Midwest keeps subtitle
midwest <- make_map(midwest_bounds[1],midwest_bounds[2],midwest_bounds[3],midwest_bounds[4],"",TRUE)

south <- make_map(south_bounds[1],south_bounds[2],south_bounds[3],south_bounds[4],"South",TRUE)

north <- make_map(north_bounds[1],north_bounds[2],north_bounds[3],north_bounds[4],"North",TRUE)

# -------------------
# PANEL
# -------------------

(west | midwest) /
  (south | north) +
  plot_annotation(
    title = "West / Midwest",
    theme = theme(
      plot.title = element_text(size = 20, face = "bold", hjust = 0.5)
    )
  )


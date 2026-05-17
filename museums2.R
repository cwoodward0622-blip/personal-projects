library(ggplot2)
library(dplyr)

# Get the maximum year
max_year <- max(plot_simplified$year)

# Create label data using the last year for each group
label_data <- plot_simplified %>%
  filter(year == max_year)

ggplot(plot_simplified, aes(x = year, y = museums, color = group)) +
  
  geom_line(size = 1.5) +
  
  # Keep original colors
  scale_color_manual(values = c("green", "orange", "red")) +
  
  labs(
    title = "Inequalities in Museum Availability Across Deprivation Levels",
    subtitle = "Fewer museums exist in more deprived areas; disparities persist over time",
    x = "Year",
    y = "Number of Open Museums"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "none",
    
    # 🔑 Add space on the right for labels
    plot.margin = margin(t = 10, r = 120, b = 10, l = 10)
  ) +
  
  # End-of-line labels
  geom_text(
    data = label_data,
    aes(
      x = max_year + 1,
      y = museums,
      label = paste(group, "areas"),
      color = group
    ),
    hjust = 0,
    size = 4,
    show.legend = FALSE
  ) +
  
  # 🔑 Allow drawing outside the plot panel
  coord_cartesian(
    xlim = c(min(plot_simplified$year), max_year + 3),
    clip = "off"
  )

library(ggplot2)
library(dplyr)

# Get the maximum year
max_year <- max(plot_simplified$year)

# Create label data using the last year for each group
label_data <- plot_simplified %>%
  filter(year == max_year)

ggplot(plot_simplified, aes(x = year, y = museums, color = group)) +
  
  # Engraved-style lines
  geom_line(
    linewidth = 1.6,
    lineend = "round"
  ) +
  
  # Muted, historical ink palette
  scale_color_manual(
    values = c(
      "Least deprived" = "#2F5D50",   # deep green ink
      "Middle deprived" = "#C28E2C",  # ochre / gold
      "Most deprived"  = "#8C2D2D"    # muted red
    )
  ) +
  
  labs(
    title = "Inequality in Museum Availability",
    subtitle = "More affluent areas consistently maintain greater access to museums",
    x = NULL,
    y = "Number of Open Museums"
  ) +
  
  # Parchment-style theme
  theme_minimal(base_size = 14) +
  theme(
    plot.background = element_rect(
      fill = "#F4EEDC", color = NA
    ),
    panel.background = element_rect(
      fill = "#F4EEDC", color = NA
    ),
    
    # Gridlines: subtle, horizontal only (Minard-style)
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(
      color = "#C8C2B5",
      linewidth = 0.4
    ),
    
    # Editorial title feel (Nat Geo)
    plot.title = element_text(
      face = "bold",
      size = 18,
      hjust = 0
    ),
    plot.subtitle = element_text(
      size = 13,
      color = "#444444",
      hjust = 0
    ),
    
    axis.text = element_text(color = "#3B3B3B"),
    axis.title.y = element_text(face = "bold"),
    
    legend.position = "none",
    
    # Space for end labels
    plot.margin = margin(t = 15, r = 140, b = 15, l = 15)
  ) +
  
  # End-of-line labels (printed annotation feel)
  geom_text(
    data = label_data,
    aes(
      x = max_year + 1,
      y = museums,
      label = paste(group, "areas"),
      color = group
    ),
    hjust = 0,
    size = 4.2,
    fontface = "italic",
    show.legend = FALSE
  ) +
  
  # Allow labels outside plot area
  coord_cartesian(
    xlim = c(min(plot_simplified$year), max_year + 3),
    clip = "off"
  )

library(ggplot2)
library(dplyr)

# Get the maximum year
max_year <- max(plot_simplified$year)

# Label data for end-of-line annotations
label_data <- plot_simplified %>%
  filter(year == max_year)

ggplot(plot_simplified, aes(x = year, y = museums, color = group)) +
  
  # Strong, ink-like lines
  geom_line(
    linewidth = 1.7,
    lineend = "round"
  ) +
  
  # History Magazine palette (yellow + brown dominant)
  scale_color_manual(
    values = c(
      "Least deprived" = "#4B2E1E",  # dark brown ink
      "Middle deprived" = "#A9712A", # warm bronze
      "Most deprived"  = "#7A3E1D"   # reddish brown
    )
  ) +
  
  labs(
    title = "Inequality in Museum Access",
    subtitle = "Museum availability remains concentrated in less deprived areas",
    x = NULL,
    y = "Number of Open Museums"
  ) +
  
  # Base theme
  theme_minimal(base_size = 14) +
  theme(
    # Yellow parchment background
    plot.background = element_rect(
      fill = "#F5D76E", color = NA
    ),
    panel.background = element_rect(
      fill = "#F5D76E", color = NA
    ),
    
    # Aged gridlines (horizontal only)
    panel.grid.major.y = element_line(
      color = "#D1B45A",
      linewidth = 0.5
    ),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    
    # Editorial typography
    plot.title = element_text(
      face = "bold",
      size = 20,
      color = "#3B2314",
      hjust = 0
    ),
    plot.subtitle = element_text(
      size = 13,
      color = "#5A3A1E",
      hjust = 0
    ),
    
    axis.text = element_text(
      color = "#3B2314"
    ),
    axis.title.y = element_text(
      face = "bold",
      color = "#3B2314"
    ),
    
    legend.position = "none",
    
    # Space for labels on the right
    plot.margin = margin(t = 20, r = 150, b = 20, l = 20)
  ) +
  
  # End-of-line labels (engraved feel)
  geom_text(
    data = label_data,
    aes(
      x = max_year + 1,
      y = museums,
      label = paste(group, "areas"),
      color = group
    ),
    hjust = 0,
    size = 4.4,
    fontface = "bold",
    show.legend = FALSE
  ) +
  
  # Allow drawing outside panel
  coord_cartesian(
    xlim = c(min(plot_simplified$year), max_year + 3),
    clip = "off"
  )


library(tidyverse)
library(plotly)
library(readr)
library(stringr)

# -----------------------------
# Load Data
# -----------------------------
pokemon <- read_csv("Pokemon.csv")

pokemon_clean <- pokemon %>%
  select(Name, Type, Attack, Defense, Speed, Total) %>%
  drop_na() %>%
  
  # ---- Clean Repeated Names (e.g. AggronMega Aggron -> Mega Aggron)
  mutate(
    Name = str_replace(Name,
                       "^([A-Za-z]+)Mega \\1$",
                       "Mega \\1")
  ) %>%
  
  # ---- Create Archetypes
  mutate(
    Archetype = case_when(
      Attack > Defense + 15 ~ "Offensive",
      Defense > Attack + 15 ~ "Defensive",
      TRUE ~ "Balanced"
    )
  )

# -----------------------------
# Select Outliers & Custom Label Positions
# -----------------------------
outliers <- pokemon_clean %>%
  filter(Name %in% c(
    "Shuckle",
    "Mega Aggron",
    "DeoxysAttack Forme",
    "Carvanha"
  )) %>%
  mutate(
    label_x = Attack,
    label_y = case_when(
      Name == "Mega Aggron" ~ Defense + 10,  # lift higher so it doesn't blend
      TRUE ~ Defense + 3
    )
  )

# -----------------------------
# Build ggplot
# -----------------------------
p <- ggplot(
  pokemon_clean,
  aes(
    x = Attack,
    y = Defense,
    size = Speed,
    color = Archetype,
    text = paste0(
      "<b>", Name, "</b><br>",
      "Type: ", Type, "<br>",
      "Attack: ", Attack, "<br>",
      "Defense: ", Defense, "<br>",
      "Speed: ", Speed
    )
  )
) +
  
  geom_point(alpha = 0.85) +
  
  # ---- Static Labels for Outliers
  geom_text(
    data = outliers,
    aes(x = label_x, y = label_y, label = Name),
    color = "white",
    size = 3,
    show.legend = FALSE
  ) +
  
  scale_color_manual(
    name = "Battle Archetype",
    values = c(
      "Offensive" = "#D62828",
      "Defensive" = "#FFFFFF",
      "Balanced"  = "#9E9E9E"
    )
  ) +
  
  scale_size(range = c(2, 9)) +
  
  labs(
    title = "Pokémon Combat Archetypes",
    x = "Attack",
    y = "Defense"
  ) +
  
  theme_minimal() +
  
  theme(
    plot.title = element_text(
      size = 24,
      face = "bold",
      color = "#E63946"
    ),
    axis.title = element_text(color = "white"),
    axis.text = element_text(color = "white"),
    legend.text = element_text(color = "white"),
    legend.title = element_text(color = "white"),
    panel.background = element_rect(fill = "#1F1F1F", color = NA),
    plot.background  = element_rect(fill = "#1F1F1F", color = NA),
    panel.grid.major = element_line(color = "#333333"),
    panel.grid.minor = element_blank()
  )

# -----------------------------
# Convert to Interactive Plotly
# -----------------------------
ggplotly(p, tooltip = "text") %>%
  layout(
    paper_bgcolor = "#1F1F1F",
    plot_bgcolor = "#1F1F1F",
    font = list(color = "white"),
    
    legend = list(
      title = list(text = "<b>Battle Archetype</b>")
    ),
    
    annotations = list(
      list(
        x = 1.02,
        y = 0.75,
        xref = "paper",
        yref = "paper",
        align = "left",
        text = paste(
          "<b>Circle Size Represents Speed</b><br>",
          "Larger Circles = Faster Pokémon<br>",
          "Smaller Circles = Slower Pokémon"
        ),
        showarrow = FALSE,
        font = list(color = "white", size = 12)
      )
    )
  )


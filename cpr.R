# Load libraries
library(ggplot2)
library(dplyr)

# Load dataset
data <- read.csv("cpr_aed_training_data.csv")

# -----------------------------
# KNOWLEDGE GRAPH
# -----------------------------

knowledge_summary <- data %>%
  group_by(Group) %>%   # <-- replace "Group" if your column name is different
  summarise(
    min_val = mean(Knowledge_Before, na.rm = TRUE),   # before = min
    max_val = mean(Knowledge_After, na.rm = TRUE)     # after = max
  )

ggplot(knowledge_summary, aes(x = Group)) +
  geom_linerange(aes(ymin = min_val, ymax = max_val), size = 1.2) +
  geom_point(aes(y = min_val), size = 3) +   # before point
  geom_point(aes(y = max_val), size = 3) +   # after point
  theme_minimal() +
  labs(
    title = "Knowledge Scores (Before vs After)",
    x = "Group",
    y = "Knowledge Score"
  )

# -----------------------------
# CONFIDENCE GRAPH
# -----------------------------

confidence_summary <- data %>%
  group_by(Group) %>%
  summarise(
    min_val = mean(Confidence_Before, na.rm = TRUE),
    max_val = mean(Confidence_After, na.rm = TRUE)
  )

ggplot(confidence_summary, aes(x = Group)) +
  geom_linerange(aes(ymin = min_val, ymax = max_val), size = 1.2) +
  geom_point(aes(y = min_val), size = 3) +
  geom_point(aes(y = max_val), size = 3) +
  theme_minimal() +
  labs(
    title = "Confidence Scores (Before vs After)",
    x = "Group",
    y = "Confidence Score"
  )



library(ggplot2)
library(dplyr)

data <- read.csv("cpr_aed_training_data.csv")

knowledge_summary <- data %>%
  group_by(Group) %>%
  summarise(
    before = mean(Knowledge_Before, na.rm = TRUE),
    after = mean(Knowledge_After, na.rm = TRUE)
  ) %>%
  mutate(improvement = after - before)

ggplot(knowledge_summary, aes(x = reorder(Group, improvement))) +
  
  # Line from before to after
  geom_segment(aes(xend = Group, y = before, yend = after),
               size = 1.5, color = "gray50") +
  
  # Before point
  geom_point(aes(y = before), size = 3, color = "skyblue3") +
  
  # After point
  geom_point(aes(y = after), size = 3.5, color = "navy") +
  
  theme_minimal() +
  
  labs(
    title = "Knowledge Scores: Before vs After Training",
    x = "Group",
    y = "Score"
  )


library(ggplot2)
library(dplyr)

# Load data
data <- read.csv("cpr_aed_training_data.csv")

# Summary
knowledge_summary <- data %>%
  group_by(Group) %>%
  summarise(
    before = mean(Knowledge_Before, na.rm = TRUE),
    after = mean(Knowledge_After, na.rm = TRUE),
    .groups = "drop"
  )

# Plot
ggplot(knowledge_summary, aes(y = reorder(Group, before))) +
  
  # line (before → after)
  geom_segment(aes(x = before, xend = after, yend = Group),
               color = "gray60", size = 1.5) +
  
  # before point (open circle)
  geom_point(aes(x = before), 
             size = 4, shape = 1, stroke = 1.5, color = "gray40") +
  
  # after point (filled circle)
  geom_point(aes(x = after), 
             size = 4, color = "steelblue") +
  
  # labels for before
  geom_text(aes(x = before, label = round(before,1)),
            hjust = 1.3, size = 4) +
  
  # labels for after
  geom_text(aes(x = after, label = round(after,1)),
            hjust = -0.3, size = 4) +
  
  theme_minimal() +
  
  labs(
    title = "Knowledge Score (Before vs After Training)",
    x = "Score",
    y = ""
  )




library(ggplot2)
library(dplyr)

data <- read.csv("cpr_aed_training_data.csv")

knowledge_summary <- data %>%
  group_by(Group) %>%
  summarise(
    before = mean(Knowledge_Before, na.rm = TRUE),
    after = mean(Knowledge_After, na.rm = TRUE),
    .groups = "drop"
  )

# Convert to long format for legend
plot_data <- knowledge_summary %>%
  tidyr::pivot_longer(cols = c(before, after),
                      names_to = "Time",
                      values_to = "Score")

ggplot() +
  
  # connecting line
  geom_segment(data = knowledge_summary,
               aes(x = before, xend = after, y = Group, yend = Group),
               color = "gray60", size = 1.5) +
  
  # points WITH legend
  geom_point(data = plot_data,
             aes(x = Score, y = Group, shape = Time, fill = Time),
             size = 4, color = "black") +
  
  # labels
  geom_text(data = knowledge_summary,
            aes(x = before, y = Group, label = round(before,1)),
            hjust = 1.3, size = 4) +
  
  geom_text(data = knowledge_summary,
            aes(x = after, y = Group, label = round(after,1)),
            hjust = -0.3, size = 4) +
  
  # shapes: open vs filled
  scale_shape_manual(values = c(before = 21, after = 21)) +
  scale_fill_manual(values = c(before = "white", after = "steelblue")) +
  
  theme_minimal() +
  
  labs(
    title = "Knowledge Score (Before vs After Training)",
    x = "Score",
    y = "",
    shape = "",
    fill = ""
  )




library(ggplot2)
library(dplyr)
library(tidyr)

# Load data
data <- read.csv("cpr_aed_training_data.csv")

# Summary
knowledge_summary <- data %>%
  group_by(Group) %>%
  summarise(
    before = mean(Knowledge_Before, na.rm = TRUE),
    after = mean(Knowledge_After, na.rm = TRUE),
    .groups = "drop"
  )

# -----------------------------
# CREATE CATEGORY VARIABLE
# -----------------------------
knowledge_summary <- knowledge_summary %>%
  mutate(Category = case_when(
    Group %in% c("Faculty", "Student", "Staff") ~ "Role",
    TRUE ~ "Training Status"
  ))

# -----------------------------
# LONG FORMAT (for legend)
# -----------------------------
plot_data <- knowledge_summary %>%
  pivot_longer(cols = c(before, after),
               names_to = "Time",
               values_to = "Score")

# -----------------------------
# PLOT
# -----------------------------
ggplot() +
  
  # connecting lines (colored by category)
  geom_segment(data = knowledge_summary,
               aes(x = before, xend = after, y = Group, yend = Group, color = Category),
               size = 1.5) +
  
  # points
  geom_point(data = plot_data,
             aes(x = Score, y = Group, shape = Time, fill = Time, color = Category),
             size = 4) +
  
  # labels
  geom_text(data = knowledge_summary,
            aes(x = before, y = Group, label = round(before,1)),
            hjust = 1.3, size = 4) +
  
  geom_text(data = knowledge_summary,
            aes(x = after, y = Group, label = round(after,1)),
            hjust = -0.3, size = 4) +
  
  # shapes (open vs filled)
  scale_shape_manual(values = c(before = 21, after = 21)) +
  scale_fill_manual(values = c(before = "white", after = "black")) +
  
  # COLORS FOR GROUP TYPES
  scale_color_manual(values = c(
    "Role" = "steelblue",
    "Training Status" = "darkorange"
  )) +
  
  theme_minimal() +
  
  labs(
    title = "Knowledge Score (Before vs After Training)",
    x = "Score",
    y = "",
    color = "Group Type",
    shape = "",
    fill = ""
  ) +
  
  theme(
    panel.grid.major.y = element_blank(),
    legend.position = "top"
  )




# -----------------------------
# LOAD LIBRARIES
# -----------------------------
library(ggplot2)
library(dplyr)
library(tidyr)

# -----------------------------
# LOAD DATA
# -----------------------------
data <- read.csv("cpr_aed_training_data.csv")

# -----------------------------
# SUMMARY DATA
# -----------------------------
knowledge_summary <- data %>%
  group_by(Group) %>%
  summarise(
    before = mean(Knowledge_Before, na.rm = TRUE),
    after = mean(Knowledge_After, na.rm = TRUE),
    .groups = "drop"
  )

# -----------------------------
# ORDER GROUPS (TOP → BOTTOM)
# -----------------------------
knowledge_summary$Group <- factor(knowledge_summary$Group,
                                  levels = c(
                                    "Overall",
                                    "Faculty", "Student", "Staff",
                                    "Active CPR Cert", "Prior/Lapsed", "No Training"
                                  )
)

# -----------------------------
# CREATE CATEGORY (COLOR GROUPS)
# -----------------------------
knowledge_summary <- knowledge_summary %>%
  mutate(Category = case_when(
    Group %in% c("Faculty", "Student", "Staff") ~ "Role",
    TRUE ~ "Training Status"
  ))

# -----------------------------
# LONG FORMAT FOR LEGEND
# -----------------------------
plot_data <- knowledge_summary %>%
  pivot_longer(cols = c(before, after),
               names_to = "Time",
               values_to = "Score")

# -----------------------------
# PLOT
# -----------------------------
ggplot() +
  
  # connecting lines
  geom_segment(data = knowledge_summary,
               aes(x = before, xend = after,
                   y = rev(Group), yend = rev(Group),
                   color = Category),
               size = 1.5) +
  
  # points (before vs after)
  geom_point(data = plot_data,
             aes(x = Score, y = rev(Group),
                 shape = Time, fill = Time, color = Category),
             size = 4) +
  
  # labels (before)
  geom_text(data = knowledge_summary,
            aes(x = before, y = rev(Group),
                label = round(before, 1)),
            hjust = 1.3, size = 4) +
  
  # labels (after)
  geom_text(data = knowledge_summary,
            aes(x = after, y = rev(Group),
                label = round(after, 1)),
            hjust = -0.3, size = 4) +
  
  # shape + fill (white vs filled)
  scale_shape_manual(values = c(before = 21, after = 21)) +
  scale_fill_manual(values = c(before = "white", after = "black")) +
  
  # colors for group types
  scale_color_manual(values = c(
    "Role" = "steelblue",
    "Training Status" = "darkorange"
  )) +
  
  # styling
  theme_minimal() +
  
  labs(
    title = "Knowledge Score (Before vs After Training)",
    x = "Score",
    y = "",
    color = "Group Type",
    shape = "",
    fill = ""
  ) +
  
  theme(
    panel.grid.major.y = element_blank(),
    legend.position = "top"
  )




# -----------------------------
# LOAD LIBRARIES
# -----------------------------
library(ggplot2)
library(dplyr)
library(tidyr)

# -----------------------------
# LOAD DATA
# -----------------------------
data <- read.csv("cpr_aed_training_data.csv")

# -----------------------------
# SUMMARY DATA
# -----------------------------
knowledge_summary <- data %>%
  group_by(Group) %>%
  summarise(
    before = mean(Knowledge_Before, na.rm = TRUE),
    after = mean(Knowledge_After, na.rm = TRUE),
    .groups = "drop"
  )

# -----------------------------
# ORDER GROUPS (TOP → BOTTOM)
# -----------------------------
knowledge_summary$Group <- factor(knowledge_summary$Group,
                                  levels = c(
                                    "Overall",
                                    "Faculty", "Student", "Staff",
                                    "Active CPR Cert", "Prior/Lapsed", "No Training"
                                  )
)

# -----------------------------
# CREATE CATEGORY + FIX OVERALL
# -----------------------------
knowledge_summary <- knowledge_summary %>%
  mutate(Category = case_when(
    Group == "Overall" ~ "Overall",
    Group %in% c("Faculty", "Student", "Staff") ~ "Role",
    TRUE ~ "Training Status"
  ))

# -----------------------------
# LONG FORMAT
# -----------------------------
plot_data <- knowledge_summary %>%
  pivot_longer(cols = c(before, after),
               names_to = "Time",
               values_to = "Score")

# -----------------------------
# PLOT
# -----------------------------
ggplot() +
  
  # lines
  geom_segment(data = knowledge_summary,
               aes(x = before, xend = after,
                   y = rev(Group), yend = rev(Group),
                   color = Category),
               size = 1.5) +
  
  # points
  geom_point(data = plot_data,
             aes(x = Score, y = rev(Group),
                 shape = Time, fill = Time, color = Category),
             size = 4) +
  
  # labels
  geom_text(data = knowledge_summary,
            aes(x = before, y = rev(Group),
                label = round(before,1)),
            hjust = 1.3, size = 4) +
  
  geom_text(data = knowledge_summary,
            aes(x = after, y = rev(Group),
                label = round(after,1)),
            hjust = -0.3, size = 4) +
  
  # shapes (before vs after)
  scale_shape_manual(values = c(before = 21, after = 21)) +
  scale_fill_manual(values = c(before = "white", after = "black")) +
  
  # COLORS (now includes Overall)
  scale_color_manual(values = c(
    "Overall" = "black",
    "Role" = "steelblue",
    "Training Status" = "darkorange"
  )) +
  
  # styling
  theme_minimal() +
  
  labs(
    title = "Knowledge Score (Before vs After Training)",
    x = "Score",
    y = "",
    color = "Group Type",
    shape = "",
    fill = ""
  ) +
  
  theme(
    panel.grid.major.y = element_blank(),
    legend.position = "top"
  )




# -----------------------------
# LOAD LIBRARIES
# -----------------------------
library(ggplot2)
library(dplyr)
library(tidyr)

# -----------------------------
# LOAD DATA
# -----------------------------
data <- read.csv("cpr_aed_training_data.csv")

# -----------------------------
# SUMMARY DATA
# -----------------------------
knowledge_summary <- data %>%
  group_by(Group) %>%
  summarise(
    before = mean(Knowledge_Before, na.rm = TRUE),
    after = mean(Knowledge_After, na.rm = TRUE),
    .groups = "drop"
  )

# -----------------------------
# ORDER GROUPS (TOP → BOTTOM)
# -----------------------------
knowledge_summary$Group <- factor(knowledge_summary$Group,
                                  levels = c(
                                    "Overall",
                                    "Faculty", "Student", "Staff",
                                    "Active CPR Cert", "Prior/Lapsed", "No Training"
                                  )
)

# -----------------------------
# CREATE CATEGORY
# -----------------------------
knowledge_summary <- knowledge_summary %>%
  mutate(Category = case_when(
    Group == "Overall" ~ "Overall",
    Group %in% c("Faculty", "Student", "Staff") ~ "Role",
    TRUE ~ "Training Status"
  ))

# -----------------------------
# LONG FORMAT
# -----------------------------
plot_data <- knowledge_summary %>%
  pivot_longer(cols = c(before, after),
               names_to = "Time",
               values_to = "Score")

# -----------------------------
# PLOT
# -----------------------------
ggplot() +
  
  # lines
  geom_segment(data = knowledge_summary,
               aes(x = before, xend = after,
                   y = rev(Group), yend = rev(Group),
                   color = Category),
               size = 1.5) +
  
  # points
  geom_point(data = plot_data,
             aes(x = Score, y = rev(Group),
                 shape = Time, fill = Time, color = Category),
             size = 4) +
  
  # labels (before)
  geom_text(data = knowledge_summary,
            aes(x = before, y = rev(Group),
                label = round(before,1)),
            hjust = 1.3, size = 4) +
  
  # labels (after)
  geom_text(data = knowledge_summary,
            aes(x = after, y = rev(Group),
                label = round(after,1)),
            hjust = -0.3, size = 4) +
  
  # shapes
  scale_shape_manual(values = c(before = 21, after = 21)) +
  scale_fill_manual(values = c(before = "white", after = "black")) +
  
  # colors
  scale_color_manual(values = c(
    "Overall" = "black",
    "Role" = "steelblue",
    "Training Status" = "darkorange"
  )) +
  
  # styling
  theme_minimal() +
  
  labs(
    title = "Knowledge Score (Before vs After Training)",
    x = "Score",
    y = ""
  ) +
  
  theme(
    panel.grid.major.y = element_blank(),
    legend.position = "none"   # ✅ removes legend only
  )





# -----------------------------
# LOAD LIBRARIES
# -----------------------------
library(ggplot2)
library(dplyr)
library(tidyr)

# -----------------------------
# LOAD DATA
# -----------------------------
data <- read.csv("cpr_aed_training_data.csv")

# -----------------------------
# SUMMARY DATA (CONFIDENCE)
# -----------------------------
confidence_summary <- data %>%
  group_by(Group) %>%
  summarise(
    before = mean(Confidence_Before, na.rm = TRUE),
    after = mean(Confidence_After, na.rm = TRUE),
    .groups = "drop"
  )

# -----------------------------
# ORDER GROUPS (TOP → BOTTOM)
# -----------------------------
confidence_summary$Group <- factor(confidence_summary$Group,
                                   levels = c(
                                     "Overall",
                                     "Faculty", "Student", "Staff",
                                     "Active CPR Cert", "Prior/Lapsed", "No Training"
                                   )
)

# -----------------------------
# CREATE CATEGORY
# -----------------------------
confidence_summary <- confidence_summary %>%
  mutate(Category = case_when(
    Group == "Overall" ~ "Overall",
    Group %in% c("Faculty", "Student", "Staff") ~ "Role",
    TRUE ~ "Training Status"
  ))

# -----------------------------
# LONG FORMAT
# -----------------------------
plot_data <- confidence_summary %>%
  pivot_longer(cols = c(before, after),
               names_to = "Time",
               values_to = "Score")

# -----------------------------
# PLOT
# -----------------------------
ggplot() +
  
  # lines
  geom_segment(data = confidence_summary,
               aes(x = before, xend = after,
                   y = rev(Group), yend = rev(Group),
                   color = Category),
               size = 1.5) +
  
  # points
  geom_point(data = plot_data,
             aes(x = Score, y = rev(Group),
                 shape = Time, fill = Time, color = Category),
             size = 4) +
  
  # labels (before)
  geom_text(data = confidence_summary,
            aes(x = before, y = rev(Group),
                label = round(before,1)),
            hjust = 1.3, size = 4) +
  
  # labels (after)
  geom_text(data = confidence_summary,
            aes(x = after, y = rev(Group),
                label = round(after,1)),
            hjust = -0.3, size = 4) +
  
  # shapes
  scale_shape_manual(values = c(before = 21, after = 21)) +
  scale_fill_manual(values = c(before = "white", after = "black")) +
  
  # colors
  scale_color_manual(values = c(
    "Overall" = "black",
    "Role" = "steelblue",
    "Training Status" = "darkorange"
  )) +
  
  # styling
  theme_minimal() +
  
  labs(
    title = "Confidence Score (Before vs After Training)",
    x = "Score",
    y = ""
  ) +
  
  theme(
    panel.grid.major.y = element_blank(),
    legend.position = "none"
  )


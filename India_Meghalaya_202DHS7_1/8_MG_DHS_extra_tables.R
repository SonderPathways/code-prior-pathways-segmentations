# Load necessary libraries
library(survey)
library(ggplot2)
library(readr)
library(readxl)
library(dplyr)

# Load the RDS file
df <- read.csv("dataOut/meghalaya_segmentaton_2024.csv")

# Load the Excel file
excel_data <- read_excel("MG_India_DHS_2020_Data_Dictionary.xlsx", sheet = "outcomes")

# Extract the variable names to explore and their descriptions
variables_to_explore <- excel_data$varnames


# Create the survey design object
Pathdesign <- svydesign(id = df$v021, strata = df$v023, weights = df$weight, survey.lonely.psu = "adjust", nest = TRUE, data = df)

# Calculate the weighted totals for each class
df_totals <- svytotal(~segment, design = Pathdesign)

# Specify the order of classes
class_order <- c("U1-MG.IND", "U2-MG.IND", "R2-MG.IND", "R3.1-MG.IND",  "R3.2-MG.IND", "R4-MG.IND", "U4-MG.IND")

# Function to create stacked bar charts for each variable
create_stacked_bar_chart <- function(variable) {
  # Calculate weighted counts for the variable by class
  formula <- as.formula(paste0("~", variable, "+segment"))
  svy_table <- svytable(formula, design = Pathdesign)
  svy_df <- as.data.frame(svy_table)
  
  # Ensure the classes are ordered
  svy_df$segment <- factor(svy_df$segment, levels = class_order)
  
  # Create the stacked bar chart
  ggplot(svy_df, aes_string(x = "segment", y = "Freq", fill = variable)) +
    geom_bar(stat = "identity", position = "fill") +
    labs(title = paste(variable),
         x = "Class",
         y = "Proportion") +
    scale_y_continuous(labels = scales::percent) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 10),
      axis.title = element_text(size = 8),
      axis.text = element_text(size = 7),
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.background = element_rect(fill = "white", colour = "white"),
      panel.grid.major = element_line(colour = "black"),
      panel.grid.minor = element_line(colour = "black")
    )
}

# Generate and save stacked bar charts for each variable
for (i in seq_along(variables_to_explore)) {
  variable <- variables_to_explore[i]
  chart <- create_stacked_bar_chart(variable)
  ggsave(filename = paste0("stacked_bar_", variable, ".png"), plot = chart)
}


# EXTRA

df <- read.csv("dataOut/meghalaya_segmentation_2024.csv")

IR <- readRDS("data/IR_meghalaya.rds")
HH <- readRDS("data/HH_meghalaya.rds")

IR <- IR %>% select("caseid","v001", "v002")
HH <- HH %>% select("hv001", "hv002", "shdist")

HH$v001 <- HH$hv001
HH$v002 <- HH$hv002

IR.HH <- base::merge(IR, HH, by=c("v001", "v002"), all.x=T)
IR.HH <- IR.HH %>%  select("caseid", "shdist")

df <- left_join(df, IR.HH, by = "caseid")
df$shdist <- as.character(df$shdist)

Pathdesign <- svydesign(id = df$v021, strata = df$v023, weights = df$weight, survey.lonely.psu = "adjust", nest = TRUE, data = df)

# Calculate the weighted totals for each class

formula <- as.formula(paste0("~", "shdist", "+segment"))
svy_table <- svytable(formula, design = Pathdesign)
svy_df <- as.data.frame(svy_table)


df_district <- svy_df %>%
  group_by(shdist) %>%
  mutate(percentage = (Freq / sum(Freq)) * 100) %>%
  ungroup() %>%
  select(shdist, segment, Freq, percentage)

write.csv(df_district, "dataOut/segment_district_meghalaya.csv", row.names = FALSE)


ggplot(df_district, aes(x = segment, y = percentage, fill = segment)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ shdist, scales = "free") +
  labs(title = "Percentage Contribution of Each Segment per District",
       x = "Segment",
       y = "Percentage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggsave("percentage_contribution_per_segment_per_district.png")

# EXTRA 2

df <- read.csv("dataOut/meghalaya_segmentation_2024.csv")
IR <- readRDS("data/IR_meghalaya.rds")
IR <- IR %>% select(caseid, v312) 
df <- left_join(df, IR, by ="caseid")

Pathdesign <- svydesign(id = df$v021, strata = df$v023, weights = df$weight, survey.lonely.psu = "adjust", nest = TRUE, data = df)

# Calculate the weighted totals for each class

formula <- as.formula(paste0("~", "v312", "+segment"))
svy_table <- svytable(formula, design = Pathdesign)
svy_df <- as.data.frame(svy_table)

df_fp <- svy_df %>%
  group_by(segment) %>%
  mutate(percentage = (Freq / sum(Freq)) * 100) %>%
  ungroup() %>%
  select(segment, v312, percentage)

ggplot(df_fp, aes(x = segment, y = percentage, fill = v312)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ v312, scales = "free") +
  labs(title = "Percentage Contribution of Each FP Method per Segment",
       x = "Segment",
       y = "Percentage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

write.csv(df_fp, "dataOut/segment_fp_meghalaya.csv", row.names = FALSE)



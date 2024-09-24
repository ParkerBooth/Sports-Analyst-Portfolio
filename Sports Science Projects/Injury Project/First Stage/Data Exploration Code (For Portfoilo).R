library(dplyr) # Data Manipulation Package %>% 
library(ggplot2) # Plotting Package
library(stargazer) # Regression Output Package
library(readxl) # Excel Reader Package
library(knitr) # Table Exporting Tool
library(kableExtra) # Table Editing Tool https://cran.r-project.org/web/packages/kableExtra/vignettes/awesome_table_in_html.html#CellText_Specification
library(tidyr) # Data Cleaning Package
library(cowplot) # Plot Combining Package
setwd("Path")


Injury_Data <- read_excel("Path")

# Check for Duplicates ----
count_table <- as.data.frame(table(Injury_Data$athlete_name, Injury_Data$issue_classification))

# Filter the data frame to keep only rows where the count is > 1
duplicates_filtered_df <- Injury_Data[Injury_Data$athlete_name %in% count_table$Var1[count_table$Freq > 1] & 
                                        Injury_Data$issue_classification %in% count_table$Var2[count_table$Freq > 1],]

# Define a function to check if two dates are within a week of each other
duplicates_filtered_df$incident_date <- as.Date(duplicates_filtered_df$incident_date)

within_a_week <- function(date1, date2) {
  return(abs(as.numeric(difftime(date1, date2, units = "days"))) <= 7)
}

# Filter the dataframe to keep only rows where incident dates are within a week of each other
duplicates_filtered_df <- duplicates_filtered_df %>%
  group_by(athlete_name, issue_classification) %>%
  filter(any(within_a_week(incident_date, lag(incident_date)))) %>% 
  select(occurrence_id, athlete_name, incident_date, incident_sport, issue_description, issue_classification, report_by, issue_occurrence)

# Check that this variable Uniquely Identifies Observations
length(unique(Injury_Data$occurrence_id))

# Remove Duplicate Observations from Data set
Injury_Data <- Injury_Data[!(Injury_Data$occurrence_id %in% c(253402, 251699, 251755, 252014, 251333, 250861, 468998, 251875, 251584, 252015, 251019, 251396, 252427, 410815)),]


# Dropping Unused Variables ----


# Drop Illness Data
Injury_Data <- Injury_Data[!(Injury_Data$issue_type == "Illness"),]
unique(Injury_Data$issue_type)

# Set new dataframe
df <- Injury_Data
View(df)

# Drop Injuries above Lumbar Spine
df <- df[!(df$body_area %in% c("Neck", "Shoulder", "Elbow", "Head", "Forearm", "Wrist/Hand", "Chest", "Upper Arm", "Thoracic Spine")),]
df <- df[!(df$body_area == "Unspecified/Crossing"),] # < 20 observations here. Mostly strains, spasms, and cysts. Not worth it to keep
unique(df$body_area)

# Drop Injuries that are not Soft Tissue
df <- df[!(df$issue_classification %in% c("Disc", "Dislocation", "Apophysitis", "Osteoarthritis", "Organ Damage", "Nerve", "Osteochondral", "Vascular", "Structural Abnormality", "Fracture", "Laceration/ Abrasion")),]
unique(df$issue_classification)


# Code for Graph of Average Days until Medically Cleared ----

# Change Dates from ISO 8601 to regular dates
df$formatted_incident_date <- as.Date(df$incident_date)
df$formatted_md_release_date <- as.Date(df$md_release_date)


# Checking Work
df %>% 
  select(incident_date, formatted_incident_date, md_release_date, formatted_md_release_date) %>% 
  View()

# Time from injury until Medically Released
df$time_to_md_release <- (df$formatted_md_release_date - df$formatted_incident_date)
df$time_to_md_release <- as.numeric(df$time_to_md_release)

# Checking Work
df %>% 
  select(formatted_incident_date, formatted_md_release_date, time_to_md_release ) %>% 
  View()

# Find Avg. Time Missed by Sport
missed_days <- df[!is.na(df$time_to_md_release), ] %>% 
  group_by(incident_sport) %>% 
  summarise(avg_days_missed = mean(time_to_md_release, na.rm = TRUE))


# Overall Avg. Time Missed
avg_md_cleared_days <- mean(df$time_to_md_release, na.rm = TRUE)
avg_md_cleared_days


# Graph Avg. Time until Medically Cleared
ggplot(missed_days, aes(x = reorder(incident_sport, -avg_days_missed), y = avg_days_missed)) +
  geom_bar(stat = "identity", fill = "blue") +
  # geom_text(aes(label = incident_sport), vjust = 0, color = "black", angle = 90) +
  labs(title = "Average Days until Medically Cleared by Sport", x = "Sport", y = "Avg. Days")



# Dummy Variables for each Sport + Regression ----

# Creating a Dummy Variable for each different Sport we analyze
sports_dummies <- unique(df$incident_sport)
df_dummies <- data.frame(matrix(0, nrow = nrow(df), ncol = 0))
for (sport in sports_dummies) {
  dummy_var_name <- paste("dummy_", gsub(" ", "_", sport), sep = "")
  df_dummies[, dummy_var_name] <- ifelse(df$incident_sport == sport, 1, 0 )
}

# Add dummy variables into the df dataset with rest of data (DONT KNOW IF THIS IS NEEDED)
dummy_var_names <- names(df_dummies)
# Loop to add dummy variables to the original data frame
for (var_name in dummy_var_names) {
  df[[var_name]] <- df_dummies[[var_name]]
}

# Rename the column with issues
new_column_name <- "dummy_Track_and_Field"  # New column name without the special character
colnames(df)[colnames(df) == "dummy_Track_&_Field"] <- new_column_name

new_column_name <- "dummy_Other_Sports"  # New column name without the special character
colnames(df)[colnames(df) == "dummy_Other_-_Sports"] <- new_column_name


# # Linear Regression Model with Dummy Variables on the time it takes to get Medically Released
# model <- lm(time_to_md_release ~ dummy_Basketball + dummy_Gymnastics + dummy_American_Football + dummy_Baseball
#             + dummy_Track_and_Field + dummy_Volleyball + dummy_Skiing + dummy_Tennis + dummy_Soccer + dummy_Swimming + dummy_Diving
#             + dummy_Softball + dummy_Other_Sports + dummy_Golf + dummy_Lacrosse + dummy_Cross_Country + dummy_Beach_Volleyball, data = df)
# summary(model)


# Examination date ----

# Re-Format Date
df$formatted_examination_date <- as.Date(df$examination_date)

# Check Work
df %>% 
  select(examination_date, formatted_examination_date) %>% 
  View()

# Difference between injury occuring date and examination date
df$time_until_examined <- (df$formatted_examination_date - df$formatted_incident_date)
df$time_until_examined <- as.numeric(df$time_until_examined)

# Check Work
df %>% 
  select(formatted_incident_date, formatted_examination_date, time_until_examined) %>% 
  View()

# Find Avg. Time Missed by Sport
days_to_examination <- df[!is.na(df$time_until_examined), ] %>% 
  group_by(incident_sport) %>% 
  summarise(avg_days_until_exam = mean(time_until_examined, na.rm = TRUE))

# Overall Avg. Time Missed
avg_days_to_examination <- mean(df$time_until_examined, na.rm = TRUE)
avg_days_to_examination


ggplot(days_to_examination, aes(x = reorder(incident_sport, -avg_days_until_exam), y = avg_days_until_exam)) +
  geom_bar(stat = "identity", fill = "blue") +
  # geom_text(aes(label = incident_sport), vjust = 0, color = "black", angle = 90) +
  labs(title = "Average Days until Examined by Sport", x = "Sport", y = "Avg. Days")


# #Linear Regression
# Examination_Model <- lm(time_until_examined ~ dummy_Basketball + dummy_Gymnastics + dummy_American_Football + dummy_Baseball
#             + dummy_Track_and_Field + dummy_Volleyball + dummy_Skiing + dummy_Tennis + dummy_Soccer + dummy_Swimming + dummy_Diving
#             + dummy_Softball + dummy_Other_Sports + dummy_Golf + dummy_Lacrosse + dummy_Cross_Country + dummy_Beach_Volleyball, data = df)
# summary(Examination_Model)




 
# Creating Data Explanatory Variables ----

# Overall Percentage of Recorded Injuries that are Lower Body
Injury_Data$lower_body <- ifelse(Injury_Data$body_area %in% c("Buttock/pelvis", "Ankle", "Foot", "Lumbar Spine", "Lower Leg", "Hip/Groin", "Thigh", "Trunk/Abdominal", "Knee"), 1, 0)
table(Injury_Data$lower_body)
lower_body_percentage <- sum(Injury_Data$lower_body == 1) / sum(Injury_Data$lower_body == 1 | Injury_Data$lower_body == 0)
lower_body_percentage

# Lower Body Injuries by sport
lower_body_percentage_by_sport <- Injury_Data %>%
  group_by(incident_sport) %>%
  summarize(lb_percentage = sum(lower_body == 1) / n()) 
print(lower_body_percentage_by_sport)


# Overall Percentage of Recorded Injuries that are Soft Tissue
Injury_Data$soft_tissue <- ifelse(Injury_Data$issue_classification %in% c("Bruising/ Haematoma" ,"Ligament", "Muscle Strain/Spasm", "Tendon", "Other Pain/ unspecified", "Post Surgery", "Synovitis/ Impingement", "Unspecified/Crossing", "Synovitis/ Impingement/ Bursitis", "Instability"), 1, 0)
table(Injury_Data$soft_tissue)
soft_tissue_percentage <- sum(Injury_Data$soft_tissue == 1) / sum(Injury_Data$soft_tissue == 1 | Injury_Data$soft_tissue == 0)
soft_tissue_percentage

# Soft Tissue Injuries by sport
soft_tissue_percentage_by_sport <- Injury_Data %>% 
  group_by(incident_sport) %>% 
  summarize(sti_percentage = sum(soft_tissue == 1) / n())
print(soft_tissue_percentage_by_sport)


# Overall Percentage of Recorded Lower Body Ijuries that are Soft Tissue
Injury_Data$lower_body_soft_tissue <- ifelse(
  Injury_Data$issue_classification %in% c("Bruising/ Haematoma" ,"Ligament", "Muscle Strain/Spasm", "Tendon", "Other Pain/ unspecified", "Post Surgery", "Synovitis/ Impingement", "Unspecified/Crossing", "Synovitis/ Impingement/ Bursitis", "Instability") &
  Injury_Data$body_area %in% c("Buttock/pelvis", "Ankle", "Foot", "Lumbar Spine", "Lower Leg", "Hip/Groin", "Thigh", "Trunk/Abdominal", "Knee"), 1, 0)
table(Injury_Data$lower_body_soft_tissue)
lb_sti_percentage <- sum(Injury_Data$lower_body_soft_tissue == 1) / sum(Injury_Data$lower_body_soft_tissue == 1 | Injury_Data$lower_body_soft_tissue == 0)
lb_sti_percentage

# Lower Body Soft Tissue Injuries by sport
lb_sti_percentage_by_sport <- Injury_Data %>% 
  group_by(incident_sport) %>% 
  summarize(lb_sti_percentage = sum(lower_body_soft_tissue == 1) / n())
print(lb_sti_percentage_by_sport)


# SWITCHING FROM OVERALL DATA TO LOWER BODY SOFT TISSUE SPECIFIC DATA FOR METRICS


# Overall Percentage of LB STI that are recurring injuries
df$injury_recurrence_dummy <- ifelse(df$recurrence %in% c("Multiple Recurrence of Past Injury/Illness", "First Recurrence of Past Injury/Illness", "Pre-Existing Injury/Illness"), 1, 0)
table(df$injury_recurrence_dummy)
injury_recurrence_percentage <- sum(df$injury_recurrence_dummy == 1) / sum(df$injury_recurrence_dummy == 1 | df$injury_recurrence_dummy == 0)
injury_recurrence_percentage

# Recurring Injury Percentage by sport
recurring_injury_percentage_by_sport <- df %>% 
  group_by(incident_sport) %>% 
  summarize(injury_recurrence_percentage = sum(injury_recurrence_dummy == 1) / n())
print(recurring_injury_percentage_by_sport)

# Foot VS Leg VS Pelvis VS Midsection

# Overall Percentage of LB STI that are in Foot Section
df$foot_dummy <- ifelse(df$body_area %in% c("Foot", "Ankle"), 1, 0)
table(df$foot_dummy)
foot_percentage <- sum(df$foot_dummy == 1) / sum(df$foot_dummy == 1 | df$foot_dummy == 0)
foot_percentage

# Foot Section Injury Percentage by sport
foot_percentage_by_sport <- df %>% 
  group_by(incident_sport) %>% 
  summarize(foot_percentage = sum(foot_dummy == 1) / n())
print(foot_percentage_by_sport)

# Overall Percentage of LB STI that are in Leg Section
df$leg_dummy <- ifelse(df$body_area %in% c("Lower Leg", "Thigh", "Knee"), 1, 0)
table(df$leg_dummy)
leg_percentage <- sum(df$leg_dummy == 1) / sum(df$leg_dummy == 1 | df$leg_dummy == 0)
leg_percentage

# Leg Section Injury Percentage by sport
leg_percentage_by_sport <- df %>% 
  group_by(incident_sport) %>% 
  summarize(leg_percentage = sum(leg_dummy == 1) / n())
print(leg_percentage_by_sport)

# Overall Percentage of LB STI that are in Pelvic Section
df$pelvic_dummy <- ifelse(df$body_area %in% c("Buttock/pelvis", "Hip/Groin"), 1, 0)
table(df$pelvic_dummy)
pelvic_percentage <- sum(df$pelvic_dummy == 1) / sum(df$pelvic_dummy == 1 | df$pelvic_dummy == 0)
pelvic_percentage

# Pelvic Section Injury Percentage by sport
pelvic_percentage_by_sport <- df %>% 
  group_by(incident_sport) %>% 
  summarize(pelvic_percentage = sum(pelvic_dummy == 1) / n())
print(pelvic_percentage_by_sport)

# Overall Percentage of LB STI that are in Midsection
df$midsection_dummy <- ifelse(df$body_area %in% c("Lumbar Spine", "Trunk/Abdominal"), 1, 0)
table(df$midsection_dummy)
midsection_percentage <- sum(df$midsection_dummy == 1) / sum(df$midsection_dummy == 1 | df$midsection_dummy == 0)
midsection_percentage

# Midsection Injury Percentage by sport
midsection_percentage_by_sport <- df %>% 
  group_by(incident_sport) %>% 
  summarize(midsection_percentage = sum(midsection_dummy == 1) / n())
print(midsection_percentage_by_sport)

# Acute VS Chronic VS Other

# Overall Percentage of LB STI that are Acute
df$acute_dummy <- ifelse(df$issue_occurrence %in% c("Acute"), 1, 0)
table(df$acute_dummy)
acute_percentage <- sum(df$acute_dummy == 1) / sum(df$acute_dummy == 1 | df$acute_dummy == 0)
acute_percentage

# Acute Injury Percentage by sport
acute_percentage_by_sport <- df %>% 
  group_by(incident_sport) %>% 
  summarize(acute_percentage = sum(acute_dummy == 1) / n())
print(acute_percentage_by_sport)

# Overall Percentage of LB STI that are Chronic
df$chronic_dummy <- ifelse(df$issue_occurrence %in% c("Chronic"), 1, 0)
table(df$chronic_dummy)
chronic_percentage <- sum(df$chronic_dummy == 1) / sum(df$chronic_dummy == 1 | df$chronic_dummy == 0)
chronic_percentage

# Chronic Injury Percentage by sport
chronic_percentage_by_sport <- df %>% 
  group_by(incident_sport) %>% 
  summarize(chronic_percentage = sum(chronic_dummy == 1) / n())
print(chronic_percentage_by_sport)

# Overall Percentage of LB STI that are Other Occurrence
df$other_occurrence_dummy <- ifelse(df$issue_occurrence %in% c("Gradual", "Overuse", "Other", "Traumatic"), 1, 0)
table(df$other_occurrence_dummy)
other_occurrence_percentage <- sum(df$other_occurrence_dummy == 1) / sum(df$other_occurrence_dummy == 1 | df$other_occurrence_dummy == 0)
other_occurrence_percentage

# Other Occurrence Injury Percentage by sport
other_occurrence_percentage_by_sport <- df %>% 
  group_by(incident_sport) %>% 
  summarize(other_occurrence_percentage = sum(other_occurrence_dummy == 1) / n())
print(other_occurrence_percentage_by_sport)

# Observations
observations_by_sport <- df %>% 
  group_by(incident_sport) %>% 
  summarize(observations_by_sport = n())

observations_overall <- nrow(df)



# Creating Data Exploration Table ----

# Dataframe that shows all stats aggregated into one place (by sport)
combined_table <- data.frame(
  Sport = lb_sti_percentage_by_sport$incident_sport,
  "Observations" = observations_by_sport$observations_by_sport,
  "LB STI" = sprintf("%.3f", lb_sti_percentage_by_sport$lb_sti_percentage),
  "STI" = sprintf("%.3f", soft_tissue_percentage_by_sport$sti_percentage),
  "LB" = sprintf("%.3f", lower_body_percentage_by_sport$lb_percentage),
  "Recurring" = sprintf("%.3f", recurring_injury_percentage_by_sport$injury_recurrence_percentage),
  "Foot Area" = sprintf("%.3f", foot_percentage_by_sport$foot_percentage),
  "Leg Area" = sprintf("%.3f", leg_percentage_by_sport$leg_percentage),
  "Pelvic Area" = sprintf("%.3f", pelvic_percentage_by_sport$pelvic_percentage),
  "Midsection" = sprintf("%.3f", midsection_percentage_by_sport$midsection_percentage),
  "Acute" = sprintf("%.3f", acute_percentage_by_sport$acute_percentage),
  "Chronic" = sprintf("%.3f", chronic_percentage_by_sport$chronic_percentage),
  # "Other Occurrence" = sprintf("%.3f", other_occurrence_percentage_by_sport$other_occurrence_percentage),
  "Until Medically Cleared" = sprintf("%.1f", missed_days$avg_days_missed),
  "Until Examination" = sprintf("%.1f", days_to_examination$avg_days_until_exam)
)

# Dataframe that shows all stats aggregated into one place (totals overall)
overall_row <- data.frame(
  Sport = "Overall",
  observations_by_sport = observations_overall,
  lb_sti_percentage = sprintf("%.3f",lb_sti_percentage),
  soft_tissue_percentage = sprintf("%.3f",soft_tissue_percentage),
  lower_body_percentage = sprintf("%.3f", lower_body_percentage),
  injury_recurrence_percentage = sprintf("%.3f", injury_recurrence_percentage),
  foot_percentage = sprintf("%.3f", foot_percentage),
  leg_percentage = sprintf("%.3f", leg_percentage),
  pelvic_percentage = sprintf("%.3f", pelvic_percentage),
  midsection_percentage = sprintf("%.3f", midsection_percentage),
  acute_percentage = sprintf("%.3f", acute_percentage),
  chronic_percentage = sprintf("%.3f", chronic_percentage),
  # other_occurrence_percentage = sprintf("%.3f", other_occurrence_percentage),
  days_until_medically_cleared = sprintf("%.1f", avg_md_cleared_days),
  days_until_examination = sprintf("%.1f", avg_days_to_examination)
)

# Merge the dataframes together
colnames(overall_row) <- colnames(combined_table)
combined_table <- rbind(combined_table, overall_row)

highlight_cols_foot <- c(2,4,6,11,12,14)
highlight_cols_acute <- c(1,3,5,7,8,9,10,13)
cell_value = combined_table[3,6]
all_values <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)

# Create a nicely formatted table
my_table <- combined_table %>% 
  kable("html", align = "c") %>%
  kable_styling(full_width = F, font_size = 14) %>%
  column_spec(1, bold = T) %>%
  add_header_above(c(" " = 2, "Percentage (%)" = 10, "# of Days" = 2)) %>%
  add_header_above(c(" " = 2, "All Injury Data" = 3, "Lower Body (LB) Soft Tissue Injury (STI) Data" = 9)) %>%
  column_spec(column = highlight_cols_foot, background = "#DCDCDC", border_left = T, border_right =  T) %>% 
  column_spec(column = highlight_cols_acute, background = "#F5F5F5", border_left = T, border_right =  T) %>% 
  row_spec(17, bold = T, font_size = 14) %>% 
  row_spec(all_values, extra_css = "border-top: .5px solid") %>%
  kable_classic()


print(my_table)

# Charting Data Explanatory Variables ----

# Making Data Long to be able to Chart
foot_percentage_by_sport_long <- gather(foot_percentage_by_sport, Body_Area, Percentage, -incident_sport)
leg_percentage_by_sport_long <- gather(leg_percentage_by_sport, Body_Area, Percentage, -incident_sport)
pelvic_percentage_by_sport_long <- gather(pelvic_percentage_by_sport, Body_Area, Percentage, -incident_sport)
midsection_percentage_by_sport_long <- gather(midsection_percentage_by_sport, Body_Area, Percentage, -incident_sport)

# Bind data frames together
body_area_data_long <- bind_rows(foot_percentage_by_sport_long, leg_percentage_by_sport_long, pelvic_percentage_by_sport_long, midsection_percentage_by_sport_long)

# Overall Percentages Data frame Creation
overall_percentage_data <- data.frame(
  incident_sport = rep("Overall", 4),
  Body_Area = c("foot_percentage", "leg_percentage", "pelvic_percentage", "midsection_percentage"),
  Percentage = c(foot_percentage, leg_percentage, pelvic_percentage, midsection_percentage)
)

# Combine data frames
combined_data <- rbind(body_area_data_long, overall_percentage_data)

# Calculate a custom ordering for incident_sport
custom_order <- combined_data %>%
  filter(incident_sport != "Overall") %>%
  group_by(incident_sport) %>%
  summarise(Percentage = mean(Percentage)) %>%
  arrange(desc(Percentage))

# Reorder incident_sport based on the custom order while keeping "Overall" at the bottom
combined_data$incident_sport <- factor(
  combined_data$incident_sport,
  levels = c("Overall", rev(unique(combined_data$incident_sport)[unique(combined_data$incident_sport) != "Overall"]))
)

# Calculate the top percentage within each "Body Area" group
top_percentage_data <- combined_data %>%
  group_by(Body_Area) %>%
  top_n(1, Percentage) %>%
  ungroup()

# Create Stacked Bar Chart with Percentage Labels for the top percentage in each group
ggplot(combined_data, aes(x = incident_sport, y = Percentage, fill = Body_Area)) +
  geom_bar(stat = "identity", position = "fill", width = .5) +
  geom_bar(data = filter(combined_data, incident_sport == "Overall"),
           aes(x = incident_sport, y = Percentage, fill = Body_Area),
           stat = "identity",
           position = "fill",
           width = 1) +
  geom_text(data = top_percentage_data, aes(label = scales::percent(Percentage), hjust = c(-1.3, -.4, 2.2, 4.2)), vjust = .4, size = 5) + 
  labs(
    title = "Percentage of Soft Tissue Injuries by Body Area",
    x = "Sport",
    y = "Percentage",
    fill = "Body Area"
  ) +
  scale_fill_manual(
    values = c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00"),
    labels = c("Foot", "Leg", "Midsection", "Pelvis")
  ) +
  coord_flip() +
  scale_x_discrete(labels = function(x) ifelse(x == "Overall", expression(bold("Overall")), x)) +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5),
        text = element_text(size = 25))


# Grouped Bar Charts Data Frame Setup
overall_lbsti_data <- data.frame(
  Category = c("Lower Body", "Soft Tissue", "Soft Tissue Lower Body "),
  Percentage = c(lower_body_percentage, soft_tissue_percentage, lb_sti_percentage)
)

# Grouped Bar Chart
ggplot(overall_lbsti_data, aes(x = Category, y = Percentage, fill = Category)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Distribution of Injury Categories in Utah Athletics",
    x = NULL,
    y = "Percentage"
  ) +
  scale_fill_manual(values = c("blue", "red", "forestgreen")) +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5), legend.position = "none", axis.text.x = element_text(face = "bold", size = 11))

# Grouped Recurring Bar Charts Set Up and Chart
recurring_chart_data <- combined_table %>% 
  select(Sport, Recurring)

filtered_recurring_chart_data <- recurring_chart_data %>% 
  filter(Sport != "Overall") %>% 
  mutate(highlight = ifelse(Sport == "American Football", TRUE, FALSE))

filtered_recurring_chart_data$Recurring <- as.numeric(filtered_recurring_chart_data$Recurring)
overall_recurring_value <- recurring_chart_data$Recurring[recurring_chart_data$Sport == "Overall"]
overall_recurring_value <- as.numeric(overall_recurring_value)

ggplot(filtered_recurring_chart_data, aes(x = reorder(Sport, -Recurring), y = Recurring)) +
  geom_bar(stat = "identity", aes(fill = highlight)) +
  geom_hline(yintercept = overall_recurring_value, linetype = "dashed", color = "blue") +
  geom_text(aes(x = 14, y = overall_recurring_value, label = "Average"), hjust = 0.5, nudge_y = 0.015, color = "blue") +
  labs(
    title = "Recurring Injury Average is being pulled down by American Football ",
    x = "",
    y = "Recurring Percentage"
  ) + 
  theme_minimal() +
  scale_fill_manual(values = c("grey20", "red")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  guides(fill = "none")




# Playing with Mechanism Variable ----

# Contact
df$contact_dummy <- ifelse(df$mechanism %in% c("Contact with Ground", "Contact with Playing Device", "Contact with Other Player", 
                                                "Collision", "Tackled", "Tackling", "Contact with Object", "Contact with Ball", 
                                                "Direct Impact", "Contact with Person", "Falling - Contact", "Fall",
                                                "Contact - Other", "Landing", "Landing from Jump", "Contact with Apparatus"), 1, 0)
table(df$contact_dummy)
contact_percentage <- sum(df$contact_dummy == 1) / nrow(df)
contact_percentage

contact_percentage_by_sport <- df %>% 
  group_by(incident_sport) %>% 
  summarize(contact_percentage = sum(contact_dummy == 1) / n())
contact_percentage_by_sport


contact_percentage <- round(contact_percentage * 100, 2)
rest_of_pie <- 100 - contact_percentage

# Create a data frame for plotting
pie_data <- data.frame(group = c("Contact", "Rest"),
                       value = c(contact_percentage, rest_of_pie))

# Colors for the slices
slice_colors <- c("#BE0000", "#808080")

# Basic pie chart using ggplot2
ggplot(pie_data, aes(x = "", y = value, fill = group)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  theme_void() +
  theme(legend.position = "none") +
  # geom_text(aes(y = 0, x = 1.6, label = paste("Contact: 32.56%")), color = "black", size=6) +
  scale_fill_manual(values = slice_colors)



contact_by_sport <- as.data.frame(contact_percentage_by_sport)
contact_by_sport$contact_percentage <- contact_by_sport$contact_percentage * 100

ggplot(data = contact_by_sport, aes(x = incident_sport, y = contact_percentage)) +
  geom_col() +
  theme_minimal() +
  labs(x = "",
       y = "Contact Percentage") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels by 45 degrees

  




df$movement_dummy <- ifelse(df$mechanism %in% c("Jumping", "Running", "Change of Direction", "Change of direction (COD)", 
                                                "Sprinting", "Accelerating", "Deceleration", "Acceleration",
                                                "Cutting/Change of Direction", "Decelerating", "Sliding", "Throwing"), 1, 0)
table(df$movement_dummy)
movement_percentage <- sum(df$movement_dummy == 1) / nrow(df)
movement_percentage

movement_percentage_by_sport <- df %>% 
  group_by(incident_sport) %>% 
  summarize(movement_percentage = sum(movement_dummy == 1) / n())
movement_percentage_by_sport

# Unknown/Other
df$unknown_other_dummy <- ifelse(df$mechanism %in% c("Unknown Mechanism", "Other Non Contact", "Non-Specific", "Non Specific", 
                                                 "Non-contact","Unknown mechanism", "Unknown", "Misc","Other", "Other Biomechanical", 
                                                 "Other acute mechanism", "Weight Training"), 1, 0)
table(df$unknown_other_dummy)
unknown_other_percentage <- sum(df$unknown_other_dummy == 1) / nrow(df)
unknown_other_percentage

unknown_other_percentage_by_sport <- df %>% 
  group_by(incident_sport) %>% 
  summarize(unknown_other_percentage = sum(unknown_other_dummy == 1) / n())
unknown_other_percentage_by_sport

# Overuse
df$overuse_dummy <- ifelse(df$mechanism %in% c("Overuse", "Chronic/Overuse", "Overload", "Repetitive Trauma"), 1, 0)
table(df$overuse_dummy)
overuse_percentage <- sum(df$overuse_dummy == 1) / nrow(df)
overuse_percentage

overuse_percentage_by_sport <- df %>% 
  group_by(incident_sport) %>% 
  summarize(overuse_percentage = sum(overuse_dummy == 1) / n())
overuse_percentage_by_sport

# Connected Scatter plot chart

# Making Data Long to be able to Chart
contact_percentage_by_sport_long <- gather(contact_percentage_by_sport, Mechanism_Type, Percentage, -incident_sport)
movement_percentage_by_sport_long <- gather(movement_percentage_by_sport, Mechanism_Type, Percentage, -incident_sport)
overuse_percentage_by_sport_long <- gather(overuse_percentage_by_sport, Mechanism_Type, Percentage, -incident_sport)
unknown_other_percentage_by_sport_long <- gather(unknown_other_percentage_by_sport, Mechanism_Type, Percentage, -incident_sport)

# Bind data frames together
mechanism_data_long <- bind_rows(contact_percentage_by_sport_long, movement_percentage_by_sport_long, overuse_percentage_by_sport_long, unknown_other_percentage_by_sport_long)

# Descending Order
mechanism_data_long$incident_sport <- factor(mechanism_data_long$incident_sport, levels = rev(unique(mechanism_data_long$incident_sport)))

# Charting Scatterplot
mechanism_data_long %>% 
  ggplot(aes(x = Percentage, y = incident_sport, color = Mechanism_Type, shape = Mechanism_Type, group = Mechanism_Type)) +
  geom_point(size = 4) +
  scale_color_manual(
    values = c("contact_percentage" = "#4A6D7C", "movement_percentage" = "#5adbff", "overuse_percentage" = "#ef8354", "unknown_other_percentage" = "#E637BF"),
    labels = c("Contact", "Movement", "Overuse", "Other"),
    name = NULL
  ) +
  scale_shape_manual(
    values = c("contact_percentage" = 15, "movement_percentage" = 16, "overuse_percentage" = 17, "unknown_other_percentage" = 8),
    labels = c("Contact", "Movement", "Overuse", "Other"),
    name = NULL
  ) +
labs(
  title = "Mechanism of Injury by Sport",
  x = "Percentage",
  y = "Sport", 
  legend = "Mechanism"
) +
  scale_x_continuous(labels = scales::percent_format(scale = 100)) +
  theme_minimal() +
  theme(legend.position = "bottom", 
        legend.justification = "center",
        plot.title = element_text(hjust = 0.5, vjust = 0.5,), 
        panel.background = element_rect(fill = "white", color = NA)
  )



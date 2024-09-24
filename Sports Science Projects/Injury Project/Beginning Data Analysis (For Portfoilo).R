library(dplyr) # Data Manipulation Package %>% 
library(tidyr) # Data Cleaning Package
library(readxl)
library(tidyverse)
library(purrr)
library(lubridate)
library(ggplot2)
library(caret)
library(stargazer) # Regression Package
setwd("Path")


# Creating Force Plate Data frame ----
list_excel_files <- list.files(path = "Path", full.name = TRUE)

read_excel_file <- function(file) {
  if(is.na(file)) stop("No file path") # Testing if Path exists
df <- readxl::read_excel(file, skip = 7, col_names = TRUE)
df$`Body Weight [kg]` <- as.numeric(df$`Body Weight [kg]`)
df
}

df_list <- purrr::map(.x = list_excel_files, .f = read_excel_file)
fp <- purrr::map_dfr(.x = list_excel_files, .f = read_excel_file)

fp <- read_excel("Path")

# Rearrange the names to "Last, First" format
split_name <- strsplit(fp$Athlete, " ")
fp$Athlete <- sapply(split_name, function(name) {
  paste(rev(name), collapse = ", ")
})

# Convert the datetime column to a Date object
fp$`Test Date` <- as.Date(fp$`Test Date`)

# Format the Date object as "mm/dd/yyyy"
fp$`Test Date` <- format(fp$`Test Date`, "%m/%d/%Y")

View(fp)
rm(df_list)
rm(split_name)
# Check for Duplicates in Injury Data ----
Injury_Data <- read_excel("Path")

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


# Dropping Unused Variables in Injury Data ----



# Drop Illness Data
Injury_Data <- Injury_Data[!(Injury_Data$issue_type == "Illness"),]
unique(Injury_Data$issue_type)

# Convert the datetime column to a Date object
Injury_Data$incident_date <- as.Date(Injury_Data$incident_date)

# Format the Date object as "mm/dd/yyyy"
Injury_Data$incident_date <- format(Injury_Data$incident_date, "%m/%d/%Y")

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

# Force plate data only goes to 12/31/2021, dropping all Injury Data previous to that
df$incident_date <- as.Date(df$incident_date, format = "%m/%d/%Y")
fp$`Test Date` <- as.Date(fp$`Test Date`, format = "%m/%d/%Y")

# Drop injuries that are before fp data that we have
latest_date <- min(fp$`Test Date`)
latest_date
df <- df[df$incident_date >= latest_date, ]

# Aggregate Trials into Average for Day ----
fp$Trial <- as.numeric(gsub(".*\\s(\\d+)", "\\1", fp$Trial))

# Averaging Trials
averaged_by_trial <- fp %>% 
  group_by(Athlete, `Test Type`, `Test Date`) %>%
  select(4:ncol(.)) %>% 
  summarize(across(everything(), mean))

# Dropping datapoints that are (Deleted)
keep_rows <- !grepl("^\\(", averaged_by_trial$Athlete)

# Subset 'averaged_by_trial' to keep only the rows where 'Athlete' doesn't start with "("
averaged_by_trial <- averaged_by_trial[keep_rows, ]
                                       
# Select only Athletes who have injuries and Force Plate data ----
matching_athletes <- intersect(df$athlete_name, averaged_by_trial$Athlete)

dfuniquenames <- data.frame(athlete_name = unique(df$athlete_name))
averaged_by_trialuniquenames <- data.frame(athlete_name = unique(averaged_by_trial$Athlete))

# Split athlete names in df into first name and last name columns
df_names <- strsplit(dfuniquenames$athlete_name, " ")
dfuniquenames$last_name <- sub(",", "", sapply(df_names, "[", 1))
dfuniquenames$first_name <- sapply(df_names, "[", 2)

# Split athlete names in averaged_by_trial into first name and last name columns
averaged_by_trial_names <- strsplit(averaged_by_trialuniquenames$athlete_name, " ")
averaged_by_trialuniquenames$last_name <- sub(",", "", sapply(averaged_by_trial_names, "[", 1))
averaged_by_trialuniquenames$first_name <- sapply(averaged_by_trial_names, "[", 2)

matching_last_names <- intersect(dfuniquenames$last_name, averaged_by_trialuniquenames$last_name)

# Limit dfuniquenames to common last names
dfuniquenames_filtered <- subset(dfuniquenames, last_name %in% matching_last_names)

# Limit averaged_by_trialuniquenames to common last names
averaged_by_trialuniquenames_filtered <- subset(averaged_by_trialuniquenames, last_name %in% matching_last_names)

diff_names <- setdiff(dfuniquenames_filtered$athlete_name, averaged_by_trialuniquenames_filtered$athlete_name)
diff_names2 <- setdiff(averaged_by_trialuniquenames_filtered$athlete_name, dfuniquenames_filtered$athlete_name)

dfuniquenames_filtered <- dfuniquenames_filtered[!(dfuniquenames_filtered$athlete_name %in% diff_names), ]
averaged_by_trialuniquenames_filtered <- averaged_by_trialuniquenames_filtered[!(averaged_by_trialuniquenames_filtered$athlete_name %in% diff_names2), ]
 
# Check Work
differences_between_dataframes <- setdiff(dfuniquenames_filtered$athlete_name, averaged_by_trialuniquenames_filtered$athlete_name)
differences_between_dataframes




# Format Dataframes to keep these names only
df <- df[df$athlete_name %in% dfuniquenames_filtered$athlete_name, ]
averaged_by_trial <- averaged_by_trial[averaged_by_trial$Athlete %in% averaged_by_trialuniquenames_filtered$athlete_name, ]

# Checking Work
checkwork <- setdiff(unique(averaged_by_trial$Athlete), unique(df$athlete_name))
checkwork

break
# Loop for all Athletes (Peak Power) ----


# Create an empty list to store results for all athletes
all_results_1month_before <- list()
all_results_6months_before <- list()

# List of athlete names
athlete_names <- df$athlete_name

# Loop through each athlete
for (athlete_name in athlete_names) {
  # Subset the injury data for the current athlete
  athlete_injury_data <- df[df$athlete_name == athlete_name, ]
  
  # Subset the performance data for the current athlete
  athlete_fp_data <- averaged_by_trial[averaged_by_trial$Athlete == athlete_name, ]
  
  if (nrow(athlete_fp_data) == 0) {
    cat("No observations found for athlete:", athlete_name, "\n")
    next  # Skip this athlete and continue with the next athlete
  }
  
  # Initialize empty data frames to store results
  results_1month_before <- data.frame(Athlete = character(0), Injury_Type = character(0), Injury_Date = character(0), Test_Date = character(0), Peak_Power_1month_before = numeric(0))
  results_6months_before <- data.frame(Athlete = character(0), Injury_Date = character(0), Test_Date = character(0), Peak_Power_6months_before = numeric(0))
  
  # Loop through each row in the athlete's injury data
  for (i in 1:nrow(athlete_injury_data)) {
    injury_day <- as.Date(athlete_injury_data$incident_date[i])  # Convert to Date format
    
    # Calculate the date 1 month before the injury date
    date_one_month_before <- injury_day - days(30)
    
    # Calculate the date 6 months before the injury date
    date_six_months_before <- date_one_month_before - months(6)
    
    # Initialize variables to store values for the current injury
    peak_power_1month_before <- numeric(0)
    test_dates_1month_before <- character(0)
    peak_power_6months_before <- numeric(0)
    test_dates_6months_before <- character(0)
    
    # Loop through each row in the athlete's performance data
    for (j in 1:nrow(athlete_fp_data)) {
      test_date <- as.Date(athlete_fp_data$`Test Date`[j])  # Convert to Date format
      
      # Check if the test date is not missing and is within 1 month before the injury date
      if (!is.na(test_date) && !is.na(date_one_month_before) && !is.na(injury_day) && test_date >= date_one_month_before && test_date < injury_day) {
        peak_power_1month_before <- c(peak_power_1month_before, athlete_fp_data$`Peak Power / BM [W/kg]`[j])
        test_dates_1month_before <- c(test_dates_1month_before, format(test_date, format = "%Y-%m-%d"))
      }
      
      # Check if the test date is not missing and is within 6 months before the injury date
      if (!is.na(test_date) && !is.na(date_six_months_before) && !is.na(injury_day) && test_date >= date_six_months_before && test_date < injury_day) {
        peak_power_6months_before <- c(peak_power_6months_before, athlete_fp_data$`Peak Power / BM [W/kg]`[j])
        test_dates_6months_before <- c(test_dates_6months_before, format(test_date, format = "%Y-%m-%d"))
      }
    }
    
    # Append the results for the current injury to the data frames
    if (length(peak_power_1month_before) > 0) {
      results_1month_before <- rbind(results_1month_before, data.frame(Athlete = athlete_injury_data$athlete_name[i], 
                                                                       Injury_Type = athlete_injury_data$body_area[i],
                                                                       Injury_Date = format(injury_day, format = "%Y-%m-%d"), 
                                                                       Test_Date = test_dates_1month_before, 
                                                                       Peak_Power_1month_before = peak_power_1month_before))
    }
    
    if (length(peak_power_6months_before) > 0) {
      results_6months_before <- rbind(results_6months_before, data.frame(Athlete = athlete_injury_data$athlete_name[i], 
                                                                         Injury_Date = format(injury_day, format = "%Y-%m-%d"), 
                                                                         Test_Date = test_dates_6months_before, 
                                                                         Peak_Power_6months_before = mean(peak_power_6months_before)))
    }
  }

  # Store the results for the current athlete in the list
  all_results_1month_before[[athlete_name]] <- results_1month_before
  all_results_6months_before[[athlete_name]] <- results_6months_before
}






# Trying New Style of Z-Scores ----

# Remove all data frames with 1 or less observations in the 1 month period
all_results_1month_before <- all_results_1month_before[sapply(all_results_1month_before, function(df) nrow(df) > 1)]

# Mirror this removal in the 6 month data frame
selected_names <- names(all_results_1month_before)
all_results_6months_before <- all_results_6months_before[(names(all_results_6months_before) %in% selected_names)]

# Create an empty list to store the merged data frames
combined_data_frames <- list()

# Iterate through the indices of data frames in the lists
for (i in 1:length(all_results_1month_before)) {
  df1 <- all_results_1month_before[[i]]
  df2 <- all_results_6months_before[[i]]
  
  # Merge data frames based on Athlete, Injury_Date, and Test_Date
  merged_df <- merge(df1, df2, by = c("Athlete", "Injury_Date", "Test_Date"))
  
  # Rename the column from "Peak_Power_1month_before" to "Peak_Power_Test_Date"
  colnames(merged_df)[colnames(merged_df) == "Peak_Power_1month_before"] <- "Peak_Power_Test_Date"
  
  # Append the merged data frame to the combined_data_frames list
  combined_data_frames[[i]] <- merged_df
}

# Create a % away from the 6 month average for each Injury (Using Z-Score)
fully_combined_data <- bind_rows(combined_data_frames)

testing_fcd <- fully_combined_data

# Time difference between Injury and Test
testing_fcd$time_between_injury_test <- floor(as.numeric(difftime(testing_fcd$Injury_Date, testing_fcd$Test_Date)))

# Fit the regression model
z_score_test_model <- lm(Peak_Power_Test_Date ~ Peak_Power_6months_before + time_between_injury_test, data = testing_fcd)

# Print the regression summary
summary(z_score_test_model)

predicted_values <- predict(z_score_test_model)
residuals <- residuals(z_score_test_model)
# Calculate z-scores
z_scores <- scale(residuals)

# Add z-scores to the original data frame
testing_fcd$Z_Score <- z_scores
summary(testing_fcd$Z_Score)


# Group a mean z score by groups of 3 days
testing_fcd <- testing_fcd %>% 
  mutate(grouped_time = (time_between_injury_test - 1) %/% 3) %>%
  group_by(grouped_time) %>% 
  mutate(avg_z_score = mean(Z_Score)) %>% 
  ungroup()

# Chart
ggplot(testing_fcd, aes(x = time_between_injury_test, y = avg_z_score)) + 
  geom_line(y=0, color = "blue", linewidth = .75) +
  geom_line(linewidth = 1) +
  labs(
    x = "Days before Injury",
    y = "Z-Score",
    title = "Average Peak Power a month before an Injury Occurred",
    subtitle = "Grouped and Averaged in 3 day increments"
  ) +
  scale_x_reverse() +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.5))






# Selecting Just Ankle Injuries for Peak Power----
# Time difference between Injury and Test
ankle_fully_combined_data <- testing_fcd %>% 
  filter(Injury_Type == "Ankle")

ankle_fully_combined_data <- ankle_fully_combined_data %>% 
  mutate(grouped_time = (time_between_injury_test - 1) %/% 3) %>%
  group_by(grouped_time) %>% 
  mutate(avg_z_score = mean(Z_Score)) %>% 
  ungroup()

ggplot(ankle_fully_combined_data, aes(x = time_between_injury_test, y = avg_z_score)) + 
  geom_line(y=0, color = "blue", linewidth = .75) +
  geom_line(linewidth = 1) +
  labs(
    x = "Days before Injury",
    y = "Z-Score",
    title = "Average Peak Power a month before an Ankle Injury Occurred",
    subtitle = "Grouped and Averaged in 3 day increments"
  ) +
  scale_x_reverse() +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.5))


# Chart of Peak Power pre Injury by Injury Type ----

# Initialize an empty list to store the results for each injury type
list_of_injury_data <- list()

Injury_Types <- unique(testing_fcd$Injury_Type)

# Loop through each injury type
for (i in Injury_Types){
  
  # Filter the data for the current injury type
  looped_data <- testing_fcd %>% 
    filter(Injury_Type == i)  
  
  # Calculate the grouped time and average z-score
  looped_data <- looped_data %>%
    mutate(grouped_time = (time_between_injury_test - 1) %/% 3) %>%
    group_by(grouped_time) %>% 
    mutate(avg_z_score = mean(Z_Score)) %>% 
    ungroup()
  
  # Store the results for the current injury type in the list
  list_of_injury_data[[i]] <- looped_data
}

# Combine the results from the list into a single data frame
fully_combined_data_by_Injury <- bind_rows(list_of_injury_data, .id = "Injury_Type")


# Charting
ggplot(fully_combined_data_by_Injury, aes(x = time_between_injury_test, y = avg_z_score, color = Injury_Type)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue", size = 0.75) +
  labs(title = "Average Z-Score for Peak Power by Injury Type",
       subtitle = "Grouped and Averaged in 3 day increments",
       x = "Days before Injury",
       y = "Average Z-Score") +
  scale_color_manual(values = c("darkgray", "darkgray", "darkgray", "darkgray", "darkgray", "darkgray", "#BF40BF", "darkgray", "#FF0000"), name = "Injury Type") +
  scale_x_reverse() +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.5),
        legend.title = element_text(face = "bold"))


# Regression Analysis ----
regression_df <- df

# Dummy Variable for Injury Types
regression_df <- regression_df %>%
  mutate(Foot_Injury = ifelse(body_area == "Ankle" | body_area == "Foot", 1, 0))

regression_df <- regression_df %>%
  mutate(Leg_Injury = ifelse(body_area == "Lower Leg" | body_area == "Knee" | body_area == "Thigh", 1, 0))

regression_df <- regression_df %>%
  mutate(Pelvic_Injury = ifelse(body_area == "Buttock/pelvis" | body_area == "Hip/Groin", 1, 0))

regression_df <- regression_df %>%
  mutate(Midsection_Injury = ifelse(body_area == "Trunk/Abdominal" | body_area == "Lumbar Spine", 1, 0))

# Mechanism Dummy Variables
regression_df$unknown_other_dummy <- ifelse(df$mechanism %in% c("Unknown Mechanism", "Other Non Contact", "Non-Specific", "Non Specific", 
                                                                "Non-contact","Unknown mechanism", "Unknown", "Misc","Other", "Other Biomechanical", 
                                                                "Other acute mechanism", "Weight Training", "Repetitive Trauma"), 1, 0)

regression_df$movement_dummy <- ifelse(df$mechanism %in% c("Jumping", "Running", "Change of Direction", "Change of direction (COD)", 
                                                           "Sprinting", "Accelerating", "Deceleration", "Acceleration",
                                                           "Cutting/Change of Direction", "Decelerating", "Sliding", "Throwing"), 1, 0)

regression_df$player_contact_dummy <- ifelse(df$mechanism %in% c("Contact with Other Player", "Collision", "Tackled", "Tackling",  
                                                                 "Contact with Person", "Contact - Other" ), 1, 0)

regression_df$separate_contact_dummy <- ifelse(df$mechanism %in% c("Contact with Ground", "Contact with Playing Device", "Contact with Object", "Contact with Ball",
                                                                   "Direct Impact", "Falling - Contact", "Fall", "Landing", "Landing from Jump", "Contact with Apparatus"), 1, 0)

# Chronic vs Acute Dummy Variable
regression_df <- regression_df %>% 
  mutate(Acute_Injury = ifelse(issue_occurrence == "Acute", 1, 0))

# Sport Dummy Variables
unique_sports <- unique(regression_df$incident_sport)

for (i in unique_sports) {
  regression_df <- regression_df %>% 
    mutate(!!paste0("sportdummy_", i) := as.numeric(incident_sport == i))
}

# Season Dummy Variables
unique_season <- unique(regression_df$incident_season)

for (i in unique_season) {
  regression_df <- regression_df %>% 
    mutate(!!paste0("seasondummy_", i) := as.numeric(incident_season == i))
}

# Drop unwanted column
regression_df <- regression_df %>%
  select(-seasondummy_NA)

# Selecting values to omit from regression as our base cases
regression_df <- regression_df %>%
  select(-`seasondummy_Off-Season`)

# Run Regression
subset_df <- select(regression_df, Foot_Injury, Leg_Injury, Pelvic_Injury, Midsection_Injury, Acute_Injury, movement_dummy, player_contact_dummy, separate_contact_dummy, starts_with("sportdummy"), starts_with("seasondummy"))
model <- lm(Foot_Injury ~ ., data = subset_df)



# Function for regression results, by sport with Contact, Acute, and Time of Year variables
regression_by_injury <- function(regressed_injury) {
  model_list <- list()
  list_of_sports <- grep("sportdummy_", names(subset_df), value = TRUE)
  
  for (i in list_of_sports) {
    sport_subset <- subset_df %>% 
      filter(!!as.name(i) == 1)
    
    # Check if all required variables are present in the subset
    required_vars <- c("Acute_Injury", "movement_dummy", "player_contact_dummy", "separate_contact_dummy",
                       "seasondummy_Non-Traditional In-Season", "seasondummy_Post-Season",
                       "seasondummy_Pre-Season", "seasondummy_In-Season (Traditional)")
    
    if(all(required_vars %in% colnames(sport_subset))) {
      # Use backticks around variable names with spaces
      formula_string <- paste(regressed_injury, "~", paste0("`", required_vars, "`", collapse = " + "))
      
      i_model <- lm(as.formula(formula_string), data = sport_subset)
      
      # Store the model in the list
      model_list[[i]] <- i_model
    } else {
      warning("Not all required variables are present in the subset.")
    }
  }
  
  return(model_list)
}

# Running Functions
foot_regression <- regression_by_injury(regressed_injury = "Foot_Injury")
leg_regression <- regression_by_injury(regressed_injury = "Leg_Injury")
pelvic_regression <- regression_by_injury(regressed_injury = "Pelvic_Injury")
midsection_regression <- regression_by_injury(regressed_injury = "Midsection_Injury")


stargazer(foot_regression, title = "Foot Regressions by Sport", type = "html", out = "foot_regression_results.html", intercept.top = TRUE, intercept.bottom = FALSE, ci = TRUE, ci.level = .90, df = F, omit.stat = c("f"), dep.var.caption = "", dep.var.labels = "", align = T,
          column.labels = c("Soccer", "Basketball", "Softball", "Swimming", "Track", "Football", "Volleyball", "Tennis", "Lacrosse", "Gymnastics", "Skiing", "Baseball", "Cross Country", "Diving", "Beach Volleyball"))

stargazer(leg_regression, title = "Leg Regressions by Sport", type = "html", out = "leg_regression_results.html", intercept.top = TRUE, intercept.bottom = FALSE, ci = TRUE, ci.level = .90, df = F, omit.stat = c("f"), dep.var.caption = "", dep.var.labels = "", align = T,
          column.labels = c("Soccer", "Basketball", "Softball", "Swimming", "Track", "Football", "Volleyball", "Tennis", "Lacrosse", "Gymnastics", "Skiing", "Baseball", "Cross Country", "Diving", "Beach Volleyball"))

stargazer(pelvic_regression, title = "Pelvic Regressions by Sport", type = "html", out = "pelvic_regression_results.html", intercept.top = TRUE, intercept.bottom = FALSE, ci = TRUE, ci.level = .90, df = F, omit.stat = c("f"), dep.var.caption = "", dep.var.labels = "", align = T,
          column.labels = c("Soccer", "Basketball", "Softball", "Swimming", "Track", "Football", "Volleyball", "Tennis", "Lacrosse", "Gymnastics", "Skiing", "Baseball", "Cross Country", "Diving", "Beach Volleyball"))

stargazer(midsection_regression, title = "Midsection Regressions by Sport", type = "html", out = "midsection_regression_results.html", intercept.top = TRUE, intercept.bottom = FALSE, ci = TRUE, ci.level = .90, df = F, omit.stat = c("f"), dep.var.caption = "", dep.var.labels = "", align = T, 
          column.labels = c("Soccer", "Basketball", "Softball", "Swimming", "Track", "Football", "Volleyball", "Tennis", "Lacrosse", "Gymnastics", "Skiing", "Baseball", "Cross Country", "Diving", "Beach Volleyball"))

# Combine Files into One
regression_file_names <- c("midsection_regression_results.html", "pelvic_regression_results.html", "leg_regression_results.html", "foot_regression_results.html")
html_contents <- lapply(regression_file_names, readLines, warn = FALSE)

# Write the combined contents to a new HTML file
writeLines(unlist(html_contents), "combined_results.html")

browseURL("combined_results.html")
# Testing Code that is not used listed below ------------------------------------------------------------ ----
# Create Data frame with FP jumps within 1 month of injury (Testing for Athlete_1) ----

Athlete_1_fp_data <- averaged_by_trial %>% 
  filter(Athlete == "Athlete_1")

Athlete_1_injury_data <- df %>% 
  filter(athlete_name == "Athlete_1")

# Assuming your date column is in the format "MM/DD/YYYY"
Athlete_1_injury_data$incident_date <- as.Date(Athlete_1_injury_data$incident_date, format = "%m/%d/%Y")
Athlete_1_fp_data$Test_date <- as.Date(Athlete_1_fp_data$`Test Date`, format = "%m/%d/%Y")

# Initialize empty data frames to store results
results_1month_before <- data.frame(Athlete = character(0), Injury_Date = character(0), Test_Date = character(0), Peak_Power_1month_before = numeric(0))
results_6months_before <- data.frame(Athlete = character(0), Injury_Date = character(0), Test_Date = character(0), Peak_Power_6months_before = numeric(0))

# Loop through each row in Athlete_1_injury_data
for (i in 1:nrow(Athlete_1_injury_data)) {
  injury_day <- as.Date(Athlete_1_injury_data$incident_date[i])  # Convert to Date format
  
  # Calculate the date 1 month before the injury date
  date_one_month_before <- injury_day - days(30)
  
  # Calculate the date 6 months before the injury date
  date_six_months_before <- date_one_month_before - months(5)
  
  # Initialize variables to store values for the current injury
  peak_power_1month_before <- numeric(0)
  test_dates_1month_before <- character(0)
  peak_power_6months_before <- numeric(0)
  test_dates_6months_before <- character(0)
  
  # Loop through each row in Athlete_1_fp_data
  for (j in 1:nrow(Athlete_1_fp_data)) {
    test_date <- as.Date(Athlete_1_fp_data$Test_date[j])  # Convert to Date format
    
    # Check if the test date is within 1 month before the injury date
    if (test_date >= date_one_month_before && test_date < injury_day) {
      peak_power_1month_before <- c(peak_power_1month_before, Athlete_1_fp_data$`Peak Power / BM [W/kg]`[j])
      test_dates_1month_before <- c(test_dates_1month_before, format(test_date, format = "%Y-%m-%d"))
    }
    
    # Check if the test date is within 6 months before the injury date
    if (test_date >= date_six_months_before && test_date < injury_day) {
      peak_power_6months_before <- c(peak_power_6months_before, Athlete_1_fp_data$`Peak Power / BM [W/kg]`[j])
      test_dates_6months_before <- c(test_dates_6months_before, format(test_date, format = "%Y-%m-%d"))
    }
  }
  
  # Append the results for the current injury to the data frames
  if (length(peak_power_1month_before) > 0) {
    results_1month_before <- rbind(results_1month_before, data.frame(Athlete = Athlete_1_injury_data$athlete_name[i], Injury_Date = format(injury_day, format = "%Y-%m-%d"), Test_Date = test_dates_1month_before, Peak_Power_1month_before = peak_power_1month_before))
  }
  
  if (length(peak_power_6months_before) > 0) {
    results_6months_before <- rbind(results_6months_before, data.frame(Athlete = Athlete_1_injury_data$athlete_name[i], Injury_Date = format(injury_day, format = "%Y-%m-%d"), Peak_Power_6months_before = mean(peak_power_6months_before)))
  }
}








# Min Max Standardization Trials ----
# Chart of all z-scores to look at distribution
ggplot(fully_combined_data, aes(x = time_between_injury_test, y = z_score)) +
  geom_point()


# Min Max Function
minMax <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

testingdf <- fully_combined_data


column_to_normalize <- "Peak_Power_Test_Date"
testingdf <- testingdf %>%
  mutate(!!paste0(column_to_normalize, "_normalized") := minMax(testingdf[[column_to_normalize]]))

column_to_normalize <- "Peak_Power_6months_before"
testingdf <- testingdf %>%
  mutate(!!paste0(column_to_normalize, "_normalized") := minMax(testingdf[[column_to_normalize]]))

testingdf <- testingdf %>% 
  mutate(grouped_time = (time_between_injury_test - 1) %/% 3) %>%
  group_by(grouped_time) %>% 
  mutate(avg_Peak_Power_Test_Date_normalized = mean(Peak_Power_Test_Date_normalized)) %>% 
  ungroup()

ggplot(testingdf, aes(x = time_between_injury_test, y = avg_Peak_Power_Test_Date_normalized)) +
  geom_line()


# Loop for all Athletes (RSI) ----


# Create an empty list to store results for all athletes
RSI_all_results_1month_before <- list()
RSI_all_results_6months_before <- list()

# List of athlete names
athlete_names <- df$athlete_name

# Loop through each athlete
for (athlete_name in athlete_names) {
  # Subset the injury data for the current athlete
  athlete_injury_data <- df[df$athlete_name == athlete_name, ]
  
  # Subset the performance data for the current athlete
  athlete_fp_data <- averaged_by_trial[averaged_by_trial$Athlete == athlete_name, ]
  
  if (nrow(athlete_fp_data) == 0) {
    cat("No observations found for athlete:", athlete_name, "\n")
    next  # Skip this athlete and continue with the next athlete
  }
  
  # Initialize empty data frames to store results
  results_1month_before <- data.frame(Athlete = character(0), Injury_Type = character(0), Issue_Occurrence = character(0), Mechanism = character(0), 
                                      Season = character(0), Injury_Date = character(0), Test_Date = character(0), RSI_1month_before = numeric(0))
  results_6months_before <- data.frame(Athlete = character(0), Injury_Date = character(0), Test_Date = character(0), RSI_6months_before = numeric(0))
  
  # Loop through each row in the athlete's injury data
  for (i in 1:nrow(athlete_injury_data)) {
    injury_day <- as.Date(athlete_injury_data$incident_date[i])  # Convert to Date format
    
    # Calculate the date 1 month before the injury date
    date_one_month_before <- injury_day - days(30)
    
    # Calculate the date 6 months before the injury date
    date_six_months_before <- date_one_month_before - months(6)
    
    # Initialize variables to store values for the current injury
    RSI_1month_before <- numeric(0)
    test_dates_1month_before <- character(0)
    RSI_6months_before <- numeric(0)
    test_dates_6months_before <- character(0)
    
    # Loop through each row in the athlete's performance data
    for (j in 1:nrow(athlete_fp_data)) {
      test_date <- as.Date(athlete_fp_data$`Test Date`[j])  # Convert to Date format
      
      # Check if the test date is not missing and is within 1 month before the injury date
      if (!is.na(test_date) && !is.na(date_one_month_before) && !is.na(injury_day) && test_date >= date_one_month_before && test_date < injury_day) {
        RSI_1month_before <- c(RSI_1month_before, athlete_fp_data$`RSI-modified [m/s]`[j])
        test_dates_1month_before <- c(test_dates_1month_before, format(test_date, format = "%Y-%m-%d"))
      }
      
      # Check if the test date is not missing and is within 6 months before the injury date
      if (!is.na(test_date) && !is.na(date_six_months_before) && !is.na(injury_day) && test_date >= date_six_months_before && test_date < injury_day) {
        RSI_6months_before <- c(RSI_6months_before, athlete_fp_data$`RSI-modified [m/s]`[j])
        test_dates_6months_before <- c(test_dates_6months_before, format(test_date, format = "%Y-%m-%d"))
      }
    }
    
    # Append the results for the current injury to the data frames
    if (length(RSI_1month_before) > 0) {
      results_1month_before <- rbind(results_1month_before, data.frame(Athlete = athlete_injury_data$athlete_name[i], 
                                                                       Injury_Type = athlete_injury_data$body_area[i],
                                                                       Issue_Occurrence = athlete_injury_data$issue_occurrence[i],
                                                                       Mechanism = athlete_injury_data$mechanism[i],
                                                                       Season = athlete_injury_data$incident_season[i],
                                                                       Injury_Date = format(injury_day, format = "%Y-%m-%d"), 
                                                                       Test_Date = test_dates_1month_before, 
                                                                       RSI_1month_before = RSI_1month_before))
    }
    
    if (length(RSI_6months_before) > 0) {
      results_6months_before <- rbind(results_6months_before, data.frame(Athlete = athlete_injury_data$athlete_name[i], 
                                                                         Injury_Date = format(injury_day, format = "%Y-%m-%d"), 
                                                                         Test_Date = test_dates_6months_before, 
                                                                         RSI_6months_before = mean(RSI_6months_before)))
    }
  }
  
  # Store the results for the current athlete in the list
  RSI_all_results_1month_before[[athlete_name]] <- results_1month_before
  RSI_all_results_6months_before[[athlete_name]] <- results_6months_before
}





# Next Step ----

# Remove all data frames with 1 or less observations in the 1 month period
RSI_all_results_1month_before <- RSI_all_results_1month_before[sapply(RSI_all_results_1month_before, function(df) nrow(df) > 1)]

# Mirror this removal in the 6 month data frame
selected_names <- names(RSI_all_results_1month_before)
RSI_all_results_6months_before <- RSI_all_results_6months_before[(names(RSI_all_results_6months_before) %in% selected_names)]

# Create an empty list to store the merged data frames
combined_data_frames <- list()

# Iterate through the indices of data frames in the lists
for (i in 1:length(RSI_all_results_1month_before)) {
  df1 <- RSI_all_results_1month_before[[i]]
  df2 <- RSI_all_results_6months_before[[i]]
  
  # Merge data frames based on Athlete, Injury_Date, and Test_Date
  merged_df <- merge(df1, df2, by = c("Athlete", "Injury_Date", "Test_Date"))
  
  # Rename the column from "Peak_Power_1month_before" to "Peak_Power_Test_Date"
  colnames(merged_df)[colnames(merged_df) == "RSI_1month_before"] <- "RSI_Test_Date"
  
  # Append the merged data frame to the combined_data_frames list
  combined_data_frames[[i]] <- merged_df
}

# Create a % away from the 6 month average for each Injury (Using Z-Score)
rsi_testing_fcd <- bind_rows(combined_data_frames)

# Time difference between Injury and Test
rsi_testing_fcd$time_between_injury_test <- floor(as.numeric(difftime(rsi_testing_fcd$Injury_Date, rsi_testing_fcd$Test_Date)))

# Fit the regression model
z_score_test_model <- lm(RSI_Test_Date ~ RSI_6months_before + time_between_injury_test, data = rsi_testing_fcd)
# Print the regression summary
summary(z_score_test_model)

# Calculate z-scores
predicted_values <- predict(z_score_test_model)
residuals <- residuals(z_score_test_model)
z_scores <- scale(residuals)

# Add z-scores to the original data frame
rsi_testing_fcd$Z_Score <- z_scores
summary(rsi_testing_fcd$Z_Score)

# Group a mean z score by groups of 3 days
rsi_testing_fcd <- rsi_testing_fcd %>% 
  mutate(grouped_time = (time_between_injury_test - 1) %/% 3) %>%
  group_by(grouped_time) %>% 
  mutate(avg_z_score = mean(Z_Score)) %>% 
  ungroup()

# Chart
ggplot(rsi_testing_fcd, aes(x = time_between_injury_test, y = avg_z_score)) + 
  geom_line(y=0, color = "blue", linewidth = .75) +
  geom_line(linewidth = 1) +
  labs(
    x = "Days before Injury",
    y = "Z-Score",
    title = "Average Z-Score for RSI Modified a month before an Injury Occurred",
    subtitle = "Grouped and Averaged in 3 day increments",
  ) +
  scale_x_reverse() +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.5),
        legend.title = element_text(face = "bold"))


# Selecting Just Ankle Injuries for RSI ----
# Time difference between Injury and Test
rsi_ankle_fully_combined_data <- rsi_testing_fcd %>% 
  filter(Injury_Type == "Ankle")

rsi_ankle_fully_combined_data <- rsi_ankle_fully_combined_data %>% 
  mutate(grouped_time = (time_between_injury_test - 1) %/% 3) %>%
  group_by(grouped_time) %>% 
  mutate(avg_z_score = mean(Z_Score)) %>% 
  ungroup()

ggplot(rsi_ankle_fully_combined_data, aes(x = time_between_injury_test, y = avg_z_score)) + 
  geom_line(y=0, color = "blue", linewidth = .75) +
  geom_line(linewidth = 1) +
  labs(
    x = "Days before Injury",
    y = "Z-Score",
    title = "Average Z-Score for RSI Modified a month before an Ankle Injury Occurred",
    subtitle = "Grouped and Averaged in 3 day increments",
  ) +
  scale_x_reverse() +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.5),
        legend.title = element_text(face = "bold"))


# Chart of RSI pre Injury by Injury Type ----

# Initialize an empty list to store the results for each injury type
list_of_injury_data <- list()

Injury_Types <- unique(rsi_testing_fcd$Injury_Type)

# Loop through each injury type
for (i in Injury_Types){
  
  # Filter the data for the current injury type
  rsi_looped_data <- rsi_testing_fcd %>% 
    filter(Injury_Type == i)  
  
  # Calculate the grouped time and average z-score
  rsi_looped_data <- rsi_looped_data %>%
    mutate(grouped_time = (time_between_injury_test - 1) %/% 3) %>%
    group_by(grouped_time) %>% 
    mutate(avg_z_score = mean(Z_Score)) %>% 
    ungroup()
  
  # Store the results for the current injury type in the list
  list_of_injury_data[[i]] <- rsi_looped_data
}

# Combine the results from the list into a single data frame
rsi_fully_combined_data_by_Injury <- bind_rows(list_of_injury_data, .id = "Injury_Type")


# Charting
ggplot(rsi_fully_combined_data_by_Injury, aes(x = time_between_injury_test, y = avg_z_score, color = Injury_Type)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue", size = 0.75) +
  labs(title = "Average Z-Score Comparison for RSI Modified",
       x = "Athlete",
       y = "Average Z-Score") +
  scale_color_manual(values = c("#D3D3D3", "#D3D3D3", "#FF0000", "#D3D3D3", "#D3D3D3", "#D3D3D3", "#D3D3D3", "#D3D3D3", "#D3D3D3")) +
  scale_x_reverse() +
  theme_classic()

# Chart of RSI pre Injury by Injury Type (4 Groups) ----

# Mechanism Dummy Variables
rsi_testing_fcd$unknown_other_dummy <- ifelse(rsi_testing_fcd$Mechanism %in% c("Unknown Mechanism", "Other Non Contact", "Non-Specific", "Non Specific", 
                                                                "Non-contact","Unknown mechanism", "Unknown", "Misc","Other", "Other Biomechanical", 
                                                                "Other acute mechanism", "Weight Training", "Repetitive Trauma"), 1, 0)

rsi_testing_fcd$movement_dummy <- ifelse(rsi_testing_fcd$Mechanism %in% c("Jumping", "Running", "Change of Direction", "Change of direction (COD)", 
                                                           "Sprinting", "Accelerating", "Deceleration", "Acceleration",
                                                           "Cutting/Change of Direction", "Decelerating", "Sliding", "Throwing"), 1, 0)

rsi_testing_fcd$player_contact_dummy <- ifelse(rsi_testing_fcd$Mechanism %in% c("Contact with Other Player", "Collision", "Tackled", "Tackling",  
                                                                 "Contact with Person", "Contact - Other" ), 1, 0)

rsi_testing_fcd$separate_contact_dummy <- ifelse(rsi_testing_fcd$Mechanism %in% c("Contact with Ground", "Contact with Playing Device", "Contact with Object", "Contact with Ball",
                                                                   "Direct Impact", "Falling - Contact", "Fall", "Landing", "Landing from Jump", "Contact with Apparatus"), 1, 0)


# Dummy Variable for Injury Types
rsi_testing_fcd <- rsi_testing_fcd %>%
  mutate(Foot_Injury = ifelse(Injury_Type == "Ankle" | Injury_Type == "Foot", 1, 0))

rsi_testing_fcd <- rsi_testing_fcd %>%
  mutate(Leg_Injury = ifelse(Injury_Type == "Lower Leg" | Injury_Type == "Knee" | Injury_Type == "Thigh", 1, 0))

rsi_testing_fcd <- rsi_testing_fcd %>%
  mutate(Pelvic_Injury = ifelse(Injury_Type == "Buttock/pelvis" | Injury_Type == "Hip/Groin", 1, 0))

rsi_testing_fcd <- rsi_testing_fcd %>%
  mutate(Midsection_Injury = ifelse(Injury_Type == "Trunk/Abdominal" | Injury_Type == "Lumbar Spine", 1, 0))


Injury_Groups <- c("Foot_Injury", "Leg_Injury", "Pelvic_Injury", "Midsection_Injury")
list_of_injury_data <- list()

# Loop through each injury type
for (i in Injury_Groups){
  
  # Filter the data for the current injury type
  rsi_looped_data <- rsi_testing_fcd %>% 
    filter(get(i) == 1)  
  
  # Calculate the grouped time and average z-score
  rsi_looped_data <- rsi_looped_data %>%
    mutate(grouped_time = (time_between_injury_test - 1) %/% 3) %>%
    group_by(grouped_time) %>% 
    mutate(avg_z_score = mean(Z_Score)) %>% 
    ungroup()
  
  # Store the results for the current injury type in the list
  list_of_injury_data[[i]] <- rsi_looped_data
}

# Combine the results from the list into a single data frame
rsi_fully_combined_data_by_Injury <- bind_rows(list_of_injury_data, .id = "Injury_Groups")

# Observations by group and Injury
observation_counts <- rsi_fully_combined_data_by_Injury %>%
  group_by(Injury_Groups, grouped_time) %>%
  summarise(observation_count = n())

rsi_fully_combined_data_by_Injury <- merge(rsi_fully_combined_data_by_Injury, observation_counts, by = c("Injury_Groups", "grouped_time"), all.x = TRUE)

rsi_fully_combined_data_by_Injury <- rsi_fully_combined_data_by_Injury %>% 
  mutate(grouped_time = 3 * grouped_time)

# Charting
ggplot(rsi_fully_combined_data_by_Injury, aes(x = time_between_injury_test, y = avg_z_score, color = Injury_Groups)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#0000FF", size = 0.75) +
  labs(
    x = "Days before Injury",
    y = "Z-Score",
    title = "Average Z-Score for RSI Modified by Injury Type",
    subtitle = "Grouped and Averaged in 3 day increments",
  ) +
  scale_color_manual(values = c("#0A5C36", "darkgray", "#FF0000", "darkgray")) +
  scale_x_reverse() +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.5),
        legend.title = element_text(face = "bold"))

# Create a bar chart with summarized data
ggplot(observation_counts, aes(x = grouped_time, y = observation_count, fill = Injury_Groups)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 1) +
  labs(title = "Observation Counts Over Time and by Injury Type",
       x = "Days Before Injury",
       y = "Observation Counts") +
  scale_fill_manual(values = c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00"),
                    labels = c("Foot", "Leg", "Midsection", "Pelvis"),
                    name = "Injury Type") +
  scale_x_reverse() +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5, vjust = 0.5),
        legend.title = element_text(face = "bold"))

messing_with_rsi <- rsi_fully_combined_data_by_Injury

messing_with_rsi <- messing_with_rsi %>%
  filter(Foot_Injury == 1) %>%
  summarise(foot_mean_avg_z_score = mean(avg_z_score, na.rm = TRUE))

messing_with_rsi <- messing_with_rsi %>%
  filter(Midsection_Injury == 1) %>%
  summarise(Midsection_mean_avg_z_score = mean(avg_z_score, na.rm = TRUE))

# FlightTime:Eccentric Duration Potential good metric ----
# Next Step (Peak Power) ----

# Remove all data frames with 1 or less observations in the 1 month period
all_results_1month_before <- all_results_1month_before[sapply(all_results_1month_before, function(df) nrow(df) > 1)]

# Mirror this removal in the 6 month data frame
selected_names <- names(all_results_1month_before)
all_results_6months_before <- all_results_6months_before[(names(all_results_6months_before) %in% selected_names)]

# Create an empty list to store the merged data frames
combined_data_frames <- list()

# Iterate through the indices of data frames in the lists
for (i in 1:length(all_results_1month_before)) {
  df1 <- all_results_1month_before[[i]]
  df2 <- all_results_6months_before[[i]]
  
  # Merge data frames based on Athlete, Injury_Date, and Test_Date
  merged_df <- merge(df1, df2, by = c("Athlete", "Injury_Date", "Test_Date"))
  
  # Rename the column from "Peak_Power_1month_before" to "Peak_Power_Test_Date"
  colnames(merged_df)[colnames(merged_df) == "Peak_Power_1month_before"] <- "Peak_Power_Test_Date"
  
  # Append the merged data frame to the combined_data_frames list
  combined_data_frames[[i]] <- merged_df
}

# Create a % away from the 6 month average for each Injury (Using Z-Score)
fully_combined_data <- bind_rows(combined_data_frames)

# Create a function to calculate Z-scores
fully_combined_data <- fully_combined_data %>% 
  mutate(z_score = (as.numeric(fully_combined_data$Peak_Power_Test_Date) - as.numeric(fully_combined_data$Peak_Power_6months_before)) / sd(as.numeric(fully_combined_data$Peak_Power_6months_before)))

# Drop observations where the z-score is 0 (Not enough testing)
fully_combined_data <- fully_combined_data %>% 
  filter(z_score != 0)

# Time difference between Injury and Test
fully_combined_data$time_between_injury_test <- floor(as.numeric(difftime(fully_combined_data$Injury_Date, fully_combined_data$Test_Date)))

# Group a mean z score by groups of 3 days
fully_combined_data <- fully_combined_data %>% 
  mutate(grouped_time = (time_between_injury_test - 1) %/% 3) %>%
  group_by(grouped_time) %>% 
  mutate(avg_z_score = mean(z_score)) %>% 
  ungroup()

# Chart
ggplot(fully_combined_data, aes(x = time_between_injury_test, y = avg_z_score)) + 
  geom_line(y=0, color = "blue", linewidth = .75) +
  geom_line(linewidth = 1) +
  labs(
    x = "Days before Injury",
    y = "Z-Score",
    title = "How Peak Power a month before injury looks"
  ) +
  scale_x_reverse() +
  theme_classic()




# ----

fp_only_rsi <- data.frame(Athlete = fp$Athlete, Injury_Date = fp$`Test Date`, RSI = fp$`RSI-modified [m/s]`)

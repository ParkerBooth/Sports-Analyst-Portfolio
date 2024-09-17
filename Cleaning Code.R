library(tidyr)
library(stringr)
library(dplyr)


# Creating Master Roster
file_path <- "Data Path"
files <- list.files(path = file_path, pattern = "^FD tests .*\\.csv$", full.names = TRUE)


data_list <- list()

  data <- read.csv(file, check.names = FALSE)
  
  columns <- colnames(data)
  data_list[[file]] <- data


# Determine the complete set of column names across all files
all_columns <- unique(unlist(lapply(data_list, colnames)))

# Function to add missing columns with N/A
align_columns <- function(df, all_columns) {
  missing_cols <- setdiff(all_columns, colnames(df))
  if (length(missing_cols) > 0) {
    df[missing_cols] <- NA
  }
  df <- df[all_columns]
  return(df)
}

# Apply the function to fill all missing columns in all data frames with N/A
aligned_data_list <- lapply(data_list, align_columns, all_columns = all_columns)

# Combine all data frames into one master file
masterFile <- do.call(rbind, aligned_data_list)

row.names(masterFile) <- NULL


masterFile <- masterFile %>% 
  select(profileId, Athlete, Sport, Archived, everything())
  

write.csv(masterFile, "Final Path", row.names = FALSE)




# More Cleaning

masterFile <- masterFile %>% 
  mutate(Sport = str_replace(Sport, "MBB", "Men's Basketball")) %>% 
  mutate(Sport = str_replace(Sport, "WBB", "Women's Basketball")) %>% 
  mutate(Sport = str_replace(Sport, "^Alpine$", "Alpine Skiing")) %>% 
  mutate(Sport = str_replace(Sport, "^Nordic$", "Nordic Skiing")) %>% 
  mutate(Sport = str_replace(Sport, "^Volleyball \\(Beach\\)$", "Beach Volleyball"))

# Fix Swimming
masterFile <- masterFile %>%
  mutate(Sport2 = ifelse(Sport == "Swim" | Sport == "Diving", "Swimming and Diving", Sport)) %>% 
  select(-Sport) %>% 
  rename(Sport = Sport2) %>% 
  select(-Sport_or_Not, -unique_teams)


# Creating Sport Specific Master Data Frames

directory_path <- "Path"

# Split the data frame by the Sport column
split_dfs <- split(masterFile, masterFile$Sport)

# List of sports to exclude
excluded_sports <- c("ForceDecks Local Database - 2020.01.24 16.04.30", "Active on Current Roster", "3rd Year", "1st Year", "2nd Year", "4th Year", "X Archive")

split_dfs <- split_dfs[!names(split_dfs) %in% excluded_sports]



# Save each split data frame as a CSV file
for(sport in names(split_dfs)) {
  # Create a file name with the sport name
  file_name <- paste0(gsub(" ", " ", sport), ".csv")
  # Construct the full path
  file_path <- file.path(directory_path, file_name)
  # Write the data frame to the CSV file
  write.csv(split_dfs[[sport]], file_path, row.names = FALSE)
}





#  -----------------------------------------------------------------------------------
# Profile List Code


profile_df <- read.csv("Path")

preferred_list <- c(
  "X-Football Archived", "X-Gymnastics Archived", "Softball", "X-Nordic Archived",
  "X-Beach Volleyball Archived", "Gymnatics", "Swimming and Diving", "X-Swim Archived",
  "X-Baseball Archived", "X-Lacrosse Archived", "Football", "Women's Soccer", "X-Women's Soccer Archived",
  "X Archive", "Women's Basketball", "Cross Country", "Volleyball", "X-Track and Field Archived", 
  "Women's Tennis", "X-Women's Tennis Archived", "X-WBB Archived", "Men's Tennis",
  "Track and Field", "Baseball", "X-Cross Country Archived", "X- Women's Soccer Archived",
  "X-Diving Archived",  "Men's Basketball", "Volleyball (Beach)", "X-MBB Archived", "Lacrosse", 
  "X-Alpine Archived", "Golf", "Nordic Skiing","Alpine Skiing", "Gymnastics", 
  "X-Volleyball Archived", "Diving", "X-Softball Archived", "X-Golf Archived")

secondary_list <- c(
  "Holding Midfielder", "Staff", "Winger", "Goal Keeper", "Outside Back", "4th Year", 
  "Center Forward", "Active on Current Roster", "ForceDecks Local Database - 2020.01.24 16.04.30",
  "Center Back", "3rd Year", "1st Year", "2nd Year", "Attacking Midfielder")

id_name_with_1_team <- profile_df %>%
  group_by(profileId, fullName) %>%
  dplyr::summarise(unique_teams = n_distinct(groupName))

id_name_with_1_team <- profile_df %>%
  left_join(id_name_with_1_team, by = c("profileId", "fullName"))



combined_everything <- id_name_with_1_team %>%
  group_by(fullName) %>% 
  mutate(unique_groups = list(sort(unique(groupName), decreasing = TRUE))) %>%
  ungroup() %>%
  mutate(Preferred_Team = ifelse(sapply(unique_groups, length) == 1, 
                                 sapply(unique_groups, function(x) x[1]), 
                                 ifelse(sapply(unique_groups, function(x) any(x %in% preferred_list)), 
                                        sapply(unique_groups, function(x) x[x %in% preferred_list][1]), 
                                        sapply(unique_groups, function(x) x[1]))))


# Filter to just one observation
combined_everything <- combined_everything %>% 
  select(-groupName, -unique_groups) %>% 
  distinct(profileId, .keep_all = TRUE) %>% 
  filter(fullName != "Pending..")


# Fix Soccer Positions
soccer_positions <- c("Center Back", "Goal Keeper", "Holding Midfielder", "Outside Back", "Winger")

combined_everything <- combined_everything %>%
  mutate(Preferred_Team = lapply(Preferred_Team, function(x) {
    if (any(x %in% soccer_positions)) {
      return("Women's Soccer")} 
    else {
      return(x)}}))

# Label for Non-Sport Categories
combined_everything <- combined_everything %>%
  mutate(Sport_or_Not = lapply(Preferred_Team, function(x){
    if (any(x %in% preferred_list)){
      return("Sport")}
    else {
      return("No")}})) %>% 
  mutate(Sport_or_Not = as.character(Sport_or_Not)) %>% 
  mutate(Preferred_Team = as.character(Preferred_Team))


# Fix Archives & Some Sport Names
combined_everything <- combined_everything %>%
  mutate(Archived = case_when(
    str_detect(Preferred_Team, "Archived") ~ "Yes",
    TRUE ~ "No")) %>%
  mutate(Sport = Preferred_Team) %>% 
  mutate(Sport = str_replace(Sport, "X-", "")) %>% 
  mutate(Sport = str_replace(Sport, "Archived", "")) %>% 
  mutate(Sport = str_trim(Sport)) %>% 
  mutate(Sport = str_replace(Sport, "Gymnatics", "Gymnastics"))

# Fix Swimming
combined_everything <- combined_everything %>%
  mutate(Sport2 = ifelse(Sport == "Swim" | Sport == "Diving", "Swimming and Diving", Sport)) %>% 
  select(-Sport) %>% 
  rename(Sport = Sport2)


# Organizing
combined_everything <- combined_everything %>% 
  select(-Preferred_Team) %>% 
  select(profileId, fullName, Sport, Sport_or_Not, Archived, unique_teams) %>% 
  rename(Athlete = fullName)



# Final Merge
finalMaster <- merge(combined_everything, masterFile, by = "profileId", all.y = TRUE)


setwd("Path")
write.csv(finalMaster, file = "data")

# Check for Missing Athletes
isna <- finalMaster %>% 
  filter(is.na(Athlete))



write.csv(isna, "Path", row.names = FALSE)



# Fixing Date Format
historical_data <- read.csv("Path")

historical_data$date <- format(historical_data$date, format = "%m/%d/%Y")

write.csv(historical_data, "Path", row.names = FALSE)



  

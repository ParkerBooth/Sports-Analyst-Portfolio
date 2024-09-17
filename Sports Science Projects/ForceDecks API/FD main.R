library(tidyverse)
library(jsonlite)
library(stringr)


Sys.setenv(CLIENT_ID = "BLANK")
Sys.setenv(CLIENT_SECRET = "BLANK")

source("PATH\\utils\\token_utils.R")
source("PATH\\utils\\cache_utils.R")
source("PATH\\utils\\fd_utils.R")

# Example script that utilizes modules to pull data
# Utilizes the modifiedFrom parameter for caching


# Retrieve Profiles

profile_function <- function() {
  
  # Retrieving credentials from .env file
  
  token <-
    get_token(clientId = Sys.getenv('CLIENT_ID'),
              clientSecret = Sys.getenv('CLIENT_SECRET'))
  
  # Checks if a previous pullDate.rds exists
  # If not, takes date-time right before attempting data pull
  # .rds file prefixed for different products
  
  prefix <- "Profiles"
  profile_modifiedFrom <- '2018-08-13T00:00:00.000Z'
  
  # Request response codes will be printed:
  # 401 indicates a connection error, indicating data may exist but retrieval failed
  # 404 indicates a HTTP error, indicating that no data exists for the dateModified input
  
  data <- get_profile_data(token,
                           profile_modifiedFrom) # FALSE or list vector of testType strings
  
  
  #  -----------------------------------------------------------------------------------
  # Profile List Code
  
  
  profile_df <- data
  
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
    mutate(Sport = str_replace(Sport, "Gymnatics", "Gymnastics")) %>% 
    mutate(Sport = str_replace(Sport, "MBB", "Men's Basketball")) %>% 
    mutate(Sport = str_replace(Sport, "WBB", "Women's Basketball")) %>% 
    mutate(Sport = str_replace(Sport, "^Alpine$", "Alpine Skiing")) %>% 
    mutate(Sport = str_replace(Sport, "^Nordic$", "Nordic Skiing")) %>% 
    mutate(Sport = str_replace(Sport, "^Volleyball \\(Beach\\)$", "Beach Volleyball"))
  
  
  # Fix Swimming
  combined_everything <- combined_everything %>%
    mutate(Sport2 = ifelse(Sport == "Swim" | Sport == "Diving", "Swimming and Diving", Sport)) %>% 
    select(-Sport) %>% 
    rename(Sport = Sport2)
  
  
  # Organizing
  combined_everything <- combined_everything %>% 
    select(-Preferred_Team) %>% 
    select(profileId, fullName, Sport, Sport_or_Not, Archived, unique_teams) %>% 
    rename(Athlete = fullName) %>% 
    select(-Sport_or_Not)
  
  
  # If the connection fails an error will be thrown,
  # preventing writeDate and data being written to .csv
  
  path <- paste("PATH", sep = '')
  
  if (!file.exists(path)) {
    dir.create(path)
  }
  
  write.csv(combined_everything,
            paste(path,
                  prefix,
                  '.csv',
                  sep = ''
            ),
            row.names = FALSE)
}



test_function <- function() {
  # Retrieving credentials from .env file
  
  token <-
    get_token(clientId = Sys.getenv('CLIENT_ID'),
              clientSecret = Sys.getenv('CLIENT_SECRET'))
  
  # Checks if a previous pullDate.rds exists
  # If not, takes date-time right before attempting data pull
  # .rds file prefixed for different products
  
  prefix <- "FD"
  times <- checkDate(prefix)
  modifiedFrom <- times$modifiedFrom
  now <- times$now
  
  # Request response codes will be printed:
  # 401 indicates a connection error, indicating data may exist but retrieval failed
  # 404 indicates a HTTP error, indicating that no data exists for the dateModified input
  
  data <- get_fd_data(
    token,
    dateTo = now,
    modifiedFrom,
    testTypes = c('ALL'),
    # 'ALL' for all test types, otherwise list from 'Test types and metrics/ForceDecks Test Types.csv'
    metrics = c(
      'ALL'
    ) # 'ALL' for all metrics, otherwise list from 'Test types and metrics/ForceDecks Test Metrics.csv'
  )
  
  # If the connection fails an error will be thrown,
  # preventing writeDate and data being written to .csv
  
  
  profile_function()


  # Final Code
  allProfiles <- read.csv("PATH")
  columnNames <- read.csv("PATH", check.names = FALSE)
  
  # Final Merge (Working Data with all Profiles)
  workingNewFDData <- merge(allProfiles, data, by = "profileId", all.y = TRUE)
  
  
  datevar_modifiedFrom <- as.POSIXct(modifiedFrom, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
  workingNewFDData$date <- as.POSIXct(workingNewFDData$date, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
  
  # Filter out all Old Data
  workingNewFDData <- workingNewFDData %>% 
    filter(date >= datevar_modifiedFrom)

  # Format the Date to "mm/dd/yyyy"
  workingNewFDData$date <- format(workingNewFDData$date, format = "%m/%d/%Y")

  # Get the column names of masterFile
  allColumnNames <- colnames(columnNames)

  # Get the column names of workingNewFDData
  workingColumns <- colnames(workingNewFDData)

  # Identify missing columns
  missingColumns <- setdiff(allColumnNames, workingColumns)

  # Add missing columns to workingNewFDData and fill them with NA
  for (col in missingColumns) {
    workingNewFDData[[col]] <- NA
  }

  # Ensure the order of columns matches masterFile
  workingNewFDData <- workingNewFDData[, allColumnNames]

  # Fixing data format issues
  workingNewFDData <- workingNewFDData %>%
    mutate(across(contains("[ms]"), ~ .x * 1000))
  
  workingNewFDData <- workingNewFDData %>%
    mutate(across(contains("RSI"), ~ .x / 100))
  
  
  if (nrow(workingNewFDData) > 0) {
  # Add new data to the bottom of Master Dataframe
  write.table(workingNewFDData, file = "PATH", append = TRUE, row.names = FALSE, col.names = FALSE, sep = ",")
  }


# Use this for saving a new file in the event that historical data needs to be pulled  
  # path <- paste("M:\\AHPS\\API Force Plate\\new_master_data\\", sep = '')
  # 
  # if (!file.exists(path)) {
  #   dir.create(path)
  # }
  # 
  # write.csv(workingNewFDData,
  #           paste(
  #             path,
  #             prefix,
  #             ' tests ',
  #             strsplit(modifiedFrom, 'T')[[1]][[1]],
  #             '.csv',
  #             sep = ''
  #           ),
  #           row.names = FALSE)

  
  # Directory where sport files are stored
  sports_dir <- "Path"
  
  # List all CSV files in the directory (assuming they are named accordingly)
  sport_files <- list.files(sports_dir, pattern = "*.csv", full.names = TRUE)
  
  # Loop through each file
  for (file in sport_files) {
    
    # Extract sport name from the file name (assuming file name contains sport name)
    # Example: If the file is named "Basketball.csv", sport_name would be "Basketball"
    sport_name <- tools::file_path_sans_ext(basename(file))
    
    # Filter workingNewFDData for the current sport
    filtered_new_data <- workingNewFDData %>% 
      filter(Sport == sport_name)
    
    # Only proceed if there is new data for this sport
    if (nrow(filtered_new_data) > 0) {
      
      # # Read the existing data for the current sport
      # existing_data <- read.csv(file, stringsAsFactors = FALSE)
      # 
      # # Append new data to the existing data
      # updated_data <- bind_rows(existing_data, filtered_new_data)
      
      # Write the updated data back to the same file
      write.table(filtered_new_data, file = file, append = TRUE, row.names = FALSE, col.names = FALSE, sep = ",")
    }
  }
  
  # update Date
  writeDate(prefix, now)
  
  
  print("It all works")
  
}



if (sys.nframe() == 0) {
  test_function()
}




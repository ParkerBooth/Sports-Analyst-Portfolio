library(jsonlite)
library(httr)
library(tidyverse)
library(data.table)
library(lubridate)

# Retrieves list of orgIds

get_orgs <- function(token) {
  url <- "https://prd-use-api-extforcedecks.valdperformance.com/v2019q3/teams"
  
  req <- GET(url, add_headers("Authorization" = token))
  
  if (req$status_code == 200) {
    print(paste("Success:", req$status_code, 'for', url))
    
    json <- content(req, as = "text")
    
    return(as.data.frame(fromJSON(json)) %>%
             mutate(orgId = as.character(id)) %>%
             pull(id))
    
  } else if (req$status_code != 200) {
    stop(paste("Error:", req$status_code, 'for', url))
    
  }
  
}

# Retrieves profile data for specified orgId string

get_profiles <- function(orgId, token, modifiedFrom, maxPage = 20) {
  page <- 1
  profiles <- data.frame()
  
  while (page < maxPage) {
    url <-
      paste(
        "https://prd-use-api-extforcedecks.valdperformance.com/v2019q3/teams/",
        orgId,
        "/athletes?modifiedFrom=",
        modifiedFrom,
        "&page=",
        page,
        sep = ""
      )
    
    req <- GET(url, add_headers("Authorization" = token))
    
    if (req$status_code == 200) {
      print(paste("Success:", req$status_code, 'for', url))
      
      json <- content(req, as = "text")
      
      # Parse the JSON data
      data <- fromJSON(json, flatten = TRUE)
      
      # Convert the list to a data frame
      df <- as.data.frame(data)
      
      # Handle cases where the 'attributes' column exists
      if ("attributes" %in% names(df)) {
        # Unnest the attributes column
        df_unnested <- df %>%
          unnest(attributes, keep_empty = TRUE) # Use keep_empty to ensure all rows are kept
        
        # Handle cases where 'valueName' is missing
        profile <- df_unnested %>%
          select(profileId = id, fullName = name, groupName = valueName) %>%
          mutate(groupName = if_else(is.na(groupName), NA_character_, groupName))
      } else {
        # If 'attributes' column does not exist, create profile with NA for groupName
        profile <- df %>%
          select(profileId = id, fullName = name) %>%
          mutate(groupName = NA)
      }
      
      
      page <- page + 1
      
      profiles <- bind_rows(profiles, profile)
      
      if (nrow(profile) < 50) {
        break
      }
      
    } else if (req$status_code == 404) {
      print(paste("Exception:", req$status_code, 'for', url))
      
      names <- c("profileId", "fullName", "groupName")
      
      return(setNames(data.frame(matrix(
        ncol = length(names), nrow = 0
      )),
      names))
      
    } else {
      stop(paste("Error:", req$status_code, 'for', url))
      
    }
    
  }
  
  return(profiles)
}


# Main function

get_profile_data <- function(token, modifiedFrom){
  
  ### Get data
  
  org_ids <- get_orgs(token)
  
  profile_data <-
    lapply(org_ids, get_profiles, token, modifiedFrom) %>% 
    rbindlist(fill = T)
  
  # Assert that either of data frames are not empty
  if ((max(dim(profile_data)) == 0)) {
    stop(
      sprintf(
        "Connection failed for under the following inputs: modifiedFrom: %s, org Ids: %s",
        modifiedFrom,
        paste(org_ids, collapse = ', ')
      )
    )
  }
  
  return(profile_data)
  
}


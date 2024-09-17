library(tidyverse)
library(jsonlite)
library(httr)
library(data.table)
library(lubridate)
source("Path")

# Retrieves test data for each orgId in list
get_tests <- function(orgId, token, modifiedFrom) {
  data <- data.frame()
  page <- 1
  
  repeat {
    url <- paste(
      "https://prd-use-api-extforcedecks.valdperformance.com/v2019q3/teams/",
      orgId,
      "/tests/",
      page,
      "?modifiedFrom=",
      modifiedFrom,
      sep = ""
    )
    
    req <- GET(url, add_headers("Authorization" = token))
    
    if (req$status_code == 200) {
      print(paste("Success:", req$status_code, 'for', url))
      
      json <- content(req, as = "text")
      new_data <- as.data.frame(fromJSON(json)) %>%
        select(
          orgId = items.teamId,
          profileId = items.athleteId,
          testId = items.id,
          date = items.recordedUTC,
          testType = items.testType
        )
      
      data <- bind_rows(data, new_data)
      page <- page + 1
      
      # Break if fewer than 50 items are returned
      if (nrow(new_data) < 50) {
        break
      }
      
    } else if (req$status_code == 204) {
      print(paste("No test since ", modifiedFrom))
      break
      
    } else if (req$status_code == 404) {
      print(paste("Exception:", req$status_code, 'for', url))
      
      names <- c("orgId", "profileId", "testId", "date", "testType", "lastModifiedUTC")
      return(setNames(data.frame(matrix(ncol = length(names), nrow = 0)), names))
      
    } else {
      stop(paste("Error:", req$status_code, 'for', url))
    }
  }
  
  return(data)
}
  


# Retrieves trial data for specified orgId and testId

get_trials <- function(id, token, metrics) {
  org_id <- id[["orgId"]]
  
  test_id <- id[["testId"]]
  
  url <-
    paste(
      "https://prd-use-api-extforcedecks.valdperformance.com/v2019q3/teams/",
      org_id,
      "/tests/",
      test_id,
      "/trials",
      sep = ""
    )
  
  req <- GET(url, add_headers("Authorization" = token))
  
  if (req$status_code == 200) {
    print(paste("Success:", req$status_code, 'for', url))
    
    json <- content(req, as = "text")
    
    trials <-
      as.data.frame(fromJSON(json)) %>%
      mutate(orgId = org_id,
             testId = test_id,
             trial = row_number()) %>%
      select(orgId,
             profileId = athleteId,
             testId,
             trial,
             trialLimb = limb,
             results) %>%
      unnest(results)
    
    if (nrow(trials) == 0) {
      return()
    }
    
    if (!'definition' %in% colnames(trials)) {
      print(test_id)
    }
    
    trials <- trials %>% 
      unnest(definition) %>%
      select(
        orgId,
        profileId,
        testId,
        trial,
        trialLimb,
        metric = name,
        unit,
        metricLimb = limb,
        value
      )
    
    if (!"ALL" %in% toupper(metrics)) {
      trials <- trials %>%
        filter(metric %in% metrics)
    }
    
    return(trials)
    
  } else if (req$status_code == 404) {
    print(paste("Exception:", req$status_code, 'for', url))
    
    names <- c("orgId",
               "profileId",
               "testId",
               "trial",
               "metricLimb",
               "trialLimb",
               "date",
               "metricId",
               "metric",
               "value",
               "unit")
    
    return(setNames(data.frame(matrix(
      ncol = length(names), nrow = 0
    )),
    names))
    
  } else {
    stop(paste("Error:", req$status_code, 'for', url))
  }
  
}

# Aggregates trial data into max, min, mean, sd

aggregate_trials <- function(data) {
  return(
    data %>% group_by_at(vars(-trial,-value)) %>%
      summarise(
        Max = max(value, na.rm = T),
        Mean = mean(value, na.rm = T),
        Min = min (value, na.rm = T),
        SD = sd(value, na.rm = T)
      ) %>%
      mutate(SD = case_when(is.na(SD) ~ 0, T ~ SD)) %>%
      pivot_longer(
        cols = c(Max, Mean, Min, SD),
        names_to = "summaryStatistic",
        values_to = "value"
      )
  )
}


# str(workingdata$unit)
# 
# workingdata$unit <- as.character(workingdata$unit)
# 
# workingdata <- workingdata %>%
#   mutate(unit = ifelse(unit == "Watt Per Kilo", "[W / kg]", unit)) %>%
#   mutate(unit = ifelse(unit == "Second", "[s]", unit))


# Fixing the look of Units
pivot_wider <- function(data) {
  return(
    data %>%
      ungroup() %>%
      mutate(unit = ifelse(unit == "Watt Per Kilo", " [W/kg]", unit)) %>% 
      mutate(unit = ifelse(unit == "Second", " [s]", unit)) %>% 
      mutate(unit = ifelse(unit == "Newton", " [N]", unit)) %>% 
      mutate(unit = ifelse(unit == "Kilo", " [kg]", unit)) %>% 
      mutate(unit = ifelse(unit == "Inch", " [in]", unit)) %>% 
      mutate(unit = ifelse(unit == "Percent", " [%]", unit)) %>% 
      mutate(unit = ifelse(unit == "Centimeter", " [cm]", unit)) %>% 
      mutate(unit = ifelse(unit == "Joule", " [J]", unit)) %>% 
      mutate(unit = ifelse(unit == "Meter Per Second", " [m/s]", unit)) %>% 
      mutate(unit = ifelse(unit == "Meter Per Second Per Second", " [m/s^2]", unit)) %>% 
      mutate(unit = ifelse(unit == "Millisecond", " [ms]", unit)) %>% 
      mutate(unit = ifelse(unit == "Newton Per Centimeter", " [N/cm]", unit)) %>% 
      mutate(unit = ifelse(unit == "Newton Per Meter", " [N/m]", unit)) %>% 
      mutate(unit = ifelse(unit == "Newton Per Second", " [N/s]", unit)) %>% 
      mutate(unit = ifelse(unit == "Newton Per Kilo", " [N/kg]", unit)) %>% 
      mutate(unit = ifelse(unit == "Newton Per Second Per Kilo", " [N/s/kg]", unit)) %>% 
      mutate(unit = ifelse(unit == "Newton Per Second Per Centimeter", " [N/s/cm]", unit)) %>% 
      mutate(unit = ifelse(unit == "Newton Second", " [N s]", unit)) %>% 
      mutate(unit = ifelse(unit == "No Unit", "", unit)) %>% 
      mutate(unit = ifelse(unit == "RSIModified", " [m/s]", unit)) %>% 
      mutate(unit = ifelse(unit == "Newton Second Per Kilo", " [N s/kg]", unit)) %>% 
      mutate(unit = ifelse(unit == "Watt", " [W]", unit)) %>% 
      mutate(unit = ifelse(unit == "Watt Per Second", " [W/s]", unit)) %>% 
      mutate(unit = ifelse(unit == "Watt Per Second Per Kilo", " [W/s/kg]", unit)) %>% 
      mutate(unit = ifelse(unit == "Pound", " [lb]", unit)) %>% 
      mutate(metricLimb = ifelse(metricLimb == "Trial", "", metricLimb)) %>% 
      mutate(metricLimb = ifelse(metricLimb == "Left", " (Left)", metricLimb)) %>% 
      mutate(metricLimb = ifelse(metricLimb == "Right", " (Right)", metricLimb)) %>% 
      mutate(metricLimb = ifelse(metricLimb == "Asym", " Asymmetry [% L,R]", metricLimb)) %>% 
      mutate(unit = ifelse(metricLimb == " Asymmetry [% L,R]", "", unit)) %>% 
      tidyr::pivot_wider(
        names_from = c(metric, metricLimb, unit),
        values_from = value,
        values_fn = max,
        names_sep = ""
      )
  )

}

# Retrieves raw trace data for specified orgId and testId

get_raw <- function(ids) {
  org_id <- ids[["orgId"]]
  
  test_id <- ids[["testId"]]
  
  url <- paste(
    "https://prd-use-api-extforcedecks.valdperformance.com/v2019q3/teams/",
    org_id,
    "/tests/",
    test_id,
    "/recording/file",
    sep = ""
  )
  
  req <- GET(url, add_headers("Authorization" = token))
  
  if ((req$status_code == 200) | (req$status_code == 204)) {
    print(paste("Success:", req$status_code, 'for', url))
    
    bin <- content(req, "raw")
    
    writeBin(bin, "temp.csv")
    
    raw_data <-
      read.csv("temp.csv",
               header = TRUE,
               dec = ",",
               skip = 1)
    
    file_name <- paste(
      folder_name,
      "/",
      'Test Id: ',
      test_id,
      ".csv",
      sep = ""
    )
    
    write.csv(raw_data_full, file_name, row.names = F)
    
  } else if (req$status_code == 404) {
    stop(paste("Exception:", req$status_code, 'for', url))
    
  } else {
    stop(paste("Error:", req$status_code, 'for', url))
    
  }
  
}

# Main function

get_fd_raw <- function(token, ids) {
  filename <- paste(getwd(),
                    '/ForceDecks Raw Traces/',
                    sep = '')
  
  if (!file.exists(filename)) {
    dir.create(filename)
  }
  
  ids <- split(ids, seq(nrow(ids)))
  
  lapply(ids, get_raw, token, filename)
  
}

# Main function

get_fd_data <-
  function(token,
           dateTo,
           modifiedFrom,
           testTypes = 'ALL',
           metrics = 'ALL',
           aggregateTrials = FALSE,
           pivotWider = TRUE) {
    ### Get data
    
    org_ids <- get_orgs(token)
    
    test_data <- lapply(org_ids, get_tests, token, modifiedFrom) %>%
      rbindlist(fill = T)
    
    # Assert that either of data frames are not empty
    if (max(dim(test_data)) == 0) {
      stop(
        sprintf(
          "Connection failed for both profile and test data under the following inputs: modifiedFrom: %s, org Ids: %s",
          modifiedFrom,
          paste(org_ids, collapse = ', ')
        )
      )
    }
    
    if (!"ALL" %in% toupper(testTypes)) {
      test_data <- test_data %>%
        filter(testType %in% testTypes)
    }
    
    ids <- test_data %>%
      select(orgId, testId) %>%
      distinct()
    
    ids <- split(ids, seq(nrow(ids)))
    
    trial_data <- lapply(ids, get_trials, token, metrics) %>%
      rbindlist(fill = T)
    
    if (nrow(trial_data) > 0) {
      if (aggregateTrials == TRUE) {
        trial_data <- aggregate_trials(trial_data)
      }
      if (pivotWider == TRUE) {
        trial_data <- pivot_wider(trial_data)
      }
    }
    
    return(test_data %>%
             merge(trial_data, by = c('orgId', 'profileId', 'testId')))
    
  }


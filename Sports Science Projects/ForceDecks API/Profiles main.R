library(tidyverse)
library(jsonlite)
library(stringr)

source("PATH\\utils\\token_utils.R")
source("PATH\\utils\\cache_utils.R")
source("PATH\\utils\\profile_utils.R")

Sys.setenv(CLIENT_ID = "BLANK")
Sys.setenv(CLIENT_SECRET = "BLANK")

# Example script that utilizes modules to pull data
# Utilizes the modifiedFrom parameter for caching

main <- function() {
  
  # Retrieving credentials from .env file
  
  token <-
    get_token(clientId = Sys.getenv('CLIENT_ID'),
              clientSecret = Sys.getenv('CLIENT_SECRET'))
  
  # Checks if a previous pullDate.rds exists
  # If not, takes date-time right before attempting data pull
  # .rds file prefixed for different products
  
  prefix <- "Profiles"
  times <- checkDate(prefix)
  modifiedFrom <- times$modifiedFrom
  now <- times$now
  
  # Request response codes will be printed:
  # 401 indicates a connection error, indicating data may exist but retrieval failed
  # 404 indicates a HTTP error, indicating that no data exists for the dateModified input
  
  data <- get_profile_data(token,
                      modifiedFrom) # FALSE or list vector of testType strings
  
  
  
  
  
  
  
  # If the connection fails an error will be thrown,
  # preventing writeDate and data being written to .csv
  
  writeDate(prefix, now)
  
  path <- paste("Final Data Path", sep = '')
  
  if (!file.exists(path)) {
    dir.create(path)
  }
  
  write.csv(data,
            paste(path,
              'data/',
              prefix,
              ' tests ',
              strsplit(modifiedFrom, 'T')[[1]][[1]],
              '.csv',
              sep = ''
            ),
            row.names = FALSE)
}

if(sys.nframe() == 0){main()}

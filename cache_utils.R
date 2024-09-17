library(lubridate)

checkDate <- function(prefix) {
  # Checks if pull date file exists
  # Else pulls from all time
  # Returns current date-time

  now <- Sys.time()
  attr(now, "tzone") <- "America/Denver"  # Handles both MST and MDT
  
  # Convert to UTC
  now_utc <- with_tz(now, tzone = "UTC")
  
  # Format to the desired string format
  now <- format(now_utc, "%Y-%m-%dT%H:%M:%S.000Z")
  
  
  filename <- "Path"
  if (file.exists(filename)) {
    return(list('modifiedFrom' = readRDS(filename),
                'now' = now))
  } else {
    return(list('modifiedFrom' = '2024-08-17T00:00:00.000Z', 
                'now' = now))
  }
  
  return(list('modifiedFrom' = past, 'now' = now))
}

writeDate <- function(prefix, now) {
    # Writing pull date
    path <- paste("Path", sep = '')
    if (!file.exists(path)) {
        dir.create(path)
    }
    filename <- paste(path, prefix, ' ', 'data pullDate.rds', sep = '')
    write_rds(now, filename)
}


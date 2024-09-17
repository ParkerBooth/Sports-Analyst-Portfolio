library(jsonlite)
library(httr)

get_token <- function(clientId, clientSecret){
  
  secret <- base64_enc(paste(clientId, clientSecret, sep = ":"))
  
  req <- POST("https://security.valdperformance.com/connect/token",
              add_headers(
                "Authorization" = paste("Basic", gsub("\n", "", secret)),
                "Content-Type" = "application/x-www-form-urlencoded;charset=UTF-8"),
              body = "grant_type=client_credentials")
  
  if(http_error(req)){
    stop(paste0("Token request failed: ", req$status_code))
  }
  
  token <- paste("Bearer", content(req)$access_token)
  
  return(token)
  
}

token <-
  get_token(clientId = Sys.getenv('CLIENT_ID'),
            clientSecret = Sys.getenv('CLIENT_SECRET'))

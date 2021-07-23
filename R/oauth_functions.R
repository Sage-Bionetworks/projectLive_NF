get_oauth_access_token <- function(url_parameters){
  
  if (!has_auth_code(url_parameters)) {
    return()
  }
  
  redirect_url <- paste0(
    OAUTH_ENDPOINT$access, 
    '?',
    'redirect_uri=',
    APP_URL, 
    '&grant_type=',
    'authorization_code',
    '&code=', 
    url_parameters$code
  )
  
  # get the access_token and userinfo token
  req <- httr::POST(
    redirect_url,
    encode = "form",
    body = '',
    httr::authenticate(OAUTH_APP$key, OAUTH_APP$secret, type = "basic"),
    config = list()
  )
  
  # Stop the code if anything other than 2XX status code is returned
  httr::stop_for_status(req, task = "get an access token")
  token_response <- httr::content(req, type = NULL)
  access_token <- token_response$access_token
}
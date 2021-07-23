get_oauth_access_token <- function(oauth_list, session){
  
  url_parameters <- shiny::parseQueryString(
    shiny::isolate(session$clientData$url_search)
  )
                                            
  if (!has_auth_code(url_parameters)) {
    return()
  }
  
  redirect_url <- paste0(
    oauth_list$endpoint$access, 
    '?',
    'redirect_uri=',
    oauth_list$url, 
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
    httr::authenticate(oauth_list$app$key, oauth_list$app$secret, type = "basic"),
    config = list()
  )
  
  # Stop the code if anything other than 2XX status code is returned
  httr::stop_for_status(req, task = "get an access token")
  token_response <- httr::content(req, type = NULL)
  access_token <- token_response$access_token
}

create_oauth_url_script_html <- function(endpoint, app, scope){
  
  authorization_url = httr::oauth2.0_authorize_url(endpoint, app, scope = scope)

  script <- shiny::tags$script(shiny::HTML(sprintf(
    "location.replace(\"%s\");", authorization_url
  )))
}
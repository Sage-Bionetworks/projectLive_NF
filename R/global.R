# functions ----

get_oauth_config <- function(config_file = "inst/oauth_config.yaml"){
  oauth_config = yaml::yaml.load_file(config_file)
  
  client_id     <- toString(oauth_config$client_id)
  client_secret <- oauth_config$client_secret
  app_url       <- oauth_config$app_url
  
  if (is.null(client_id)) stop("config.yaml is missing client_id")
  if (is.null(client_secret)) stop("config.yaml is missing client_secret")
  if (is.null(app_url)) stop("config.yaml is missing app_url")
  
  return(list(
    "client_id" = client_id,
    "client_secret" = client_secret,
    "app_url" = app_url
  ))
}


has_auth_code <- function(params) {
  # params is a list object containing the parsed URL parameters. Return TRUE if
  # based on these parameters, it looks like auth code is present that we can
  # use to get an access token. If not, it means we need to go through the OAuth
  # flow.
  return(!is.null(params$code))
}


get_synapse_claims_list <- function(){
  # These are the user info details ('claims') requested from Synapse:
  return(
    list(
      family_name=NULL, 
      given_name=NULL,
      email=NULL,
      email_verified=NULL,
      userid=NULL,
      orcid=NULL,
      is_certified=NULL,
      is_validated=NULL,
      validated_given_name=NULL,
      validated_family_name=NULL,
      validated_location=NULL,
      validated_email=NULL,
      validated_company=NULL,
      validated_at=NULL,
      validated_orcid=NULL,
      company=NULL
    )
  )
}

get_synapse_claims_json <- function(){
  rjson::toJSON(list(
    id_token = get_synapse_claims_list(), 
    userinfo = get_synapse_claims_list()
  ))
}

create_oauth_app <- function(oauth_config){
  httr::oauth_app(
    "shinysynapse",
    key = oauth_config$client_id,
    secret = oauth_config$client_secret, 
    redirect_uri = oauth_config$app_url
  )
}

create_oauth_endpoint <- function(){
  httr::oauth_endpoint(
    authorize = stringr::str_c(
      "https://signin.synapse.org?claims=", 
      get_synapse_claims_json()
    ),
    access = "https://repo-prod.prod.sagebase.org/auth/v1/oauth2/token"
  )
}

get_scopes <- function(){
  # The 'openid' scope is required by the protocol for retrieving user information.
  return("openid view download modify")
}

#----

if (interactive()) {
  options(shiny.port = 8100)
} 

OAUTH_CONFIG = get_oauth_config()

APP_URL = OAUTH_CONFIG$app_url

OAUTH_APP <- create_oauth_app(OAUTH_CONFIG)

OAUTH_ENDPOINT <- create_oauth_endpoint()

SCOPE <- get_scopes()
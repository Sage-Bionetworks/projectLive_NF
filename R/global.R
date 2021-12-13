if (interactive()) {
  options(shiny.port = 8100)
} 

OAUTH_LIST <- projectlive.modules::create_oauth_list()

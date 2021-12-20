
# Activate virtual env
Sys.unsetenv("RETICULATE_PYTHON")
# Note, the name of the virtual environment is defined in the GH Actions workflow
reticulate::use_virtualenv(file.path(getwd(),"virtual_env"))


if (interactive()) {
  options(shiny.port = 8100)
} 

OAUTH_LIST <- projectlive.modules::create_oauth_list("oauth_config.yaml")


# Activate virtual env
Sys.unsetenv("RETICULATE_PYTHON")
# Note, the name of the virtual environment is defined in the GH Actions workflow
venv_name<-"virtual_env"
reticulate::use_virtualenv(file.path(getwd(),venv_name))
# We get a '126' error (non-executable) if we don't do this:
system(sprintf("chmod -R +x %s", venv_name))



if (interactive()) {
  options(shiny.port = 8100)
} 

OAUTH_LIST <- projectlive.modules::create_oauth_list("oauth_config.yaml")

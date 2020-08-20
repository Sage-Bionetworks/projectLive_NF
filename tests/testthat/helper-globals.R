require(magrittr)
require(rlang)
config_file <- system.file("data_config.json", package = "projectLive") 
data_config <- jsonlite::read_json(config_file)
create_team_table_from_synapse <- function(syn, data_config){
  synapse_id <- purrr::pluck(data_config, "team_table", "synapse_id")
  team_col <- purrr::pluck(data_config, "team_table", "team_column")
  group_col <- purrr::pluck(data_config, "team_table", "group_column")
  
  team_table <-
    glue::glue(
      "SELECT '{team_col}' AS t, '{group_col}' AS g FROM {synapse_id}"
    ) %>% 
    syn$tableQuery(includeRowIdAndRowVersion = F) %>% 
    purrr::pluck("filepath") %>% 
    readr::read_csv(.) %>% 
    dplyr::mutate_all(
      ~stringr::str_remove_all(.x, '[\\"\\[\\]\\\\]')
    ) %>% 
    dplyr::mutate_all(~stringr::str_split(.x, ", ")) %>% 
    dplyr::select("teams" = "t", "groups" = "g")
}

get_allowed_groups_from_synapse_user_id <- function(syn, data_config, user_id){
  # The following code chunk ensures that only members of specific teams can access the content of the app. 
  # Individual users will not be able to access the tables/files through this code even if they have access to the entity on synapse
  dashboard_entity <- "syn22281727"
  
  # the teams that user belongs to
  user_teams <- syn$restGET(
    glue::glue("/user/{syn$getUserProfile()[['ownerId']]}/team?limit=10000"))$results 
  all_teams <- purrr::map_chr(user_teams, function(x) x$id)

  # the teams allowed to view the dashboard
  dashboard_teams <- syn$restGET(glue::glue("/entity/{dashboard_entity}/acl"))
  allowed_teams <- purrr::map_chr(dashboard_teams$resourceAccess, function(x) x$principalId)
  
  #final allowed agencies:
  all_teams[all_teams %in% allowed_teams]
  
  team_table <- 
    create_team_table_from_synapse(syn, data_config) %>% 
    filter(teams %in% all_teams[all_teams %in% allowed_teams]) %>% 
    select(groups) %>% 
    unlist() %>% 
    unique() %>% 
    sort()
  
}

get_synapse_tbl <- function(syn, table_id){
  list_columns <- table_id %>% 
    syn$getTableColumns() %>% 
    reticulate::iterate(.) %>% 
    purrr::keep(
      ., 
      stringr::str_detect(
        purrr::map_chr(., purrr::pluck, "columnType"), "_LIST"
      )
    ) %>% 
    purrr::map_chr(purrr::pluck("name"))
  
  
  syn$tableQuery(glue::glue("SELECT * FROM {table_id}")) %>% 
    purrr::pluck("filepath") %>% 
    readr::read_csv(.) %>% 
    dplyr::select(!dplyr::contains("depr")) %>% 
    dplyr::mutate_at(
      list_columns, ~stringr::str_remove_all(.x, '[\\"\\[\\]\\\\]')
    ) %>% 
    dplyr::mutate_at(list_columns, ~stringr::str_split(.x, ", "))
}

store_file_in_synapse <- function(file, parent_id, remove_file = T){
  file <- reticulate::import("synapseclient")$File(file, parent_id)
  syn$store(file)
  if(remove_file) rm(file)
}

read_rds_file_from_synapse <- function(syn, synapse_id){
  synapse_id %>% 
    syn$get(.) %>% 
    purrr::pluck("path") %>% 
    readRDS(.)
}

synapse_dates_to_year <- function(dates){
  dates %>% 
    magrittr::divide_by(., 1000) %>% 
    purrr::map(as.POSIXct, origin = "1970-01-01") %>% 
    purrr::map_dbl(lubridate::year) %>% 
    as.integer()
}

synapse_dates_to_month <- function(dates){
  dates %>% 
    magrittr::divide_by(., 1000) %>% 
    purrr::map(as.POSIXct, origin = "1970-01-01") %>% 
    purrr::map(lubridate::month, label  = TRUE, abbr = TRUE) %>% 
    unlist()
}

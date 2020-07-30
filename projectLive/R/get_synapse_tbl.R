get_synapse_tbl <- function(syn, table_id, funding_agency = NULL){

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
  
  if(is.null(funding_agency)){
    query <- glue::glue("SELECT * FROM {table_id}")
  } else {
    query <- 
      glue::glue(
        "SELECT * FROM {table_id} WHERE fundingAgency = '{funding_agency}'"
      )
  }

  query %>% 
    syn$tableQuery() %>% 
    purrr::pluck("filepath") %>% 
    readr::read_csv(.) %>% 
    dplyr::select(!dplyr::contains("depr")) %>% 
    dplyr::mutate_at(
      list_columns, ~stringr::str_remove_all(.x, '[\\"\\[\\]\\\\]')
    ) %>% 
    dplyr::mutate_at(list_columns, ~stringr::str_split(.x, ", "))
}
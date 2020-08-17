create_data_focus_tables <- function(data, x_column, fill_columns){
  purrr::map(fill_columns, ~ dplyr::select(data, x_column, .x)) %>% 
    purrr::map(tidyr::drop_na) %>% 
    purrr::discard(., purrr::map(., nrow) == 0)
}

concatenate_list_columns <- function(tbl, columns){
  dplyr::mutate_at(
    tbl,
    columns,
    ~purrr::map_chr(.x, stringr::str_c, collapse = " | ")
  ) 
}

concatenate_df_list_columns_with_param_list <- function(tbl, param_list){
  list_columns <- param_list %>% 
    purrr::pluck("columns") %>% 
    dplyr::tibble(
      "type" = safe_pluck_list(., "type"),
      "name" = safe_pluck_list(., "name")
    ) %>% 
    dplyr::select("type", "name") %>% 
    dplyr::filter(.data$type == "list:character") %>% 
    dplyr::pull("name") %>% 
    unname() 
    
  concatenate_list_columns(tbl, list_columns)
}

safe_pluck_list <- function(lst, n){
  lst %>% 
    purrr::map(purrr::pluck, n, .default = NA) %>% 
    unlist()
}

rename_df_columns_with_param_list <- function(tbl, param_list){
  column_select_list <- param_list %>%
    purrr::pluck("columns") %>%
    dplyr::tibble(
      "display_name" = safe_pluck_list(., "display_name"),
      "name" = safe_pluck_list(., "name")
    ) %>% 
    dplyr::select("display_name", "name") %>% 
    dplyr::mutate("display_name" = dplyr::if_else(
      is.na(.data$display_name),
      stringr::str_to_title(.data$name),
      .data$display_name
    )) %>% 
    tibble::deframe(.)
  
  dplyr::select(tbl, column_select_list) 
}

recode_column_values <- function(tbl, column, lst = NULL, ...){
  if(is.null(lst)) lst <- list("0" = "0")
  col_var <- rlang::sym(column)
  tbl <- dplyr::mutate(tbl, !!col_var := dplyr::recode(!!col_var, !!!lst, ...)) 
  dplyr::pull(tbl, column)
  return(tbl)
}

recode_column_values_with_param_list <- function(tbl, param_list){
  recode_column_values(
    tbl,
    param_list$name, 
    param_list$replace_values,
    .default = param_list$default_replace,
    .missing = param_list$na_replace
  )
}

recode_df_with_param_list <- function(tbl, param_list){
  column_param_list <- param_list %>% 
    purrr::pluck("columns") %>%  
    purrr::keep(
      ., 
      purrr::map(., purrr::pluck("type")) %in% c("character", "list:character")
    )
  for (param_list in column_param_list) {
    tbl <- recode_column_values_with_param_list(tbl, param_list)
  }
  return(tbl)
}

get_distinct_value_from_column <- function(tbl, column){
  tbl %>% 
    dplyr::pull(column) %>% 
    dplyr::n_distinct()
}

add_distinct_values_from_columns <- function(tbl, columns){
  result <-  
    purrr::map_int(columns, ~get_distinct_value_from_column(tbl, .x)) %>% 
    sum()
}

#' @importFrom magrittr %>% 
#' @importFrom rlang := .data
create_plot_df_from_count_df <- function(column, count_column, data){
  data %>%
    dplyr::filter(.data$name == column) %>% 
    tidyr::pivot_wider() %>% 
    dplyr::rename(!! rlang::ensym(count_column) := "count")
}

create_plot_dfs_from_count_df <- function(columns, count_columns, data){
  purrr::map2(
    columns,
    count_columns,
    create_plot_df_from_count_df,
    data
  )
}


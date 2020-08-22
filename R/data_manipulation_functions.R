#' Format Plot Data With Parameter List
#'
#' This function runs a tible through the main data cleanup functions befoire 
#' being displayed in a plot or data table. See those functions for more 
#' details.
#' 
#' @param data A Tibble
#' 
#'   data <- dplyr::tribble(
#'    ~consortium, ~year, ~month,
#'    NA,          2000L, NA,
#'    "c1",        20001, "January"
#'  )
#'
#' @param param_list A list with a named list named "columns" that has an entry  
#' for each column needed in the tibble. Each column must have a "name",
#' and "type" field. Optional fields include "replace_values", "display_name",
#' "na_replace", and "deafult_replace".
#' 
#'   param_list <- list(
#'    "columns" = list(
#'       list(
#'         "name" = "consortium",
#'         "display_name" = "Consortium",
#'         "na_replace" = "Not Applicable",
#'         "type" = "character"
#'       ),
#'      )
#'     )
#'
#' @importFrom magrittr %>% 
format_plot_data_with_param_list <- function(data, param_list){
  data %>% 
    concatenate_df_list_columns_with_param_list(param_list) %>%    
    recode_df_with_param_list(param_list) %>% 
    rename_df_columns_with_param_list(param_list)
}

#' Create Data Focus Tables
#' This function creates a list of tables from on input tibble. The list will
#' have one table per column listed in the fill_columns list. This function is 
#' used in the study_summary module to create the data_focus plots.
#'
#' @param data A tibble
#' @param x_column A string that is the name of a column in the data
#' @param fill_columns A list of strings that are names of columns in data
#' @importFrom magrittr %>% 
create_data_focus_tables <- function(data, x_column, fill_columns){
  purrr::map(fill_columns, ~ dplyr::select(data, x_column, .x)) %>% 
    purrr::set_names(fill_columns) %>% 
    purrr::map(tidyr::drop_na) %>% 
    purrr::discard(., purrr::map(., nrow) == 0) 
}

#' Concatenate List Columns
#' This function will concatenate list columns into character columns
#'
#' @param tbl A Tibble
#' @param columns A list of strings that are names of columns in data to 
#' be concatenated
#' @importFrom magrittr %>% 
concatenate_list_columns <- function(tbl, columns){
  dplyr::mutate_at(
    tbl,
    columns,
    ~purrr::map_chr(.x, stringr::str_c, collapse = " | ")
  ) 
}

#' Concatenate Dataframe List Columns With Parameter List
#' This function will concatenate list columns into character columns. 
#' Any column of type "list:character" will be concatenated
#' 
#' @param data A Tibble
#' 
#'   data <- dplyr::tribble(
#'    ~study,       ~month,
#'    c("s1", "s2)  "January"
#'  )
#'
#' @param param_list A list with a named list named "columns" that has an entry  
#' for each column needed in the tibble. Each column must have a "name", and
#' "type" field. 
#' 
#'   param_list <- list(
#'    "columns" = list(
#'       list(
#'         "name" = "study",
#'         "display_name" = "Consortium",
#'         "type" = "list:character"
#'       ),
#'      )
#'     )
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom magrittr %>% 
#' @importFrom rlang .data
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

#' Safe Pluck List
#' This function is used to safely pluck named value from a list of named lists.
#' This will return a vector of values the length of the list. If the named list
#' doesn't have the name, an NA will be returned.
#'
#' @param lst A list of named lists
#' 
#' list1 = list(
#'  list("name" = "col1", "display_name" = "Column1", "type" = "x"),
#'  list("name" = "col2", "type" = "x")
#' )
#' 
#' @param n A string. If the string is a name in the named lists, the value of
#' that string will be returned, otherwise NA.
#' @importFrom magrittr %>% 
safe_pluck_list <- function(lst, n){
  lst %>% 
    purrr::map(purrr::pluck, n, .default = NA) %>% 
    unlist()
}

#' Rename Dataframe Columns With Parameter List
#' 
#' This function will select and rename columns based on the parameter list. 
#' Only columns in the parameter list will be selected. Columns with a display
#' name, will be renamed that, otherwise stringr::str_to_title will be used on 
#' the anme.
#'
#' @param tbl A tibble
#' 
#'  data <- dplyr::tribble(
#'    ~study, ~month,    ~year,
#'    "s1",   "January", 2001L
#'  )
#' @param param_list A list with a named list named "columns" that has an entry  
#' for each column needed in the tibble. Each column must have a "name",
#' and "type" field. "display_name" will be used to rename the column.
#' 
#'   param_list <- list(
#'    "columns" = list(
#'       list(
#'         "name" = "study",
#'         "display_name" = "Consortium",
#'         "type" = "character"
#'       ),
#'      list(
#'         "name" = "month",
#'         "display_name" = "Month",
#'         "type" = "integer"
#'       ),
#'      )
#'     )
#' @importFrom magrittr %>% 
#' @importFrom rlang .data
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

#' Recode Column Values
#' This function will recode a column of values in a tibble based o the list 
#' passed. If no list is passed none of the values will be replaces unless 
#' optional arguments are passed via ... to dplyr::recode such as .default
#' or .missing
#'
#' @param tbl A tibble
#' 
#' data <-  dplyr::tribble(
#'  ~col1, ~col2,
#'  "a",   "c",
#'  "a",   "d",
#'  "b",   NA
#')
#' 
#' @param column A string that is the name of a column in the tbl that will be 
#' recoded
#' @param lst A named list where each name is a possible value in the column,
#' and the values are their replacements
#' 
#' lst <- list("a" = "x", "b" = "y", "c" = "z")
#' 
#' @param ... Other arguments to dplyr::recode
recode_column_values <- function(tbl, column, lst = NULL, ...){
  if(is.null(lst)) lst <- list("0" = "0")
  col_var <- rlang::sym(column)
  tbl <- dplyr::mutate(tbl, !!col_var := dplyr::recode(!!col_var, !!!lst, ...)) 
  dplyr::pull(tbl, column)
  return(tbl)
}

#' Recode Dataframe With Parameter List
#' This function replaces values in a character column. If the parameter list
#' has "replace_values" field, this will be the mapping used. If the parameter
#' list has a "deafult_replace", anything not in the replace values list will be
#' replaced by this value. If the parameter list has a "na_replace" field NA 
#' values will be replaced by this. 
#' 
#' It is assumed that this table has been run through 
#' concatenate_df_list_columns_with_param_list, as columns of type character,
#' and those that were of list:character will be recoded.
#'
#' @param tbl A tibble
#' 
#'  data <-  dplyr::tribble(
#'   ~col1, ~col2,
#'   "a",   "c",
#'   "a",   "d",
#'   "b",   NA
#'  )
#'
#' @param param_list A list with a named list named "columns" that has an entry  
#' for each column needed in the tibble. Each column must have a "name",
#' and "type" field. Optional fields include "replace_values", "display_name",
#' "na_replace", and "deafult_replace".
#' 
#'   param_list1 <- list(
#'    "columns" = list(
#'      "col1" = list(
#'        "name" = "col1",
#'        "type" = "character",
#'        "replace_values" = list(
#'          "a" = "A"
#'        )
#'      ),
#'      "col2" = list(
#'        "name" = "col2",
#'        "type" = "character",
#'        "replace_values" = list(
#'          "c" = "C"
#'         ),
#'        "na_replace" = "Missing",
#'        "default_replace" = "Other"
#'      )
#'     )
#' @importFrom magrittr %>% 
recode_df_with_param_list <- function(tbl, param_list){
  column_param_list <- param_list %>% 
    purrr::pluck("columns") %>%  
    purrr::keep(
      ., 
      purrr::map(., purrr::pluck("type")) %in% c("character", "list:character")
    )
  for (param_list in column_param_list) {
    tbl <- recode_column_values(
      tbl,
      param_list$name, 
      param_list$replace_values,
      .default = param_list$default_replace,
      .missing = param_list$na_replace
    )
  }
  return(tbl)
}

#' Get Number Of Distinct Values From Column
#' This function returns the number of distinct values, including NA's in
#' a column
#'
#' @param tbl A Tibble
#' @param column A string that is the name of a column in the tibble
#' @importFrom magrittr %>% 
get_distinct_value_from_column <- function(tbl, column){
  tbl %>% 
    dplyr::pull(column) %>% 
    dplyr::n_distinct()
}

#' Add Distinct Values From Columns
#' This function find sthe number of distinct values from one or more columns
#' and returns the sum of those.
#'
#' @param tbl A tibble
#' @param columns a list of strings that are names of columns in the tibble
#' @importFrom magrittr %>% 
add_distinct_values_from_columns <- function(tbl, columns){
  result <-  
    purrr::map_int(columns, ~get_distinct_value_from_column(tbl, .x)) %>% 
    sum()
}

#' Create Plot Dataframe From Count Dataframe
#' This function creates a summary of the name and value column.
#' 
#' The name column is filtered for only values that equal the column_value.
#' 
#' The name and value column are pivoted so that a new column is created that
#' has the name of the column value. The number of rows will equal the number
#' of unique value in the value column, where the name column is equal to
#' the column_value
#' 
#' Finally the counts column is renamed the using the count_column
#' 
#' 
#' @param column_value A value that exists in the "name" column of the data
#' @param count_column A string that is the new column name
#' @param data A tibble
#' @importFrom magrittr %>% 
#' @importFrom rlang := .data
#' @examples
#'  data <- dplyr::tribble(
#'  ~studyName, ~name,          ~value,                   ~count,
#'  "s1",       "assay",        "immunohistochemistry",   395L,
#'  "s1",       "resourceType", "experimentalData",       416L,
#'  "s1",       "resourceType", "report",                 12L,
#'  "s1",       "species",      "Human",                  421L,
#'  "s1",       "tumorType",    "Cutaneous Neurofibroma", 387L
#' )
#' create_plot_df_from_count_df("assay", "Assays", data)
create_plot_df_from_count_df <- function(column, count_column, data){
  data %>%
    dplyr::filter(.data$name == column) %>% 
    tidyr::pivot_wider() %>% 
    dplyr::rename(!!rlang::ensym(count_column) := "count")
}

#' Create Plot Dataframes From Count Dataframe
#' See create_plot_df_from_count_df for outputs
#' @param column_values A list of values that exists in the "name" column 
#' of the data
#' @param count_columns A list of strings that will the new column names
#' @param data A tibble
create_plot_dfs_from_count_df <- function(column_values, count_columns, data){
  purrr::map2(
    column_values,
    count_columns,
    create_plot_df_from_count_df,
    data
  )
}


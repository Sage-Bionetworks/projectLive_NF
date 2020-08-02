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
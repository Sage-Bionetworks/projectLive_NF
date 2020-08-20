create_info_box <- function(param_list, group_object){
  shinydashboard::infoBox(
    title = param_list$title,
    value = add_distinct_values_from_columns(
      group_object[[param_list$table]],
      param_list$columns
    ),
    icon = shiny::icon(param_list$icon),
    color = "light-blue",
    fill = TRUE
  )
}
test_that("create_plot_with_param_list", {
  data <- dplyr::tibble(
    "Study Leads" = c("s1", "s2", "s3"),
    "Resource Type" = c("r1", "r2", "r3"),
    "Year" = c(2000L, 2001L, 2002L),
    "Month" = factor("Jul", "Jul", "Jun")
  )
  param_list <- list(
    "plot" = list(
      "x" = "Study Leads",
      "fill" = "Resource Type",
      "facet" = list("Year", "Month")
    ),
    "tooltips" = list("count", "fill")
  )
  fig <- create_plot_with_param_list(
    data, param_list, "create_file_upload_timeline_plot"
  )
  print(fig)
  expect_type(fig, "list")
})
# 
# test_that("create_study_per_consortium_plot", {
#   data <- dplyr::tibble(
#     "Consortium" = c("c1", "c2", "c3", "c1", "c2"),
#     "Access Type" = c("a1", "a2", "a1", "a1", "a1"),
#     "Year" = c(2001L, 2001L, 2001L, 2002L, 2002L)
#   )
#   p <- create_study_per_consortium_plot(
#     data  = data, 
#     x     = "Consortium",
#     fill  = "Access Type",
#     "Year"
#   )
#   p <-  plotly::ggplotly(p, tooltip = c("count", "Access Type"))
#   print(p)
#   expect_type(p, "list")
# })
# 
# test_that("create_files_per_study_plot", {
#   tbl <- dplyr::tibble(
#     "c1" = c("a", "b", "c"),
#     "c2" = c("d", "e", "f"),
#     "c3" = c("x", "y", "z"),
#     "YEAR" = c(2000L, 2001L, 2002L)
#   )
#   p <- create_files_per_study_plot(tbl, "c1", "c3", "c3", "YEAR")
#   print(p)
#   expect_type(p, "list")
# })
# 
# test_that("create_file_upload_timeline_plot", {
#   tbl <- dplyr::tibble(
#     "Study Leads" = c("s1", "s2", "s3"),
#     "Resource Type" = c("r1", "r2", "r3"),
#     "Year" = c(2000L, 2001L, 2002L),
#     "Month" = factor("Jul", "Jul", "Jun")
#   )
#   
#   fig1 <- create_file_upload_timeline_plot(
#     tbl, 
#     x = "Study Leads", 
#     fill = "Resource Type", 
#     list("Year", "Month")
#   ) %>%
#     plotly::ggplotly(
#       tooltip = c("count", "fill")
#     )
#   
#   print(fig1)
#   expect_type(fig1, "list")
#   
#   fig2 <- create_file_upload_timeline_plot(
#     tbl, 
#     x = "Study Leads", 
#     fill = "Resource Type", 
#     list("Year")
#   ) %>%
#     plotly::ggplotly(
#       tooltip = c("count", "fill")
#     )
#   
#   print(fig2)
#   expect_type(fig2, "list")
# })
# 


test_that("create_study_per_consortium_plot", {
  data <- dplyr::tibble(
    "Consortium" = c("c1", "c2", "c3", "c1", "c2"),
    "Access Type" = c("a1", "a2", "a1", "a1", "a1"),
    "Year" = c(2001L, 2001L, 2001L, 2002L, 2002L)
  )
  p <- create_study_per_consortium_plot(
    data  = data, 
    x     = "Consortium",
    fill  = "Access Type",
    "Year"
  )
  p <-  plotly::ggplotly(p, tooltip = c("count", "Access Type"))
  print(p)
  expect_type(p, "list")
})

test_that("create_files_per_study_plot", {
  tbl <- dplyr::tibble(
    "c1" = c("a", "b", "c"),
    "c2" = c("d", "e", "f"),
    "c3" = c("x", "y", "z"),
    "YEAR" = c(2000L, 2001L, 2002L)
  )
  p <- create_files_per_study_plot(tbl, "c1", "c3", "c3", "YEAR")
  print(p)
  expect_type(p, "list")
})

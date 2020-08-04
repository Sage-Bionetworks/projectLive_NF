test_that("create_study_per_consortium_plot", {
  data <- dplyr::tibble(
    "Consortium" = c("c1", "c2", "c3", "c1", "c2"),
    "Study Name" = c("s1", "s2", "s3", "s4", "s1"),
    "Access Type" = c("a1", "a2", "a1", "a1", "a1"),
    "Year" = c(2001L, 2001L, 2001L, 2002L, 2002L)
  )
  param_list <- purrr::pluck(
    data_config,
    "modules",
    "summary_snapshot",
    "outputs",
    "study_per_consortium"
  )
  p <- create_study_per_consortium_plot(
    data  = data, 
    x     = param_list$columns$x$display_name,
    y     = param_list$columns$y$display_name,
    fill  = param_list$columns$fill$display_name,
    color = param_list$columns$fill$display_name,
    param_list$columns$facet$display_name
  )
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

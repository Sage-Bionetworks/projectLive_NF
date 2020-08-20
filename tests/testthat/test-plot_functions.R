test_that("create_plot_with_param_list", {
  data <- dplyr::tibble(
    "Study Leads" = c("s1", "s2", "s3"),
    "Resource Type" = c("r1", "r2", "r3"),
    "Year" = c(2000L, 2001L, 2002L),
    "Month" = factor("Jul", "Jul", "Jun"),
    "Count" = c(10, 30, 40)
  )
  param_list <- list(
    "plot" = list(
      "x" = "Study Leads",
      "y" = "Count",
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

test_that("create_consortium_activity_plot", {
  data <- dplyr::tibble(
    "Consortium" = c("c1", "c2", "c3", "c1", "c2"),
    "Access Type" = c("a1", "a2", "a1", "a1", "a1"),
    "Year" = c(2001L, 2001L, 2001L, 2002L, 2002L)
  )
  p <- create_consortium_activity_plot(
    data  = data,
    x     = "Consortium",
    fill  = "Access Type",
    "Year"
  )
  p <-  plotly::ggplotly(p, tooltip = c("count", "Access Type"))
  print(p)
  expect_type(p, "list")
})


test_that("create_file_upload_timeline_plot", {
  tbl1 <- dplyr::tibble(
    "Study Leads" = c("s1", "s1", "s2", "s3", "s4"),
    "Resource Type" = c("r1", "r2", "r2", "r3", NA),
    "Year" = c(2000L, 2000L, 2001L, 2002L, 2000L),
    "Count" = c(2,1,2,5,0)
  )
  
  fig1 <- create_file_upload_timeline_plot(
    tbl1,
    x = "Study Leads",
    y = "Count",
    fill = "Resource Type",
    facet = "Year"
  ) 
  print(fig1)
  plotly::ggplotly(fig1, tooltip = c("Count", "fill"))
  print(fig1)
  expect_type(fig1, "list")
  
  tbl2 <- dplyr::tibble(
    "Study Leads" = c("s1", "s2", "s3"),
    "Resource Type" = c("r1", "r2", "r3"),
    "Year" = c(2000L, 2001L, 2002L)
  ) %>% 
    dplyr::mutate("Study Leads" = forcats::as_factor(`Study Leads`)) %>% 
    tidyr::drop_na() %>% 
    dplyr::count(`Study Leads`, `Resource Type`, `Year`, name = "Count") %>% 
    tidyr::complete(`Study Leads`, `Resource Type`, `Year`, fill = list("Count" = 0)) %>% 
    dplyr::mutate("Resource Type" = dplyr::if_else(
      .data$Count == 0,
      NA_character_,
      .data$`Resource Type`
    ))

  fig2 <- create_file_upload_timeline_plot(
    tbl2,
    x = "Study Leads",
    y = "Count",
    fill = "Resource Type",
    facet = "Year"
  ) 
  print(fig2)
  plotly::ggplotly(fig1, tooltip = c("Count", "fill"))
  print(fig2)
  expect_type(fig2, "list")

})



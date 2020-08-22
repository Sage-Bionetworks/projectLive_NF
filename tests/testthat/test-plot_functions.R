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
  expect_type(fig, "list")
})

test_that("create_consortium_activity_plot", {
  data <- dplyr::tibble(
    "Consortium" = c("c1", "c2", "c3", "c1", "c2"),
    "Access Type" = c("a1", "a2", "a1", "a1", "a1"),
    "Year" = c(2001L, 2001L, 2001L, 2002L, 2002L)
  )
  fig <- create_consortium_activity_plot(
    data  = data,
    x     = "Consortium",
    fill  = "Access Type",
    "Year"
  ) %>% 
    plotly::ggplotly(tooltip = c("count", "Access Type"))
  expect_type(fig, "list")
})

test_that("create_resources_generated_plot",{
  data <- dplyr::tibble(
    "Study Name" = c("s1", "s1", "s2", "s3"),
    "Data Type" = c("d1", "d2", "d2", "d3"),
    "Year" = c(2000L, 2000L, 2001L, 2002L)
  )
  
  fig <- create_resources_generated_plot(
    data,
    x = "Study Name",
    fill = "Data Type",
    facet = "Year"
  ) %>% 
    plotly::ggplotly(tooltip = c("Count", "fill"))
  expect_type(fig, "list")
})

test_that("create_publication_status_plot",{
  data <- dplyr::tibble(
    "Study Name" = c("s1", "s1", "s2", "s3"),
    "Year" = c(2000L, 2000L, 2001L, 2002L)
  )
  
  fig <- create_publication_status_plot(
    data,
    x = "Year",
    fill = "Study Name"
  ) %>% 
    plotly::ggplotly(tooltip = c("Count", "fill"))
  expect_type(fig, "list")
})

test_that("create_publication_disease_plot",{
  data <- dplyr::tibble(
    "Manifestation" = c("m1", "m1", "m2", "m3"),
    "Year" = c(2000L, 2000L, 2001L, 2002L)
  )
  
  fig <- create_publication_disease_plot(
    data,
    x = "Year",
    fill = "Manifestation"
  ) %>% 
    plotly::ggplotly(tooltip = c("Count", "fill"))
  expect_type(fig, "list")
})


test_that("create_file_upload_timeline_plot", {
  data <- dplyr::tibble(
    "Study Leads" = c("s1", "s1", "s2", "s3", "s4"),
    "Resource Type" = c("r1", "r2", "r2", "r3", NA),
    "Year" = c(2000L, 2000L, 2001L, 2002L, 2000L),
    "Count" = c(2,1,2,5,0)
  )
  
  fig <- create_file_upload_timeline_plot(
    data,
    x = "Study Leads",
    y = "Count",
    fill = "Resource Type",
    facet = "Year"
  ) %>% 
    plotly::ggplotly(tooltip = c("Count", "fill"))
  expect_type(fig, "list")
})

test_that("create_annotation_activity_plot", {
  data <- dplyr::tibble(
    "Study Leads" = c("s1", "s1", "s2", "s3", "s4"),
    "Assay" = c("a1", "a2", "a2", "a3", NA),
    "Year" = c(2000L, 2000L, 2001L, 2002L, 2000L),
    "Count" = c(2,1,2,5,0)
  )
  
  fig <- create_annotation_activity_plot(
    data,
    x = "Study Leads",
    y = "Count",
    fill = "Assay",
    facet = "Year"
  ) %>% 
    plotly::ggplotly(tooltip = c("Count", "fill"))
  expect_type(fig, "list")
})

test_that("create_data_focus_plots",{
  
  data_list <- list(
    "Assays" = dplyr::tribble(
      ~`Study Name`, ~Assays,
      "s1",          "a1",    
      "s1",          "a2"    
    ),
    "Resources" = dplyr::tribble(
      ~`Study Name`, ~Resources,
      "s1",          "r1",       
      "s1",          "r1"
    )
  )
  
  param_list <- list(
    "plot" = list(
      "x" = "Study Name",
      "fill" = list(
        "Assay",
        "Resources"
      )
    )
  )
  fig <- create_data_focus_plots(data_list, param_list)
  expect_type(fig, "list")
})

test_that("create_data_focus_plot",{
  data <- dplyr::tibble(
    "Study Name" = c("s1", "s1", "s1", "s1"),
    "Assay" = c("a1", "a2", "a1", "a2")
  ) 
  
  fig <- create_data_focus_plot(
    data,
    x = "Study Name",
    fill = "Assay"
  ) %>% 
    plotly::ggplotly(tooltip = c("Count", "fill"))
  expect_type(fig, "list")
})

test_that("create_study_timeline_plot",{
  data <- dplyr::tibble(
    "Study Name" = c("s1", "s1", "s1"),
    "Resource Type" = c("a1", "a2", "a1"),
    "Year" = c(2000L, 2001L, 2002L),
    "Month" = factor("Jul", "Jul", "Jun"),
  ) 
  
  fig <- create_study_timeline_plot(
    data,
    x = "Study Name",
    fill = "Resource Type",
    facet = list("Year", "Month")
    
  ) %>% 
    plotly::ggplotly(tooltip = c("Count", "fill"))
  expect_type(fig, "list")
})




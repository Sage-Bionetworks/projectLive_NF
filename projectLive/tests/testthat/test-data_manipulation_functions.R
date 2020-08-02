require(magrittr)
require(rlang)

count_df <- dplyr::tribble(
  ~studyName, ~name,          ~value,                   ~count,
  "s1",       "assay",        "immunohistochemistry",   395L,
  "s1",       "resourceType", "experimentalData",       416L,
  "s1",       "resourceType", "report",                 12L,
  "s1",       "species",      "Human",                  421L,
  "s1",       "tumorType",    "Cutaneous Neurofibroma", 387L
)

test_that("create_plot_df_from_count_df ", {
  result1 <- create_plot_df_from_count_df("assay", "Assays", count_df)
  expect_named(result1, c("studyName", "Assays", "assay"))
  expect_equal(nrow(result1), 1)
  expect_equal(result1$Assays, 395L)
  result2 <- create_plot_df_from_count_df("resourceType", "Resources", count_df)
  expect_named(result2, c("studyName", "Resources", "resourceType"))
  expect_equal(nrow(result2), 2)
  expect_equal(result2$Resources, c(416L, 12L))
})

test_that("create_plot_dfs_from_count_df ", {
  columns       <- c("assay", "resourceType")
  count_columns <- c("Assays", "Resources")
  
  result <- create_plot_dfs_from_count_df(columns, count_columns, count_df)
  
  expect_named(result[[1]], c("studyName", "Assays", "assay"))
  expect_named(result[[2]], c("studyName", "Resources", "resourceType"))
  
  expect_equal(nrow(result[[1]]), 1)
  expect_equal(nrow(result[[2]]), 2)
  
  expect_equal(result[[1]]$Assays, 395L)
  expect_equal(result[[2]]$Resources, c(416L, 12L))
})



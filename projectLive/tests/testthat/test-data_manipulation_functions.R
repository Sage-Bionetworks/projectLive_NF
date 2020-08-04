test_that("concatenate_list_columns", {
  tbl1 <- dplyr::tibble(
    "cola" = list(c("a", "b"), "a", c("a", "c")),
    "colb" = c("a", "b", "c")
  )
  col1 <- "cola"
  col2 <- "colb"
  res1 <- concatenate_list_columns(tbl1, col1)
  res2 <- concatenate_list_columns(tbl1, col2)
  res3 <- concatenate_list_columns(tbl1, c(col1, col2))
  expect_equal(
    res1,
    dplyr::tibble(
      "cola" = c("a | b", "a", "a | c"),
      "colb" = c("a", "b", "c")
    )
  )
  expect_equal(res2, tbl1)
  expect_equal(
    res3,
    dplyr::tibble(
      "cola" = c("a | b", "a", "a | c"),
      "colb" = c("a", "b", "c")
    )
  )
})


test_that("safe_pluck_list", {
  list1 = list(
    list("name" = "col1", "display_name" = "Column1", "type" = "x"),
    list("name" = "col2", "type" = "x")
  )
  res1 <- safe_pluck_list(list1, "name")
  res2 <- safe_pluck_list(list1, "display_name")
  expect_equal(res1, c("col1", "col2"))
  expect_equal(res2, c("Column1", NA))
})


test_that("rename_df_columns_with_param_list", {
  tbl1 <- dplyr::tibble("col1" = c(), "col2" = c(), "colNum3" = c())
  param_list <- list(
    "columns" = list(
      "col1" = list("name" = "col1", "display_name" = "Column1", "type" = "x"),
      "col2" = list("name" = "col2", "type" = "x"),
      "col3" = list("name" = "colNum3", "type" = "x")
    )
  )
  res1 <- rename_df_columns_with_param_list(tbl1, param_list)
  expect_equal(
    res1, 
    dplyr::tibble("Column1" = c(), "Col2" = c(), "Colnum3" = c())
  )
})

test_that("recode_column_values", {
  tbl1 <- dplyr::tibble("col1" = c("a", "a", "b"), "col2" = c("c", "d", NA))
  lst1 <- list("a" = "x", "b" = "y", "c" = "z")
  col1 <- "col1"
  col2 <- "col2"

  res1 <- recode_column_values(tbl1, col1, lst1)
  expect_equal(
    res1,
    dplyr::tibble("col1" = c("x", "x", "y"), "col2" = c("c", "d", NA))
  )
  res2 <- recode_column_values(tbl1, col2, lst1, .missing = "missing")
  expect_equal(
    res2,
    dplyr::tibble("col1" = c("a", "a", "b"), "col2" = c("z", "d", "missing"))
  )
  res3 <- recode_column_values(tbl1, col2, list(), .missing = "missing")
  expect_equal(
    res3,
    dplyr::tibble("col1" = c("a", "a", "b"), "col2" = c("c", "d", "missing"))
  )
  res4 <- recode_column_values(tbl1, col2, lst = NULL, .missing = "missing")
  expect_equal(
    res4,
    dplyr::tibble("col1" = c("a", "a", "b"), "col2" = c("c", "d", "missing"))
  )
  res5 <- recode_column_values(tbl1, col2)
  expect_equal(
    res5,
    dplyr::tibble("col1" = c("a", "a", "b"), "col2" = c("c", "d", NA))
  )
})

test_that("recode_column_values_with_param_list", {
  tbl1 <- dplyr::tibble("col1" = c("a", "a", "b"), "col2" = c("c", "d", NA))
  param_list1 <- list(
    "name" = "col1",
    "replace_values" = list(
      "a" = "x",
      "b" = "y",
      "c" = "z"
    )
  )
  param_list2 <- list(
    "name" = "col2",
    "replace_values" = list(
      "a" = "x",
      "b" = "y",
      "c" = "z"
    ),
    "na_replace" = "M",
    "default_replace" = "O"
  )
  param_list3 <- list(
    "name" = "col2",
    "na_replace" = "M",
    "default_replace" = "O"
  )
  param_list4 <- list(
    "name" = "col2",
    "na_replace" = "M"
  )
  param_list5 <- list(
    "name" = "col2"
  )
  res1 <- recode_column_values_with_param_list(tbl1, param_list1)
  res2 <- recode_column_values_with_param_list(tbl1, param_list2)
  res3 <- recode_column_values_with_param_list(tbl1, param_list3)
  res4 <- recode_column_values_with_param_list(tbl1, param_list4)
  res5 <- recode_column_values_with_param_list(tbl1, param_list5)
  expect_equal(
    res1,
    dplyr::tibble("col1" = c("x", "x", "y"), "col2" = c("c", "d", NA))
  )
  expect_equal(
    res2,
    dplyr::tibble("col1" = c("a", "a", "b"), "col2" = c("z", "O", "M"))
  )
  expect_equal(
    res3,
    dplyr::tibble("col1" = c("a", "a", "b"), "col2" = c("O", "O", "M"))
  )
  expect_equal(
    res4,
    dplyr::tibble("col1" = c("a", "a", "b"), "col2" = c("c", "d", "M"))
  )
  expect_equal(res5, tbl1)
})

test_that("recode_df_with_param_list", {
  tbl1 <- dplyr::tibble(
    "col1" = c("a", NA, "b"), 
    "col2" = c("c", "d", NA),
    "col3" = c(1L, 2L, NA),
    )
  param_list1 <- list(
    "columns" = list(
      "col1" = list(
        "name" = "col1",
        "type" = "character",
        "replace_values" = list(
          "a" = "x"
        )
      ),
      "col2" = list(
        "name" = "col2",
        "type" = "character",
        "replace_values" = list(
          "c" = "z"
        ),
        "na_replace" = "M",
        "default_replace" = "O"
      ),
      "col3" = list(
        "name" = "col3",
        "type" = "integer"
      )
    )
  )
  res1 <- recode_df_with_param_list(tbl1, param_list1)
  expect_equal(
    res1,
    dplyr::tibble(
      "col1" = c("x", NA, "b"), 
      "col2" = c("z", "O", "M"),
      "col3" = c(1L, 2L, NA),
    )
  )
})



test_that("add_distinct_values_from_columns", {
  tbl <- dplyr::tibble(
    "col1" = c("a", "a", "b"),
    "col2" = c("c", "d", "e"),
    "col3" = c("a", "b", "e")
  )
  expect_equal(add_distinct_values_from_columns(tbl, "col1"), 2)
  expect_equal(add_distinct_values_from_columns(tbl, "col2"), 3)
  expect_equal(add_distinct_values_from_columns(tbl, c("col1", "col2")), 5)
  expect_equal(add_distinct_values_from_columns(
    tbl, c("col1", "col2", "col3")), 8
  )
})

test_that("get_distinct_value_from_column", {
  df <- dplyr::tibble(
    "col1" = c("a", "a", "b"),
    "col2" = c("c", "d", "e")
  )
  expect_equal(get_distinct_value_from_column(df, "col1"), 2)
  expect_equal(get_distinct_value_from_column(df, "col2"), 3)
})


# test_that("build_translation_df_from_list", {
#   lst <- list(
#     list("name" = "assay1", "display_name" = "Assay1"),
#     list("name" = "assay2", "display_name" = "Assay2")
#   )
#   result <- build_translation_df_from_list(lst, "name", "display_name")
#   expect_named(result, c("name", "new_name"))
# })
# 
# test_that("build_translation_df_from_list", {
#   lst <- list(
#     list("name" = "assay1", "display_name" = "Assay1"),
#     list("name" = "assay2", "display_name" = "Assay2")
#   )
#   result <- build_translation_df_from_list(lst, "name", "display_name")
#   expect_named(result, c("name", "new_name"))
# })

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



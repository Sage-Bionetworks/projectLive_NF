test_that("replace_values_if_col_value_in_list", {
  data <- dplyr::tribble(
    ~x,  ~y,  ~Count,
    "a", "a", 1L,
    "b", "b", 2L
  )
  expected_result1 <- dplyr::tribble(
    ~x,  ~y,  ~Count,
    NA,  "a", 1L,
    "b", "b", 2L
  )
  expect_equal(
    expected_result1,
    replace_values_if_col_value_in_list(data, "Count", 1L, "x")
  )
  expected_result2 <- dplyr::tribble(
    ~x,  ~y,  ~Count,
    NA,  NA,  1L,
    "b", "b", 2L
  )
  expect_equal(
    expected_result2,
    replace_values_if_col_value_in_list(data, "Count", 1L, c("x", "y"))
  )
  expected_result3 <- dplyr::tribble(
    ~x,             ~y,            ~Count,
    NA_character_,  NA_character_, 1L,
    NA_character_,  NA_character_, 2L
  )
  expect_equal(
    expected_result3,
    replace_values_if_col_value_in_list(data, "Count", c(1L, 2L), c("x", "y"))
  )
})

test_that("create_plot_count_df", {
  data <- dplyr::tribble(
    ~Lead,  ~fill,  ~Year,
    "a",    "a",    2000L, 
    "a",    "a",    2000L, 
    "a",    "a",    2001L,
    "b",    "a",    2001L,
    "c",    NA,     NA    
  )
  
  expected_result1 <- dplyr::tribble(
    ~Lead,  ~Year,  ~fill, ~Count,
    "a",    2000L,  "a",   2L,
    "a",    2001L,  "a",   1L,
    "b",    2000L,  NA,    0L,
    "b",    2001L,  "a",   1L,
    "c",    2000L,  NA,    0L,
    "c",    2001L,  NA,    0L
  ) %>% 
    dplyr::mutate("Lead" = forcats::as_factor(.data$Lead))
  
  expect_equal(
    create_plot_count_df(data, "Lead", c("Lead", "Year")), 
    expected_result1
  )
})


# 
# test_that("format_plot_data_with_param_list", {
#   param_list <- list(
#     "columns" = list(
#       list(
#         "name" = "consortium",
#         "display_name" = "Consortium",
#         "na_replace" = "Not Applicable",
#         "type" = "character"
#       ),
#       list(
#         "name" = "year",
#         "display_name" = "Year",
#         "type" = "integer"
#       )
#     )
#   )
#   data <- dplyr::tribble(
#     ~consortium, ~year, ~month,
#     NA,          2000L, NA,
#     "c1",        20001, "January"
#   )
#   expected_result <- dplyr::tribble(
#     ~Consortium,      ~Year,
#     "Not Applicable", 2000L,
#     "c1",             20001
#   )
#   expect_equal(
#     expected_result, 
#     format_plot_data_with_param_list(data, param_list)
#   )
# })
# 
# test_that("create_data_focus_tables", {
#   data <- dplyr::tribble(
#     ~Study,  ~Assay, ~Resource, ~Year,
#     "s1",    "a1",   "r1",      2001L,
#     "s2",    "a2",   NA,        2002L
#   )
#   expected_results <- list(
#     "Assay" = dplyr::tribble(
#       ~Study,  ~Assay,
#       "s1",    "a1",
#       "s2",    "a2"
#     ),
#     "Resource" = dplyr::tribble(
#       ~Study,  ~Resource,
#       "s1",    "r1"
#     )
#   )
#   expect_equal(
#     expected_results, 
#     create_data_focus_tables(data, "Study", c("Assay", "Resource"))
#   )
# })
# 
# test_that("concatenate_list_columns", {
#   tbl1 <- dplyr::tibble(
#     "cola" = list(c("a", "b"), "a", c("a", "c")),
#     "colb" = c("a", "b", "c")
#   )
#   col1 <- "cola"
#   col2 <- "colb"
#   res1 <- concatenate_list_columns(tbl1, col1)
#   res2 <- concatenate_list_columns(tbl1, col2)
#   res3 <- concatenate_list_columns(tbl1, c(col1, col2))
#   expect_equal(
#     res1,
#     dplyr::tibble(
#       "cola" = c("a | b", "a", "a | c"),
#       "colb" = c("a", "b", "c")
#     )
#   )
#   expect_equal(res2, tbl1)
#   expect_equal(
#     res3,
#     dplyr::tibble(
#       "cola" = c("a | b", "a", "a | c"),
#       "colb" = c("a", "b", "c")
#     )
#   )
# })
# 
# test_that("safe_pluck_list", {
#   list1 = list(
#     list("name" = "col1", "display_name" = "Column1", "type" = "x"),
#     list("name" = "col2", "type" = "x")
#   )
#   res1 <- safe_pluck_list(list1, "name")
#   res2 <- safe_pluck_list(list1, "display_name")
#   expect_equal(res1, c("col1", "col2"))
#   expect_equal(res2, c("Column1", NA))
# })
# 
# test_that("rename_df_columns_with_param_list", {
#   tbl1 <- dplyr::tibble("col1" = c(), "col2" = c(), "colNum3" = c())
#   param_list1 <- list(
#     "columns" = list(
#       "col1" = list("name" = "col1", "display_name" = "Column1", "type" = "x"),
#       "col2" = list("name" = "col2", "type" = "x"),
#       "col3" = list("name" = "colNum3", "type" = "x")
#     )
#   )
#   param_list2 <- list(
#     "columns" = list(
#       "col1" = list("name" = "col1", "display_name" = "Column1", "type" = "x"),
#       list("name" = "col2", "type" = "x", "display_name" = "Column2"),
#       list("name" = "colNum3", "type" = "x", "display_name" = "Column3")
#     )
#   )
#   param_list3 <- list(
#     "columns" = list(
#       "col2" = list("name" = "col2", "type" = "x"),
#       "col3" = list("name" = "colNum3", "type" = "x")
#     )
#   )
#   res1 <- rename_df_columns_with_param_list(tbl1, param_list1)
#   res2 <- rename_df_columns_with_param_list(tbl1, param_list2)
#   res3 <- rename_df_columns_with_param_list(tbl1, param_list3)
#   expect_equal(
#     res1,
#     dplyr::tibble("Column1" = c(), "Col2" = c(), "Colnum3" = c())
#   )
#   expect_equal(
#     res2,
#     dplyr::tibble("Column1" = c(), "Column2" = c(), "Column3" = c())
#   )
#   expect_equal(
#     res3,
#     dplyr::tibble("Col2" = c(), "Colnum3" = c())
#   )
# })
# 
# 
# test_that("recode_column_values", {
#   tbl1 <- dplyr::tribble(
#     ~col1, ~col2,
#     "a",   "c",
#     "a",   "d",
#     "b",   NA
#   )
#   lst1 <- list("a" = "x", "b" = "y", "c" = "z")
#   col1 <- "col1"
#   col2 <- "col2"
# 
#   res1 <- recode_column_values(tbl1, col1, lst1)
#   expect_equal(
#     res1,
#     dplyr::tibble("col1" = c("x", "x", "y"), "col2" = c("c", "d", NA))
#   )
#   res2 <- recode_column_values(tbl1, col2, lst1, .missing = "missing")
#   expect_equal(
#     res2,
#     dplyr::tibble("col1" = c("a", "a", "b"), "col2" = c("z", "d", "missing"))
#   )
#   res3 <- recode_column_values(tbl1, col2, list(), .missing = "missing")
#   expect_equal(
#     res3,
#     dplyr::tibble("col1" = c("a", "a", "b"), "col2" = c("c", "d", "missing"))
#   )
#   res4 <- recode_column_values(tbl1, col2, lst = NULL, .missing = "missing")
#   expect_equal(
#     res4,
#     dplyr::tibble("col1" = c("a", "a", "b"), "col2" = c("c", "d", "missing"))
#   )
#   res5 <- recode_column_values(tbl1, col2)
#   expect_equal(
#     res5,
#     dplyr::tibble("col1" = c("a", "a", "b"), "col2" = c("c", "d", NA))
#   )
# })
# 
# 
# test_that("recode_df_with_param_list", {
#   tbl1 <- dplyr::tibble(
#     "col1" = c("a", NA, "b"), 
#     "col2" = c("c", "d", NA),
#     "col3" = c(1L, 2L, NA),
#     )
#   param_list1 <- list(
#     "columns" = list(
#       "col1" = list(
#         "name" = "col1",
#         "type" = "character",
#         "replace_values" = list(
#           "a" = "x"
#         )
#       ),
#       "col2" = list(
#         "name" = "col2",
#         "type" = "character",
#         "replace_values" = list(
#           "c" = "z"
#         ),
#         "na_replace" = "M",
#         "default_replace" = "O"
#       ),
#       "col3" = list(
#         "name" = "col3",
#         "type" = "integer"
#       )
#     )
#   )
#   res1 <- recode_df_with_param_list(tbl1, param_list1)
#   expect_equal(
#     res1,
#     dplyr::tibble(
#       "col1" = c("x", NA, "b"), 
#       "col2" = c("z", "O", "M"),
#       "col3" = c(1L, 2L, NA),
#     )
#   )
# })
# 
# test_that("add_distinct_values_from_columns", {
#   tbl <- dplyr::tibble(
#     "col1" = c("a", "a", "b"),
#     "col2" = c("c", "d", "e"),
#     "col3" = c("a", "b", "e")
#   )
#   expect_equal(add_distinct_values_from_columns(tbl, "col1"), 2)
#   expect_equal(add_distinct_values_from_columns(tbl, "col2"), 3)
#   expect_equal(add_distinct_values_from_columns(tbl, c("col1", "col2")), 5)
#   expect_equal(add_distinct_values_from_columns(
#     tbl, c("col1", "col2", "col3")), 8
#   )
# })
# 
# test_that("get_distinct_value_from_column", {
#   df <- dplyr::tibble(
#     "col1" = c("a", "a", "b"),
#     "col2" = c("c", "d", "e")
#   )
#   expect_equal(get_distinct_value_from_column(df, "col1"), 2)
#   expect_equal(get_distinct_value_from_column(df, "col2"), 3)
# })
# 
# 
# count_df <- dplyr::tribble(
#   ~studyName, ~name,          ~value,                   ~count,
#   "s1",       "assay",        "immunohistochemistry",   395L,
#   "s1",       "resourceType", "experimentalData",       416L,
#   "s1",       "resourceType", "report",                 12L,
#   "s1",       "species",      "Human",                  421L,
#   "s1",       "tumorType",    "Cutaneous Neurofibroma", 387L
# )
# 
# test_that("create_plot_df_from_count_df ", {
#   result1 <- create_plot_df_from_count_df("assay", "Assays", count_df)
#   expect_named(result1, c("studyName", "Assays", "assay"))
#   expect_equal(nrow(result1), 1)
#   expect_equal(result1$Assays, 395L)
#   result2 <- create_plot_df_from_count_df("resourceType", "Resources", count_df)
#   expect_named(result2, c("studyName", "Resources", "resourceType"))
#   expect_equal(nrow(result2), 2)
#   expect_equal(result2$Resources, c(416L, 12L))
# })
# 
# test_that("create_plot_dfs_from_count_df ", {
#   columns       <- c("assay", "resourceType")
#   count_columns <- c("Assays", "Resources")
# 
#   result <- create_plot_dfs_from_count_df(columns, count_columns, count_df)
# 
#   expect_named(result[[1]], c("studyName", "Assays", "assay"))
#   expect_named(result[[2]], c("studyName", "Resources", "resourceType"))
# 
#   expect_equal(nrow(result[[1]]), 1)
#   expect_equal(nrow(result[[2]]), 2)
# 
#   expect_equal(result[[1]]$Assays, 395L)
#   expect_equal(result[[2]]$Resources, c(416L, 12L))
# })



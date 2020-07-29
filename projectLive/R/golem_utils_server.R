# Inverted versions of in, is.null and is.na
`%not_in%` <- Negate(`%in%`)

not_null <- Negate(is.null)

not_na <- Negate(is.na)

# Removes the null from a vector
drop_nulls <- function(x){
  x[!sapply(x, is.null)]
}

# If x is null, return y, otherwise return x
"%||%" <- function(x, y){
  if (is.null(x)) {
    y
  } else {
    x
  }
}
# If x is NA, return y, otherwise return x
"%|NA|%" <- function(x, y){
  if (is.na(x)) {
    y
  } else {
    x
  }
}

# typing reactiveValues is too long
rv <- shiny::reactiveValues
rvtl <- shiny::reactiveValuesToList

synapse_dates_to_year <- function(dates){
  dates %>% 
    magrittr::divide_by(., 1000) %>% 
    purrr::map(as.POSIXct, origin = "1970-01-01") %>% 
    purrr::map_dbl(lubridate::year) %>% 
    as.integer()
}

synapse_dates_to_month <- function(dates){
  dates %>% 
    magrittr::divide_by(., 1000) %>% 
    purrr::map(as.POSIXct, origin = "1970-01-01") %>% 
    purrr::map(lubridate::month, label  = TRUE, abbr = TRUE) %>% 
    unlist()
}


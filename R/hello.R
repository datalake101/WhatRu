#whatRu
# display column types

whatRu <- function(df){
  binary <- character()
  multinomial <- character()
  continuous <- character()
  ordinal <- character()
  discrete <- character()

  for (col in names(df)) {
    x <- df[[col]]
    unique_vals <- base::length(unique(x))
    all_int <- all(x %% 1 == 0)
    all_nonneg <- all(x >= 0)
    has_fraction <- any(x %% 1 != 0)
    if (unique_vals == 2) {
      binary <- c(binary, col)
    } else if (all_nonneg && !has_fraction && unique_vals >= 5 && unique_vals < 11) {
      ordinal <- c(ordinal, col)
    } else if (!has_fraction && unique_vals >= 5) {
      discrete <- c(discrete, col)
    } else if (unique_vals >= 3 && unique_vals < 11) {
      multinomial <- c(multinomial, col)
    } else {
      continuous <- c(continuous, col)
    }
  }

  list(binary = binary,
       multinomial = multinomial,
       continuous = continuous,
       ordinal = ordinal,
       discrete = discrete)
}


#' Classify columns in a data frame as binary, multinomial, continuous, ordinal, or discrete.
#'
#' @param df A data frame.
#'
#' @return A list with five elements: binary, multinomial, continuous, ordinal, and discrete.
#' Each element contains the column names that belong to that classification.
#'
#' @examples
#' whatRu(mtcars)
#'
#' @import here

whatRu <- function(df){
  binary <- character()
  multinomial <- character()
  continuous <- character()
  ordinal <- character()
  discrete <- character()

  for (col in names(df)) {
    x <- df[[col]]
    unique_vals <- base::length(unique(x))
    all_int <- all(x %% 1 == 0)
    all_nonneg <- all(x >= 0)
    has_fraction <- any(x %% 1 != 0)
    if (unique_vals == 2) {
      binary <- c(binary, col)
    } else if (all_nonneg && !has_fraction && unique_vals >= 5 && unique_vals < 11) {
      ordinal <- c(ordinal, col)
    } else if (!has_fraction && unique_vals >= 5) {
      discrete <- c(discrete, col)
    } else if (unique_vals >= 3 && unique_vals < 11) {
      multinomial <- c(multinomial, col)
    } else {
      continuous <- c(continuous, col)
    }
  }

  list(binary = binary,
       multinomial = multinomial,
       continuous = continuous,
       ordinal = ordinal,
       discrete = discrete)
}

#' Convert Transfer Values to Numeric
#'
#' Converts a character representation of transfer fees or values
#' with suffixes "M" (million) or "K" (thousand) into numeric values.
#' If the value is already numeric, it is returned as-is. Non-numeric
#' values are converted to NA.
#'
#' @param x Character or numeric vector of values (e.g., "59M", "500K", "42").
#' @return Numeric vector with values in actual numbers (e.g., 59000000, 500000, 42).
#' @examples
#' convert_value(c("10M", "500K", "100"))
#' @export
convert_value <- function(x) {
  x <- toupper(x)
  dplyr::case_when(
    grepl("M$", x) ~ readr::parse_number(x) * 1e6,
    grepl("K$", x) ~ readr::parse_number(x) * 1e3,
    suppressWarnings(!is.na(as.numeric(x))) ~ as.numeric(x),
    TRUE ~ NA_real_
  )
}



#' Format Numeric Values with M/K Suffix
#'
#' Converts numeric values into a more readable format using "M" for millions
#' and "K" for thousands.
#'
#' @param val Numeric value to format.
#' @return Character string with formatted value.
#' @examples
#' format_value(59000000)  # "59M"
#' format_value(500000)    # "500K"
#' format_value(42)        # "42"
#' @export
format_value <- function(val) {
  if (val >= 1e6) {
    paste0(round(val / 1e6, 1), "M")
  } else if (val >= 1e3) {
    paste0(round(val / 1e3, 1), "K")
  } else {
    as.character(val)
  }
}


#' Clean Player Transfer Dataset
#'
#' Cleans a dataset of player transfers by optionally reordering columns,
#' dropping unnecessary columns, and formatting player names.
#'
#' @param data A data frame containing player transfer data.
#' @param keep_order Optional character vector of column names specifying the order to keep.
#' @param drop Optional character vector of column names to remove from the dataset.
#' @param player_name_col Name of the column containing player names.
#' @return A cleaned data frame with formatted player names and reordered/dropped columns.
#' @examples
#' clean_dataset(pl_incomings, keep_order = c("Player", "Club"), drop = c("Notes"), player_name_col = "Player")
#' @export
clean_dataset <- function(data, keep_order = NULL, drop = NULL, player_name_col = NULL) {
  data <- data %>%
    # Reorder if given
    { if (!is.null(keep_order)) dplyr::select(., all_of(keep_order), everything()) else . } %>%
    # Drop if given
    { if (!is.null(drop)) dplyr::select(., -all_of(drop)) else . }
  
  data[[player_name_col]] <- data[[player_name_col]] %>%
    janitor::make_clean_names(case = "title")
  
  data[[player_name_col]] <- sub(" [a-z] .*", "", data[[player_name_col]]) %>%
    sub("^(.+?) \\1$", "\\1", .)
  
  colnames(data)[3] <- "Player"
  
  return(data)
}


#' Filter Transfers by Loan or Euro/Transfer Fee
#'
#' Filters incoming or outgoing transfers that have a Euro symbol or contain "transfer".
#'
#' @param df Data frame containing transfers with a column named `Fee`.
#' @return Filtered data frame containing only transfers with "€" or "transfer".
#' @examples
#' pl_incomings <- pl_incomings %>% filter_loans_and_transfers()
#' @export
filter_loans_and_transfers <- function(df) {
  df %>%
    filter(grepl("€", Fee, ignore.case = TRUE) | grepl("transfer", Fee, ignore.case = TRUE))
}

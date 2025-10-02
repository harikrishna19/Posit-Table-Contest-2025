#' Summarize Total Spending by Team
#'
#' Groups the dataset by Team, converts transfer fees to numeric, and calculates
#' total spending per team. Also creates a human-readable formatted version.
#'
#' @param data A data frame with at least columns `Team` and `Fee`.
#' @return A data frame with `Team`, `Total_Spent` (numeric), and `Total_Spent_Convert` (formatted string).
#' @examples
#' get_summaries(pl_incomings)
get_summaries <- function(data) {
  data <- data %>% 
    dplyr::group_by(Team) %>% 
    dplyr::mutate(NumericValue = sapply(Fee, convert_value)) %>% 
    dplyr::summarise(Total_Spent = sum(NumericValue, na.rm = TRUE),
                     Total_Spent_Convert = format_value(Total_Spent))
  return(data)
}


#' Create Player Reactable Table
#'
#' Generates a reactable table for player-level transfer data with
#' custom formatting, sticky columns, colored ROI and overpaid percentages,
#' and hover highlights.
#'
#' @param data A data frame containing player transfer details.
#' @return A reactable table object.
#' @examples
#' make_player_table(pl_incomings)
make_player_table <- function(data) {
  reactable(
    defaultColDef = colDef(align = "center"),
    theme = reactableTheme(
      borderColor = "#e0e0e0",
      headerStyle = list(
        background = "#eaeaea",
        color = "#222222",
        borderColor = "#e0e0e0"
      ),
      highlightColor = "#e6f2f5"
    ),
    data %>% select(Player, Age, Position, `Club Left`, Market.value, Fee, Age, OverpaidPct, ROI),
    bordered = TRUE, highlight = TRUE, compact = TRUE, pagination = FALSE, resizable = TRUE, striped = TRUE,
    columns = list(
      Player = colDef(minWidth = 170, sticky = "left"),
      `Club Left` = colDef(name = "Club Left", minWidth = 170),
      Market.value = colDef(name = "Market Value", minWidth = 170),
      OverpaidPct = colDef(
        name = "% Overpaid", minWidth = 170,
        style = function(value) list(color = ifelse(value <= 0, "green", "red"), fontWeight = "bold"),
        cell = function(value) paste0(value, "%")
      ),
      ROI = colDef(
        name = "ROI (%)", minWidth = 170,
        style = function(value) list(color = ifelse(value >= 0, "green", "red"), fontWeight = "bold"),
        cell = function(value) paste0(value, "%")
      )
    )
  )
}


#' Merge Incoming and Outgoing Team Data
#'
#' Merges summaries of incoming and outgoing transfers for each team,
#' orders them according to a specified team list, and adds team logos.
#'
#' @param inc_data Data frame of incoming transfer summaries.
#' @param out_data Data frame of outgoing transfer summaries.
#' @param teams Character vector of team names to define factor levels and order.
#' @param pl_inc_data Original incoming transfer data containing `team_logo`.
#' @return Combined data frame of incoming and outgoing totals per team with logos.
#' @examples
#' merge_tables(pl_inc_summaries, pl_out_summaries, teams, pl_incomings)
merge_tables <- function(inc_data, out_data, teams, pl_inc_data) {
  combined <- merge(inc_data, out_data, by = "Team")
  combined$Team <- factor(combined$Team, levels = teams)
  combined <- combined[order(combined$Team), ]
  team_logos <- unique(pl_incomings$team_logo)
  combined$team_logo <- team_logos
  return(combined)
}


#' Update Player Data with Age Categories and Position Emojis
#'
#' Adds an `Age_Category` column and maps player positions to emojis
#' for more visual tables.
#'
#' @param data A data frame containing at least `Age` and `Position`.
#' @return The same data frame with updated `Age_Category` and `Position`.
#' @examples
#' update_cols(pl_incomings)
update_cols <- function(data) {
  data <- data %>% dplyr::mutate(
    Age_Category = case_when(
      Age <= 21 ~ "U21",
      Age <= 25 ~ "22-25",
      Age <= 30 ~ "26-30",
      TRUE ~ "30+"
    ),
    Position = case_when(
      Position == "Goalkeeper" ~ "🧤 GK",
      Position %in% c("Centre-Back", "Left-Back", "Right-Back") ~ "🛡️ DEF",
      Position %in% c("Left Midfield", "Right Midfield", "Defensive Midfield", "Attacking Midfield", "Central Midfield") ~ "🎯 MID",
      Position %in% c("Centre-Forward", "Second Striker") ~ "⚽ FWD",
      Position %in% c("Right Winger", "Left Winger") ~ "⚽ Winger",
      TRUE ~ Position
    )
  )
  return(data)
}


#' Calculate Additional Columns for Player Transfers
#'
#' Cleans column names, standardizes fee formats, identifies invalid rows,
#' and calculates `OverpaidPct` and `ROI` for valid numeric transfers.
#'
#' @param data A data frame containing at least `Fee` and `Market.value`.
#' @return Data frame with `OverpaidPct` and `ROI` columns calculated.
#' @examples
#' calculated_cols(pl_incomings)
calculated_cols <- function(data) {
  data <- data %>% rename_with(~ ifelse(.x %in% c("Joined.1", "Left.1"), "Club Left", .x))
  data <- data %>%
    mutate(
      Market.value = str_replace(Market.value, "m$", "M"),
      Fee = str_replace(Fee, "m$", "M")
    ) %>%
    mutate(
      invalid_row = str_detect(Fee, regex("^(End of loan|Loan transfer|-)", ignore_case = TRUE)) |
        str_detect(Market.value, regex("^(End of loan|Loan transfer|-)", ignore_case = TRUE)),
      Fee_num = suppressWarnings(parse_number(Fee)),
      MV_num = suppressWarnings(parse_number(Market.value)),
      OverpaidPct = if_else(
        !invalid_row & !is.na(Fee_num) & !is.na(MV_num),
        round(((Fee_num - MV_num) / MV_num) * 100, 1),
        NA_real_
      ),
      ROI = if_else(
        !invalid_row & !is.na(Fee_num) & !is.na(MV_num) & Fee_num != 0,
        round(((MV_num - Fee_num) / Fee_num) * 100, 1),
        NA_real_
      )
    ) %>% select(-Fee_num, -MV_num, -invalid_row)
}

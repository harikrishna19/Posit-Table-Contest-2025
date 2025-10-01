get_summaries <- function(data) {
  data <- data %>% dplyr::group_by(Team) %>% 
    dplyr::mutate(NumericValue = sapply(Fee, convert_value)) %>% 
    dplyr::summarise(Total_Spent=sum(NumericValue,na.rm = TRUE),
                     Total_Spent_Convert=format_value(Total_Spent))
  return(data)
}



# Function to make child tables
make_player_table <- function(data) {
  reactable(
    defaultColDef = colDef(align = "center"),
    theme = reactableTheme(
      backgroundColor = "bisque",       # very light gray background
      color = "#333333",                 # dark text
      borderColor = "#e0e0e0",           # subtle borders
      headerStyle = list(
        background = "#eaeaea",          # light gray header
        color = "#222222",
        fontWeight = "bold",
        borderColor = "#e0e0e0"
      ),
      highlightColor = "#e6f2f5",        # hover highlight
    ),
    data %>% select(Player,Age,Position,Market.value,Fee,Age,OverpaidPct,ROI),
    bordered = TRUE, highlight = TRUE, compact = TRUE,pagination = F,resizable = T,striped = T,
    columns = list(
      Player = colDef(minWidth = 140),
      Market.value=colDef(name="Market Value"),
      OverpaidPct = colDef(
        name = "% Overpaid",
        style = function(value) {
          list(color = ifelse(value <= 0, "green", "red"), fontWeight = "bold")
        },
        cell = function(value) paste0(value, "%")
      ),
      
      ROI = colDef(
        name = "ROI (%)",
        style = function(value) {
          list(color = ifelse(value >= 0, "green", "red"), fontWeight = "bold")
        },
        cell = function(value) paste0(value, "%")
      )
      )
  )
}


merge_tables<-function(inc_data,out_data,teams,pl_inc_data){
  combined<-merge(inc_data,out_data,by="Team")
  combined$Team <- factor(combined$Team, levels = teams)
  combined <- combined[order(combined$Team), ]
  team_logos=unique(pl_incomings$team_logo)
  combined$team_logo<-team_logos
  return(combined)
}





update_cols <- function(data) {
  data <- data %>% dplyr::mutate(
    Age_Category = case_when(
      Age <= 21 ~ "U21",
      Age <= 25 ~ "22-25",
      Age <= 30 ~ "26-30",
      TRUE ~ "30+"
    ),
    # Add emojis for position
    Position = case_when(
      Position == "Goalkeeper" ~ "🧤 GK",
      Position %in% c("Centre-Back","Left-Back","Right-Back") ~ "🛡️ DEF",
      Position %in% c("Left Midfield","Right Midfield","Defensive Midfield","Attacking Midfield") ~ "🎯 MID",
      Position %in% c("Centre-Forward","Second Striker") ~ "⚽ FWD",
      Position %in% c("Right Winger","Left Winger") ~ "⚽ Winger",
      TRUE ~ Position  # fallback for any other positions
    )
  )
  return(data)
}


library(dplyr)
library(readr)
library(stringr)

calculated_cols <- function(data) {
  data <- data %>%
    mutate(
      # Identify invalid text rows
      invalid_row = str_detect(Fee, regex("^(End of loan|Loan transfer|-)", ignore_case = TRUE)) |
        str_detect(Market.value, regex("^(End of loan|Loan transfer|-)", ignore_case = TRUE)),
      
      # Parse numbers safely
      Fee_num = suppressWarnings(parse_number(Fee)),
      MV_num  = suppressWarnings(parse_number(Market.value)),
      
      # Calculate OverpaidPct only if numeric and not invalid row
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
    ) %>%
    select(-Fee_num, -MV_num, -invalid_row)  # remove temporary helper columns
}



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
    data %>% select(Player,Age,Position,Market.value,Fee,Age),
    bordered = TRUE, highlight = TRUE, compact = TRUE,pagination = F,resizable = T,striped = T,
    columns = list(
      Player = colDef(minWidth = 140,align = "center"),
      Fee = colDef(align = "center"))
    # rowStyle = JS(
    #   paste0(
    #     "function(rowInfo) {
    #   if (!rowInfo) return {};
    #   var colors = ", jsonlite::toJSON(team_colors), ";
    #   return { backgroundColor: colors[rowInfo.index] };
    # }"
    #   )
    # )
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
    Age = case_when(
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
      Position %in% c("Right Winger","Left Winger") ~ "⚽ FWingers",
      TRUE ~ Position  # fallback for any other positions
    )
  )
  return(data)
}

update_cols(pl_incomings)

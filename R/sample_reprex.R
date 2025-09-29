library(reactable)
library(reactablefmtr)
library(readr)
library(scales)
outer_table1<-outer_table

df <- data.frame(
  Team   = c("Arsenal", "Villa", "Spurs", "Chelsea"),
  Spend  = c(230, 120, 95, 300),
  Gain   = c(60, 150, 40, 90),
  Profit = c(-170, 30, -55, -210)  # Gain - Spend
)

outer_table1$Total_Spent_Convert.x <-parse_number(outer_table1$Total_Spent_Convert.x)
reactable(
  outer_table,
  columns = list(
    team_logo = colDef(name="",
                       cell = embed_img(width=30,height=40)
    ),
    Team = colDef(minWidth = 80,align = "center",vAlign = "center"),
    Total_Spent_Convert.x = colDef(
      cell = data_bars(
        outer_table,
        fill_color = "green",
        number_fmt = scales::label_number(suffix = "M")
      )
    ),
    Total_Spent_Convert.y = colDef(name = "Total Money From Transfers (£M)",align = "center"),
    Expenditure=colDef(
      cell = function(value, index) {
        paste0(parse_number(outer_table$Total_Spent_Convert.x[index]) - 
                 parse_number(outer_table$Total_Spent_Convert.y[index]),"M")
      })
  )
)






library(reactable)
library(reactablefmtr)
library(scales)

df <- data.frame(
  Team   = c("Arsenal", "Villa", "Spurs", "Chelsea"),
  Change = c(0.12, -0.3, 0.08, -0.15)
)

reactable(
  df,
  pagination = FALSE,
  defaultSorted = "Change",
  defaultSortOrder = "asc",
  columns = list(
    Change = colDef(
      cell = data_bars(
        df,
        fill_color = c("lightblue", "orange"),
        number_fmt = percent,
        text_position = "outside-end",   # <- completely to the right
        text_color = "black",            # ensure readable text
        bar_height = "20px"
      ),
      align = "left"
    )
  )
)



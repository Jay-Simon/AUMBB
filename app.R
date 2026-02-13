
# -----------------------------
# 1️⃣ Start with a clean environment
# -----------------------------
rm(list = ls())

# Load packages
library(data.table)
library(bslib)

# Set working directory
#setwd("/Users/nataliewebb/Desktop/Basketball-Analytics")

# -----------------------------
# 2️⃣ Define folders and list CSVs
# -----------------------------
practice_folder <- ".//Labeled Practices"
games_folder <- ".//SQ Games 25'-26'"

# Get CSV file paths
shot_files_practice <- list.files(path = practice_folder, pattern = "\\.csv$", full.names = TRUE)
shot_files_games <- list.files(path = games_folder, pattern = "\\.csv$", full.names = TRUE)

# Debug: check how many files
cat("Practice files:", length(shot_files_practice), "\n")
cat("Game files:", length(shot_files_games), "\n")

# -----------------------------
# 3️⃣ Read + combine each set separately
# -----------------------------
read_and_clean <- function(file_list) {
  dflist <- lapply(file_list, read.csv, stringsAsFactors = FALSE)
  df <- rbindlist(dflist, fill = TRUE)
  
  # Keep only relevant columns
  df <- df[, c("Result","Shooter","Shot.Clock","Shot.Contest","Shot.Location","Shot.Type")]
  
  # Remove completely blank rows
  df <- df[!apply(df == "", 1, all), ]
  
  # Valid values
  valid_Results <- c("Made 3","Made 2","Miss 3","Miss 2")
  valid_Shot.Clocks <- c("5-0","10-6","21-11","22+")
  valid_Shot.Contests <- c("Highly Contested","Lightly Contested","Wide Open","Blocked")
  valid_Shot.Locations <- c("Rim","Close Shot Left","Close Shot Middle","Close Shot Right",
                            "Left Corner 2","Left Corner 3","Left Wing 2","Left Wing 3",
                            "Right Corner 2","Right Corner 3","Right Wing 2","Right Wing 3",
                            "Top of Key 2","Top of Key 3")
  valid_Shot.Types <- c("Layup","Catch & Shoot","Pullup","Fadeaway","Hook","Step Back","Floater",
                        "Putback","Dunk","Other")
  
  # Keep only valid rows
  isRowValid <- function(one_row) {
    if(one_row$Result %in% valid_Results &
       one_row$Shot.Clock %in% valid_Shot.Clocks &
       one_row$Shot.Contest %in% valid_Shot.Contests &
       one_row$Shot.Location %in% valid_Shot.Locations &
       one_row$Shot.Type %in% valid_Shot.Types &
       !is.na(one_row$Shooter) & one_row$Shooter != "") {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
  
  valid_rows <- sapply(1:nrow(df), function(i) isRowValid(df[i, , drop = FALSE]))
  df <- df[valid_rows, ]
  
  # Add dependent variables
  df$MadeShot <- grepl("Made", df$Result)
  df$Points <- ifelse(grepl("Miss", df$Result), 0,
                      ifelse(grepl("2", df$Result), 2, 3))
  
  return(df)
}

# -----------------------------
# 4️⃣ Create separate cleaned datasets
# -----------------------------
practice_df <- read_and_clean(shot_files_practice)
game_df <- read_and_clean(shot_files_games)

# -----------------------------
# 5️⃣ Quick summary checks
# -----------------------------
cat("\nPractice data summary:\n")
cat("Rows:", nrow(practice_df), "\n")
cat("Made vs Missed:\n")
print(table(practice_df$Result, useNA="ifany"))

cat("\nGame data summary:\n")
cat("Rows:", nrow(game_df), "\n")
cat("Made vs Missed:\n")
print(table(game_df$Result, useNA="ifany"))

# Optional: save cleaned datasets
fwrite(game_df, "Game_Cleaned.csv")
fwrite(practice_df, "Practice_Cleaned.csv")
cat("\nCleaned datasets saved as 'Practice_Cleaned.csv' and 'Game_Cleaned.csv'\n")





# Creating R Shiny App

library(shiny)
library(data.table)

# ---- LOAD DATA ----
practice_df <- fread("Practice_Cleaned.csv")
game_df <- fread("Game_Cleaned.csv")

# ---- UI ----
#ui <- fluidPage(
ui <- page_fillable(
titlePanel("Shot Quality Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      width = 2,
      radioButtons(
        "dataset",
        "Select Dataset:",
        choices = c("Practice", "Game"),
        selected = "Practice"
      ),
      
      checkboxGroupInput(
        "points_filter",
        "Select Points:",
        choices = c(0, 2, 3),
        selected = c(0, 2, 3)
      )
    ),
    
    mainPanel(
      
      layout_columns(
        layout_columns(
          card(
            card_header("Shot Location Summary"),
            tableOutput("location_table")
          ),
          card(
            card_header("Shot Contest Summary"),
            tableOutput("contest_table")
          ),
          col_widths = c(12, 12)
        ),
        card(
          card_header("Shot Location vs. Shot Contest"),
          tableOutput("location_contest_crosstab")
        ),
        col_widths = c(6, 6)
        )
     )
      
      
      #h3("Shot Location Summary"),
      #tableOutput("location_table"),
      
      #br(),
      
      #h3("Shot Contest Summary"),
      #tableOutput("contest_table")
    )
)

# ---- SERVER ----
server <- function(input, output) {
  
  # Choose dataset
  selected_data <- reactive({
    if (input$dataset == "Practice") {
      practice_df
    } else {
      game_df
    }
  })
  
  # Apply points filter
  filtered_data <- reactive({
    selected_data()[Points %in% input$points_filter]
  })
  
  # ---- LOCATION TABLE ----
  output$location_table <- renderTable({
    filtered_data()[,
                    .(
                      Shots = .N,
                      Made = sum(MadeShot),
                      Missed = .N - sum(MadeShot),
                      `Made %` = round(mean(MadeShot) * 100, 1)
                    ),
                    by = Shot.Location
    ][order(-`Made %`)]
  })
  
  # ---- CONTEST TABLE ----
  output$contest_table <- renderTable({
    filtered_data()[,
                    .(
                      Shots = .N,
                      Made = sum(MadeShot),
                      Missed = .N - sum(MadeShot),
                      `Made %` = round(mean(MadeShot) * 100, 1)
                    ),
                    by = Shot.Contest
    ][order(-`Made %`)]
  })
  
  # ---- CROSSTAB ----
  output$location_contest_crosstab <- renderTable({
    req(filtered_data)
    tab <- table(filtered_data()$Shot.Location, filtered_data()$Shot.Contest)
    as.data.frame.matrix(tab)
  }, rownames = TRUE)
  
}

# ---- RUN APP ----
shinyApp(ui = ui, server = server)

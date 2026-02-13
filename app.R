
# -----------------------------
# 1️⃣ Start with a clean environment
# -----------------------------
rm(list = ls())

# Load packages
library(data.table)
library(bslib)
library(shinylive)
library(httpuv)
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
# 3️⃣ Conference Teams
# -----------------------------
conference_teams <- c("Boston", "Army", "Navy", "Loyola", "Colgate",
                      "Lehigh", "Lafayette", "Holy Cross", "Bucknell")

# -----------------------------
# 4️⃣ Read & Clean Function
# -----------------------------
read_and_clean <- function(file_list, is_game = TRUE) {
  df <- rbindlist(lapply(file_list, function(file) {
    d <- read.csv(file, stringsAsFactors = FALSE)
    fname <- tools::file_path_sans_ext(basename(file))
    
    # Detect conference from filename
    is_conf <- sapply(conference_teams, function(team) grepl(team, fname, ignore.case = TRUE))
    d$Opponent <- fname
    d$Conference <- any(is_conf)
    
    # If columns are missing (practice files), create them
    needed_cols <- c("Result","Shooter","Foul","Shot.Clock","Shot.Contest","Shot.Location","Shot.Type")
    for(col in needed_cols) {
      if(!col %in% names(d)) d[[col]] <- NA
    }
    
    return(d)
  }), fill = TRUE)
  
  # Filter valid rows
  df <- df[Result %in% c("Made 2","Made 3","Miss 2","Miss 3") | is.na(Result)]
  df <- df[Shot.Clock %in% c("5-0","10-6","21-11","22+") | is.na(Shot.Clock)]
  df <- df[Shot.Type != "" & Shot.Location != "" & Shot.Contest != "" | is.na(Shot.Type)]
  
  # Trim whitespace
  df[, Shot.Contest := ifelse(!is.na(Shot.Contest), trimws(Shot.Contest), NA)]
  df[, Shot.Location := ifelse(!is.na(Shot.Location), trimws(Shot.Location), NA)]
  df[, Shot.Type := ifelse(!is.na(Shot.Type), trimws(Shot.Type), NA)]
  
  # Remove impossible combos
  df <- df[is.na(Shot.Contest) | !grepl(",", Shot.Contest)]
  
  # Dependent vars
  df$MadeShot  <- grepl("Made", df$Result)
  df$ShotValue <- ifelse(grepl("2", df$Result), 2,
                         ifelse(grepl("3", df$Result), 3, NA))
  
  # For practices, hide Foul column
  if(!is_game) df$Foul <- NULL
  
  return(df)
}

# -----------------------------
# 5️⃣ Player Groups
# -----------------------------
scholarship_players <- c("#12 Geoff Sprouse", "#7 Wyatt Nausadis", "#33 Matt Mayock",
                         "#23 Greg Jones", "#32 Julen Iturbe", "#35 Chris Eagan",
                         "#4 Kade Sebastian", "#2 Madden Collins", "#3 Dean Hogans")

big_players <- c("#32 Julen Iturbe", "#35 Chris Eagan", "#13 Carson McDonald",
                 "#16 Luke Brown", "#21 Noah Jones")

# -----------------------------
# 6️⃣ Add Player Columns
# -----------------------------
add_player_columns <- function(df) {
  df[, Scholarship := Shooter %in% scholarship_players]
  df[, Position := ifelse(Shooter %in% big_players, "Big", "Guard")]
  return(df)
}

add_shot_zone <- function(df) {
  df[, ShotZone := fcase(
    Shot.Location %in% c("Right Corner 3","Left Corner 3"), "Corner 3's",
    Shot.Location %in% c("Left Wing 3","Top of Key 3","Right Wing 3"), "Above Break 3's",
    Shot.Location %in% c("Rim","Close Shot Middle"), "In the Paint",
    Shot.Location %in% c("Left Corner 2","Left Wing 2","Top of Key 2","Right Wing 2","Right Corner 2",
                         "Close Shot Left","Close Shot Right"), "Mid Range 2's",
    default = NA_character_
  )]
  return(df)
}

# -----------------------------
# 7️⃣ Load datasets
# -----------------------------
practice_df <- read_and_clean(shot_files_practice, is_game = FALSE)
game_df     <- read_and_clean(shot_files_games, is_game = TRUE)

practice_df <- add_player_columns(practice_df)
game_df     <- add_player_columns(game_df)

practice_df <- add_shot_zone(practice_df)
game_df     <- add_shot_zone(game_df)

all_players <- sort(unique(c(practice_df$Shooter, game_df$Shooter)))

# -----------------------------
# 8️⃣ Shiny UI
# -----------------------------
ui <- page_sidebar(
  title = "Shot Quality Explorer",
  theme = bs_theme(bootswatch = "flatly", primary = "#0066CC"),
  
  sidebar = sidebar(
    width = 300,
    radioButtons("dataset", "Select Dataset:", choices = c("Practice", "Game"), selected = "Game"),
    checkboxGroupInput("points_filter", "Shot Value:", choices = c(2,3), selected = c(2,3), inline=TRUE),
    selectInput("player_filter","Player(s):", choices = all_players, multiple=TRUE, selectize=TRUE),
    checkboxGroupInput("scholarship_filter","Scholarship Status:", choices=c("Scholarship","Non-Scholarship"),
                       selected=c("Scholarship","Non-Scholarship")),
    checkboxGroupInput("position_filter","Position:", choices=c("Big","Guard"), selected=c("Big","Guard"), inline=TRUE),
    checkboxGroupInput("conference_filter","Conference Games:", choices=c("Conference","Non-Conference"),
                       selected=c("Conference","Non-Conference")),
    hr(),
    p(class="text-muted small","Minimum attempt thresholds are lowered when specific players are selected.")
  ),
  
  layout_columns(
    col_widths=c(4,4,4),
    value_box(title="Total Attempts", value=textOutput("total_attempts"), theme="primary"),
    value_box(title="Overall FG%", value=textOutput("overall_fg"), theme="success"),
    value_box(title="Filtered Shots", value=textOutput("filtered_count"), theme="info")
  ),
  
  navset_card_pill(
    title="Shot Analysis",
    nav_panel("Overview",
              layout_columns(col_widths=c(6,6),
                             card(card_header("FG% by Shot Zone"), card_body(tableOutput("location_table"))),
                             card(card_header("FG% by Contest"), card_body(tableOutput("contest_table")))
              )),
    nav_panel("Contest × Location", card(full_screen=TRUE, card_header("Field Goal % by Contest × Shot Zone"),
                                         card_body(tableOutput("contest_location_table")))),
    nav_panel("Contest × Shot Clock", card(full_screen=TRUE, card_header("Field Goal % by Contest × Shot Clock"),
                                           card_body(tableOutput("contest_clock_table")))),
    nav_panel("Contest × Shot Type", card(full_screen=TRUE, card_header("Field Goal % by Contest × Shot Type"),
                                          card_body(tableOutput("contest_type_table")))),
    nav_panel("Location × Shot Clock", card(full_screen=TRUE, card_header("Field Goal % by Shot Zone × Shot Clock"),
                                            card_body(tableOutput("location_clock_table")))),
    nav_panel("Location × Shot Type", card(full_screen=TRUE, card_header("Field Goal % by Shot Zone × Shot Type"),
                                           card_body(tableOutput("location_type_table")))),
    nav_panel("Shot Clock × Shot Type", card(full_screen=TRUE, card_header("Field Goal % by Shot Clock × Shot Type"),
                                             card_body(tableOutput("clock_type_table"))))
  )
)

# -----------------------------
# 9️⃣ Shiny Server
# -----------------------------
server <- function(input, output, session) {
  
  selected_data <- reactive({
    if(input$dataset=="Practice") practice_df else game_df
  })
  
  player_mode_active <- reactive({ !is.null(input$player_filter) && length(input$player_filter)>0 })
  
  filtered_data <- reactive({
    df <- selected_data()
    req(nrow(df) > 0)
    
    # Filters
    if(!is.null(input$points_filter) && length(input$points_filter)>0) df <- df[ShotValue %in% as.numeric(input$points_filter)]
    if(player_mode_active()) df <- df[Shooter %in% input$player_filter]
    if(!is.null(input$scholarship_filter) && length(input$scholarship_filter)>0)
      df <- df[(Scholarship & "Scholarship" %in% input$scholarship_filter) |
                 (!Scholarship & "Non-Scholarship" %in% input$scholarship_filter)]
    if(!is.null(input$position_filter) && length(input$position_filter)>0) df <- df[Position %in% input$position_filter]
    if(!is.null(input$conference_filter) && length(input$conference_filter)>0)
      df <- df[(Conference & "Conference" %in% input$conference_filter) |
                 (!Conference & "Non-Conference" %in% input$conference_filter)]
    
    # Remove multi-shot types
    df <- df[is.na(Shot.Type) | !grepl(",", Shot.Type)]
    
    # Foul filter for games only
    if("Foul" %in% names(df)){
      df <- df[is.na(Foul) | Foul=="No Foul" | (Foul=="Foul" & (Result=="Made 2" | Result=="Made 3"))]
    }
    
    return(df)
  })
  
  # -----------------------------
  # Value Boxes
  # -----------------------------
  output$total_attempts <- renderText({ format(nrow(selected_data()), big.mark=",") })
  output$overall_fg <- renderText({ df <- filtered_data(); req(nrow(df)>0)
  paste0(round(sum(df$MadeShot)/nrow(df)*100,1), "%") })
  output$filtered_count <- renderText({ format(nrow(filtered_data()), big.mark=",") })
  
  # -----------------------------
  # Summary Tables
  # -----------------------------
  make_summary <- function(var){
    df <- filtered_data()
    if(!(var %in% names(df))) return(data.frame(Category="No data", `FG% (Attempts)`="—"))
    if(nrow(df)==0) return(data.frame(Category="No data", `FG% (Attempts)`="—"))
    attempts <- df[, .N, by=var]; makes <- df[MadeShot==TRUE, .N, by=var]
    merged <- merge(attempts,makes,by=var,all.x=TRUE); merged[is.na(N.y), N.y:=0]
    merged[, FG:=round((N.y/N.x)*100,1)]; merged[, Display:=paste0(FG,"% (",N.x,")")]
    setnames(merged, c(var,"Attempts","Makes","FG%","Display"))
    merged[, .(Category=get(var), `FG% (Attempts)`=Display)]
  }
  
  output$location_table <- renderTable(make_summary("ShotZone"))
  output$contest_table  <- renderTable(make_summary("Shot.Contest"))
  
  # -----------------------------
  # Crosstabs
  # -----------------------------
  make_crosstab <- function(var1,var2,team_threshold=10){
    df <- filtered_data()
    if(!(var1 %in% names(df)) | !(var2 %in% names(df))) return(data.frame(Note="No data matches filters"))
    if(nrow(df)==0) return(data.frame(Note="No data matches filters"))
    
    attempts <- df[, .N, by=c(var1,var2)]; setnames(attempts,"N","Attempts")
    makes <- df[MadeShot==TRUE, .N, by=c(var1,var2)]; setnames(makes,"N","Makes")
    merged <- merge(attempts,makes,by=c(var1,var2),all.x=TRUE); merged[is.na(Makes), Makes:=0]
    
    min_attempts <- if(player_mode_active()) 1 else team_threshold
    merged[, FG_Percent:=ifelse(Attempts>=min_attempts, round(Makes/Attempts*100,1), NA_real_)]
    merged[, Display:=ifelse(is.na(FG_Percent), paste0("— (",Attempts,")"), paste0(FG_Percent,"% (",Attempts,")"))]
    
    fg_table <- dcast(merged, as.formula(paste(var1,"~",var2)), value.var="Display")
    
    # Row totals
    row_totals <- merged[, .(RowTotal=sum(Attempts)), by=var1]
    fg_table <- merge(fg_table,row_totals,by=var1,all.x=TRUE)
    fg_table[, RowTotal:=paste0("(",RowTotal,")")]
    
    # Column totals
    col_totals <- merged[, .(Attempts=sum(Attempts)), by=var2]
    total_row <- as.list(rep("", ncol(fg_table))); names(total_row) <- names(fg_table)
    total_row[[1]] <- "TOTAL"
    for(col in col_totals[[var2]]){ if(col %in% names(total_row)) total_row[[col]]<-paste0("(",col_totals[get(var2)==col,Attempts],")")}
    total_row[["RowTotal"]]<-paste0("(",sum(col_totals$Attempts),")")
    
    fg_table <- rbind(fg_table,total_row,fill=TRUE)
    as.data.frame(fg_table)
  }
  
  output$contest_location_table <- renderTable(make_crosstab("Shot.Contest","ShotZone"))
  output$contest_clock_table    <- renderTable(make_crosstab("Shot.Contest","Shot.Clock"))
  output$contest_type_table     <- renderTable(make_crosstab("Shot.Contest","Shot.Type"))
  output$location_clock_table   <- renderTable(make_crosstab("ShotZone","Shot.Clock"))
  output$location_type_table    <- renderTable(make_crosstab("ShotZone","Shot.Type"))
  output$clock_type_table       <- renderTable(make_crosstab("Shot.Clock","Shot.Type"))
}

# ---- RUN APP ----
shinyApp(ui=ui, server=server)

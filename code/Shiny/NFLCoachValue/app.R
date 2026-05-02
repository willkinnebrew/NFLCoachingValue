library(shiny)
library(tidyverse)
library(nflreadr)
library(gt)
library(purrr)


scores <- read.csv("data/season_scores.csv")

best_coaches_raw <- read.csv("data/total_coach_scores.csv")


teams <- nflreadr::load_teams() %>% 
  select(team = team_abbr, team_logo_espn, team_color) %>% 
  mutate(team = case_when(
    team == "OAK" ~ "LV",
    team == "SD" ~ "LAC", 
    team == "STL" ~ "LA",
    team == "WAS" ~ "WSH",
    .default = team
  )) %>% 
  mutate(team = ifelse(team == "LA", "LAR", team))

# ---------------------------
# UI
# ---------------------------
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        font-size: 12px;
      }

      .form-group {
        margin-bottom: 6px;
      }

      .selectize-input {
        font-size: 12px;
        min-height: 28px;
      }

      .irs {
        transform: scale(1);
        transform-origin: left;
      }
    "))
  ),
  
  titlePanel("NFL Coaching Evaluator"),
  
  sidebarLayout(
    sidebarPanel(
      
      sliderInput("season_range",
                  "Season Range",
                  min = 2017,
                  max = max(scores$season),
                  value = c(2017, 2025),
                  step = 1,
                  sep = ""),
      
      radioButtons(
        "rank_direction",
        "Rank Direction",
        choices = c("Top" = "top", "Bottom" = "bottom"),
        selected = "top",
        inline = TRUE
      ),
      
      numericInput("top_n", "Top N", value = 10, min = 1, max = 50),
      
      selectInput("view_type",
                  "View",
                  choices = c("Best Coaches (Cumulative)", 
                              "Best Seasons")),
      
      hr(),
      fluidRow(
        
        column(
          width = 2,
          h4("Weights")
        ),
        
        column(
          width = 3,
          actionButton("reset_weights", "Reset Weights")
        ),
        
        column(
          width = 3,
          actionButton("zero_weights", "Set Weights to 0")
        ),
        
        column(
          width = 2,
          downloadButton("download", "Download CSV"))
      ),
      sliderInput("w_player_dev", "Overall Player Dev", 0, 2, 1, step = 0.1),
      sliderInput("w_off_dev", "Offensive Player Dev", 0, 2, 1, step = 0.1),
      sliderInput("w_def_dev", "Defensive Player Dev", 0, 2, 1, step = 0.1),
      sliderInput("w_win", "Win %", 0, 2, 1, step = 0.1),
      sliderInput("w_playoff", "Playoffs", 0, 2, 1, step = 0.1),
      sliderInput("w_close", "Close Games", 0, 2, 1, step = 0.1),
      sliderInput("w_aggressive", "Aggressiveness", 0, 2, 1, step = 0.1),
      sliderInput("w_twomin", "2-Minute", 0, 2, 1, step = 0.1),
      sliderInput("w_division", "Division", 0, 2, 1, step = 0.1),
      
      hr()
    ),
    
    mainPanel(
      gt_output("table")
    )
  )
)

# ---------------------------
# SERVER
# ---------------------------
server <- function(input, output, session) {
  
  observeEvent(input$reset_weights, {
    
    updateSliderInput(session, "w_player_dev", value = 1)
    updateSliderInput(session, "w_off_dev", value = 1)
    updateSliderInput(session, "w_def_dev", value = 1)
    updateSliderInput(session, "w_win", value = 1)
    updateSliderInput(session, "w_playoff", value = 1)
    updateSliderInput(session, "w_close", value = 1)
    updateSliderInput(session, "w_aggressive", value = 1)
    updateSliderInput(session, "w_twomin", value = 1)
    updateSliderInput(session, "w_division", value = 1)
    
  })
  
  observeEvent(input$zero_weights, {
    
    updateSliderInput(session, "w_player_dev", value = 0)
    updateSliderInput(session, "w_off_dev", value = 0)
    updateSliderInput(session, "w_def_dev", value = 0)
    updateSliderInput(session, "w_win", value = 0)
    updateSliderInput(session, "w_playoff", value = 0)
    updateSliderInput(session, "w_close", value = 0)
    updateSliderInput(session, "w_aggressive", value = 0)
    updateSliderInput(session, "w_twomin", value = 0)
    updateSliderInput(session, "w_division", value = 0)
    
  })
  
  # recalculate weighted score
  weighted_scores <- reactive({
    
    df <- scores %>%
      filter(
        season >= input$season_range[1],
        season <= input$season_range[2]
      ) %>%
      mutate(
        score_raw =
          input$w_player_dev * player_dev_percentile +
          input$w_off_dev * offense_dev_percentile +
          input$w_def_dev * defense_dev_percentile +
          input$w_win * win_percentile +
          input$w_playoff * playoff_percentile +
          input$w_close * close_win_percentile +
          input$w_aggressive * aggressiveness_percentile +
          input$w_twomin * two_minute_percentile +
          input$w_division * division_win_percentile
      )
    
    total_weight <- 
      input$w_player_dev +
      input$w_off_dev +
      input$w_def_dev +
      input$w_win +
      input$w_playoff +
      input$w_close +
      input$w_aggressive +
      input$w_twomin +
      input$w_division
    
    df %>%
      mutate(score = (score_raw / total_weight) * 10)
  })
  
  coach_metadata <- reactive({
    
    df <- weighted_scores() %>%
      pivot_longer(
        cols = c(coach, offense, defense),
        names_to = "role",
        values_to = "coach_name"
      ) %>%
      separate_rows(coach_name, sep = ",") %>%
      mutate(coach_name = str_trim(coach_name)) %>%
      filter(!is.na(coach_name), coach_name != "")
    
    latest_team <- df %>%
      group_by(coach_name) %>%
      slice_max(season, with_ties = FALSE) %>%
      ungroup() %>%
      select(coach_name, team)
    
    logos <- df %>%
      distinct(coach_name, team) %>%
      left_join(teams, by = "team") %>%
      group_by(coach_name) %>%
      summarise(
        logos = paste0(team_logo_espn, collapse = " "),
        .groups = "drop"
      )
    
    list(latest_team = latest_team, logos = logos)
  })
  
  # ---------------------------
  # BEST COACHES VIEW
  # ---------------------------
  best_coaches <- reactive({
    
    df <- weighted_scores() %>%
      pivot_longer(
        cols = c(coach, offense, defense),
        names_to = "role",
        values_to = "coach_name"
      ) %>%
      separate_rows(coach_name, sep = ",") %>%
      mutate(coach_name = str_trim(coach_name)) %>%
      filter(!is.na(coach_name), coach_name != "") %>%
      
      mutate(score_weighted = case_when(
        role == "coach" ~ score,
        TRUE ~ score * 0.5
      )) %>%
      
      group_by(coach_name) %>%
      summarise(
        seasons = n_distinct(season),
        total_score = sum(score_weighted, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(total_score))
    
    if (input$rank_direction == "top") {
      df %>% slice_head(n = input$top_n)
    } else {
      df %>% slice_tail(n = input$top_n)
    }
  })
  
  # ---------------------------
  # BEST SEASONS VIEW
  # ---------------------------
  best_seasons <- reactive({
    
    df <- weighted_scores() %>%
      group_by(season, team, coach) %>%
      summarise(score = mean(score), .groups = "drop") %>%
      arrange(desc(score))
    
    if (input$rank_direction == "top") {
      df <- df %>% slice_head(n = input$top_n)
    } else {
      df <- df %>% slice_tail(n = input$top_n)
    }
    
    df
  })
  
  # ---------------------------
  # TABLE RENDER
  # ---------------------------
  output$table <- render_gt({
    
    if (input$view_type == "Best Coaches (Cumulative)") {
      
      meta <- coach_metadata()
      
      df <- best_coaches() %>%
        left_join(meta$latest_team, by = "coach_name") %>%
        rename(latest_team = team) %>%
        left_join(teams %>% select(team, team_color), 
                  by = c("latest_team" = "team")) %>%
        left_join(meta$logos, by = "coach_name") %>%
        arrange(desc(total_score))
      
      gt_tbl <- df %>%
        gt() %>%
        
        text_transform(
          locations = cells_body(columns = logos),
          fn = function(x) {
            map(x, ~ {
              urls <- strsplit(.x, " ")[[1]]
              paste0(
                web_image(url = urls, height = 25),
                collapse = " "
              )
            })
          }
        ) %>%
        
        cols_label(
          coach_name = "Coach",
          seasons = "Seasons",
          total_score = "Score",
          logos = "Teams"
        )
      
      for (i in seq_len(nrow(df))) {
        gt_tbl <- gt_tbl %>%
          tab_style(
            style = cell_text(color = df$team_color[i]),
            locations = cells_body(
              rows = i,
              columns = c(coach_name, total_score)
            )
          )
      }
      
      gt_tbl %>%
        cols_align(align = "center", logos) %>%
        fmt_number(columns = total_score, decimals = 2) %>%
        tab_style(
          style = cell_text(weight = "bold"),
          locations = cells_body(columns = coach_name)
        ) %>%
        cols_hide(columns = c(team_color, latest_team))
      
    } else {
      
      df <- best_seasons() %>%
        left_join(teams, by = "team")
      
      gt_tbl <- df %>%
        gt() %>%
        cols_label(
          season = "Season",
          team = "Team",
          coach = "Coach",
          score = "Score"
        )
      
      for (i in seq_len(nrow(df))) {
        gt_tbl <- gt_tbl %>%
          tab_style(
            style = cell_text(color = df$team_color[i]),
            locations = cells_body(
              rows = i,
              columns = c(coach, score)
            )
          )
      }
      
      gt_tbl %>%
        fmt_number(columns = score, decimals = 2) %>%
        tab_style(
          style = cell_text(weight = "bold"),
          locations = cells_body(columns = coach)
        ) %>%
        cols_hide(columns = c(team_color, team_logo_espn))
    }
    
    
  })
  
  # ---------------------------
  # DOWNLOAD
  # ---------------------------
  output$download <- downloadHandler(
    filename = function() {
      paste0("coach_results_", Sys.Date(), ".csv")
    },
    content = function(file) {
      if (input$view_type == "Best Coaches (Cumulative)") {
        write.csv(best_coaches(), file, row.names = FALSE)
      } else {
        write.csv(best_seasons(), file, row.names = FALSE)
      }
    }
  )
}

shinyApp(ui, server)
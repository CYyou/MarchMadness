# Variables that can be put on the x and y axes
axis_vars <- c(
  "delta of seed" = "seed",
  "delta of blockpct" = "blockpct",
  "delta of team season wins" = "pt_team_season_wins",
  "delta of oppf3grate" = "oppf3grate",
  "delta of fg2pct" = "fg2pct",
  "delta of fg3pct" = "fg3pct",
  "delta of adjoe" = "adjoe",
  "delta of adjde" = "adjde",
  "delta of dist" = "dist",
  "delta of manhattan_dist" = "manhattan_dist",
  "delta of adjem" = "adjem"
  
)

library(shiny)
library(ggvis)
library(dplyr)
library(shinythemes)

# For dropdown menu
actionLink <- function(inputId, ...) {
  tags$a(href='javascript:void',
         id=inputId,
         class='action-button',
         ...)
}

# user interface
ui <- fluidPage(
  shinythemes::themeSelector(),
  titlePanel("Game explorer"),
  fluidRow(
    column(2,
           wellPanel(
             h4("Filter"),
             # Filter Navigators
             sliderInput("Team1_freq", "Minimum number of TEAM1 appeared in 2002-2016 NCAA Tourney",
                         1, 28, 3, step = 1),
             sliderInput("season", "Game Season", 2002, 2016, value = c(2002, 2016)),
             sliderInput("delta_Seed", "Maximum number of delta seed",
                         -15, 15, c(-15,15), step = 1),
             selectInput("Result", "Game Result",
                         choices = c("EITHER", "TEAM1 WON", "TEAM2 WON")),
             sliderInput("numot", "Number of overtimes",
                         0, 2, c(0, 2), step = 1),
             selectInput("Region", "The region of TEAM1",
                         c("All", "MIDWEST", "WEST", "EAST", "SOUTH")
             ),
             textInput("coach", "Coach name contains (e.g., Gottfried)"),
             textInput("teamname", "Team1's teamname contains (e.g. Atlantic)")
           ),
           wellPanel(
             selectInput("xvar", "X-axis variable", axis_vars, selected = "seed"),
             selectInput("yvar", "Y-axis variable", axis_vars, selected = "blockpct"),
             tags$small(paste0(
               "Note: Delta means the difference between TEAM1 and TEAM2 on a specific attribute"
             ))
           )
    ),
    # scatter plot
    column(6,
           ggvisOutput("plot1"),
           wellPanel(
             span("Number of games selected:",
                  #textOutput("n_games")
                  textOutput("n_games")
             )
           )
    ),
    # histograms for x and y variables
    column(4,
           ggvisOutput("plot2"),
           ggvisOutput("plot3"),
           sliderInput("bin", "Bin Width",
                       1, 5, 3, step = 1)
    )
  )
)

# load the dataset
all_games <- read.csv("NCAA_Tourney_2002-2016.csv", header = T, sep = ",")

# shiny server
server <- function(input, output, session) {
  
  # Filter the games, returning a data frame
  games <- reactive({
    # Due to dplyr issue #318, we need temp variables for input values
    freq <- input$Team1_freq
    if(input$Result=="EITHER"){
      minresult <- 0
      maxresult <- 1
    }else{
      if(input$Result=="TEAM1 WON"){
        minresult <- 1
        maxresult <- 1
      }else{
        minresult <- 0
        maxresult <- 0
      }
    }
    minseason <- input$season[1]
    maxseason <- input$season[2]
    minnumot <- input$numot[1]
    maxnumot <- input$numot[2]
    minseed <- input$delta_Seed[1]
    maxseed <- input$delta_Seed[2]
    
    # Apply filters
    m <- all_games %>%
      filter(
        team1_freq >= freq,
        result >= minresult,
        result <= maxresult,
        Season >= minseason,
        Season <= maxseason,
        Numot >= minnumot,
        Numot <= maxnumot,
        seed <= maxseed,
        seed >= minseed
      ) %>%
      arrange(result)
    
    if (input$Region != "All") {
      Region <- input$Region
      m <- m %>% filter(grepl(Region, region))
    }
    # Optional: filter by director
    if (!is.null(input$coach) && input$coach != "") {
      coach <- input$coach
      m <- m %>% filter(grepl(coach, team1_coach_name))
    }
    # Optional: filter by cast member
    if (!is.null(input$teamname) && input$teamname != "") {
      teamname <- input$teamname
      m <- m %>% filter(grepl(teamname, team1_teamname))
    }
    
    
    m <- as.data.frame(m)
    
    # Add column which says whether the team 1 won the game
    m$had_win <- character(nrow(m))
    m$had_win[m$result == 0] <- "No"
    m$had_win[m$result == 1] <- "Yes"
    m
  })
  
  # Function for generating tooltip text
  game_tooltip <- function(x) {
    if (is.null(x)) return(NULL)
    if (is.null(x$game_id)) return(NULL)
    
    # Pick out the game with this ID
    all_games <- isolate(games())
    game <- all_games[all_games$game_id == x$game_id, ]
    
    paste0("<b>", game$team1_teamname, "-", game$team2_teamname,"</b><br>",
           "Season: ", game$Season, "<br>",
           "Host: ", game$host_site, "<br>",
           "Final score: ", game$team1_score, "-", game$team2_score
    )
  }
  
  # A reactive expression with the ggvis plot
  vis <- reactive({
    # Lables for axes
    xvar_name <- names(axis_vars)[axis_vars == input$xvar]
    yvar_name <- names(axis_vars)[axis_vars == input$yvar]
    
    # Since the inputs are strings, we need to make some changes to make it work for ggvis
    xvar <- prop("x", as.symbol(input$xvar))
    yvar <- prop("y", as.symbol(input$yvar))
    
    games %>%
      ggvis(x = xvar, y = yvar) %>%
      layer_points(size := 50, size.hover := 200,
                   fillOpacity := 0.2, fillOpacity.hover := 0.5,
                   stroke = ~had_win, key := ~game_id) %>%
      add_tooltip(game_tooltip, "hover") %>%
      add_axis("x", title = xvar_name,title_offset = 50,properties = axis_props(title=list(fontSize=18),
                                                                                labels = list(fontSize = 14))) %>%
      add_axis("y", title = yvar_name,title_offset = 50,properties = axis_props(title=list(fontSize=18),
                                                                                labels = list(fontSize = 14))) %>%
      add_legend("stroke", title = "Which Team Won", values = c("Team1", "Team2"),properties = legend_props(title=list(fontSize=15),labels=list(fontSize=14))) %>%
      scale_nominal("stroke", domain = c("Yes", "No"),
                    range = c("darkorange", "dodgerblue")) %>%
      set_options(width = 900, height = 720)
  })
  
  vis1 <- reactive({
    # Lables for axes
    xvar_name <- names(axis_vars)[axis_vars == input$xvar]
    
    # Since the inputs are strings, we need to make some changes to make it work for ggvis
    xvar <- prop("x", as.symbol(input$xvar))
    
    bin_width <- input$bin
    
    games %>%
      ggvis(xvar) %>%
      layer_histograms(width = bin_width,
                       center = 35,
                       fill := "pink",fillOpacity := 0.6) %>%
      add_axis("x", title = xvar_name,title_offset = 50,properties = axis_props(title=list(fontSize=16),
                                                                                labels = list(fontSize = 12)))%>%
      add_axis("y", title = "Bin Count",title_offset = 50,properties = axis_props(title=list(fontSize=16),
                                                                                  labels = list(fontSize = 12))) %>%
      set_options(width = 500, height = 360)
  })
  
  vis2 <- reactive({
    yvar_name <- names(axis_vars)[axis_vars == input$yvar]
    bin_width <- input$bin
    
    # Since the inputs are strings, we need to make some changes to make it work for ggvis
    yvar <- prop("x", as.symbol(input$yvar))
    
    games %>%
      ggvis(yvar) %>%
      layer_histograms(width = bin_width,
                       center = 35,
                       fill := "red",fillOpacity := 0.6) %>%
      add_axis("x", title = yvar_name,title_offset = 50,properties = axis_props(title=list(fontSize=16),
                                                                                labels = list(fontSize = 12)))%>%
      add_axis("y", title = "Bin Count", title_offset = 50,properties = axis_props(title=list(fontSize=16),
                                                                                   labels = list(fontSize = 12))) %>%
      set_options(width = 500, height = 360)
  })
  
  
  
  vis %>% bind_shiny("plot1")
  vis1 %>% bind_shiny("plot2")
  vis2 %>% bind_shiny("plot3")
  
  output$n_games <- renderText({ nrow(games()) })
}

shinyApp(ui = ui, server = server)
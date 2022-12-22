library(shiny)
library(tidyverse)

xgb_model_ff <- readRDS("models/ff_model.rds")
xgb_model_si <- readRDS("models/si_model.rds")
xgb_model_fc <- readRDS("models/fc_model.rds")
xgb_model_sl <- readRDS("models/sl_model.rds")
xgb_model_cu <- readRDS("models/cu_model.rds")
xgb_model_ch <- readRDS("models/ch_model.rds")
averages <- read.csv("model_stats.csv")

# Define UI ---------------------------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Stuff+ Calculator"),
  
  fluidRow(
    
    column(4,
           radioButtons("throws", h3("Pitcher Handedness"),
                        choices = list("Right-Handed" = "RHP",
                                       "Left-Handed" = "LHP"),
                        selected = "RHP")),
    column(4,
           selectInput("pitch_type", h3("Pitch Type"), 
                       choices = list("Four-Seam Fastball" = "FF",
                                      "Sinker / Two-Seam Fastball" = "SI",
                                      "Cutter" = "FC",
                                      "Slider" = "SL",
                                      "Curveball / Knuckle Curve" = "CU",
                                      "Changeup / Splitter" = "CH"), selected = "FF")),
    column(4, 
           numericInput("velo", 
                        h3("Pitch Velocity (MPH)"), 
                        value = 93.5))
    
  ),
  
  fluidRow(
    
    column(4, 
           numericInput("vb", 
                        h3("Vertical Break (feet)"), 
                        value = -0.6)),
    column(4, 
           numericInput("hb", 
                        h3("Horizontal Break (feet)"), 
                        value = 1.34)),
    column(4, 
           numericInput("extension", 
                        h3("Release Extension (feet)"), 
                        value = 6.36)),
  ),
  
  fluidRow(
    
    column(4, br(), submitButton("Submit"))
    
  ),
  
  mainPanel(br(), br(), 
            h1(textOutput("selected_var"))
  )
  
  # fluidRow(
  #   
  #   column(3,
  #          fileInput("file", h3("File input"))),
  #   
  #   column(3, 
  #          h3("Help text"),
  #          helpText("Note: help text isn't a true widget,", 
  #                   "but it provides an easy way to add text to",
  #                   "accompany other widgets.")),
  # )
  
)

# Define server logic -----------------------------------------------------------------------------
server <- function(input, output) {
  
  scaling <- reactiveVal(averages)
  
  throws <- reactive(input$throws)
  vb <- reactive(if (throws() == "LHP") -1 * input$vb else input$vb)
  
  df <- reactive({
    
    pitch_type <- input$pitch_type

    data <- as.matrix(data.frame(release_speed = input$velo,
                                 pfx_x = vb(), pfx_z = input$hb,
                                 release_extension = input$extension))
    
    xgb_model <- eval(parse(text = paste0("xgb_model_", tolower(pitch_type))))
    
    stuff <- predict(xgb_model, newdata = data)
    
    avg_stuff <- scaling() %>%
      filter(pitch_type_condensed == pitch_type) %>%
      select(avg_stuff)
    
    sd_stuff <- scaling() %>%
      filter(pitch_type_condensed == pitch_type) %>%
      select(sd_stuff)
    
    stuff_plus <- round((((stuff - avg_stuff) / sd_stuff) - 1) * -100, 1)
    
    return(stuff_plus)
    
  })
  
  output$selected_var <- renderText({ 
    paste0("Your Stuff+ is: ", df())
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)

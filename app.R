# Before the application can be run, wd set up.
# setwd("~/Code/RShiny")

library(shiny)
library(rgdal)
library(rgeos)
library(tmap)
library(leaflet)
library(vioplot)

# Define UI for random distribution app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Camden Data Explorer"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      h3("Make your choice"),
      helpText("Create an interactive map of Camden."),
      
      selectInput("Census_var", 
                  label = "Choose a variable to display",
                  choices = list("Qualification", 
                                 "Unemployment",
                                 "Low Occupancy", 
                                 "White British"),
                  selected = "Qualification"),
      
      h4("About"),
      p("Here's a paragraph description about the project."),
      h4("Data Source"),
      p("This data is from the UK Census.")
    
  ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      textOutput("selected_var"),
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Map", leafletOutput("working_map")),
                  tabPanel("Violin Plot", plotOutput("vioplot"))

      )
      
    )
  )
)

# Define server logic for random distribution app ----
server <- function(input, output) {
  
 # output$selected_var <- renderText({ 
 #   paste("You have selected", input$Census_var)
 # })
  
  CamdenData <- readOGR(".","Camden")


  output$working_map <- renderLeaflet({
    
    #CamdenData <- readOGR(".","Camden")
    
    data <- switch(input$Census_var, 
                            "Qualification" = "Qulfctn",
                            "Unemployment" = "Unmplyd",
                            "Low Occupancy" = "Lw_Occp",
                            "White British" = "Wht_Brt")
    
    color <- switch(input$Census_var, 
                             "Qualification" = "darkgreen",
                             "Unemployment" = "black",
                             "Low Occupancy" = "darkorange",
                             "White British" = "darkviolet")
    
    legend <- switch(input$Census_var, 
                              "Qualification" = "% Qualification",
                              "Unemployment" = "% Unemployment",
                              "Low Occupancy" = "% Low Occupancy",
                              "White British" = "% White British")
    
    working_map <- tm_shape(CamdenData) + tm_fill(data, title=input$Census_var, style="jenks")
    tmap_leaflet(working_map)
    
  })

  
  
  # Generate summary of table
  #output$sum <- renderPrint({
    
    #CamdenData <- readOGR(".","Camden")
  
  #  summary(CamdenData@data[,2:5])
  # })
  
  #Generate a Violin Plot of Variables 
  output$vioplot <- renderPlot({
  vioplot(Census.Data$Unemployed, Census.Data$Qualification, Census.Data$White_British, Census.Data$Low_Occupancy, ylim=c(0,100), col = "dodgerblue", rectCol="dodgerblue3", colMed="dodgerblue4", names=c("Unemployed", "Qualifications", "White British", "Occupancy"))
  })
}

# Create Shiny app ----
shinyApp(ui, server)

# To run application, enter the following in R console
# runApp("MapDemo")
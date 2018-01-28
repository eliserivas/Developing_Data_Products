library(shiny)
library(plyr)
library(dplyr)
library(ggplot2)
library(plotly)

# choices = names(spotify)[c(2,3,4,5,6,7,8,11,12,14)

#### DATA PREP ####
# Load in data
setwd("c:/Users/elrivas/Documents/Trainings/JHU/Class9/Week4/")
spotify <- read.csv("spotify_song_attributes.csv")
# Rename variables so they appear nicely in app
colnames(spotify) <- c("X", "Acousticness","Danceability","Duration","Energy","Instrumentalness","Key",
                       "Liveness","Loudness","Mode", "Speechiness", "Tempo","Time_Signature", "Valence", 
                       "Target","Song_Title","Artist")
# Make duration in seconds
spotify$Duration <- (spotify$Duration/1000)/60
# Factor variables key, mode, time_signature
spotify$Key <- factor(mapvalues(spotify$Key, 
                                from=c(0,1,2,3,4,5,6,7,8,9,10,11), 
                                to=c("C", "C#/Db", "D", "D#/Eb",
                                     "E", "F", "F#/Gb", "G", "G#/Ab",
                                     "A", "A#/Bb", "B")))
spotify$Mode <- factor(mapvalues(spotify$Mode, from=c(0,1), 
                                 to=c("Minor","Major")))
spotify$Time_Signature <- factor(spotify$Time_Signature)
spotify$Target <- factor(mapvalues(spotify$Target, from=c(0,1), 
                                   to=c("Dislike", "Like")))

#### UI ####
ui <- fluidPage(
  ## 2 Title Panels
  # 1 for Title
  # 2 for a green splitter
  titlePanel("Spotify Song Attribute Explorer"),
  titlePanel(fluidRow(column(12, div(style = "height:25px;background-color: green;")))),
  
  verticalLayout(
    fluidRow(column(width=4, 
                    selectInput("x_input", "Select X Input", choices = names(spotify[,c(2,3,4,5,6,8,9,11,12,14)]), selected = "Loudness")),
             column(width=4,
                    selectInput("y_input", "Select Y Input", choices = names(spotify[,c(2,3,4,5,6,8,9,11,12,14)]), selected = "Energy")),
             column(width=4,
                    selectInput("color_input", "Color By", choices = names(spotify[,c(7,10,13,15)]), selected = "Target"))),
    fluidRow(column(width=12, "Select X and Y inputs to plot. You can color observations by a categorical variable. Linear regression represented by the red line. Hover over observations to learn more about the songs",
                    plotlyOutput("scatterPlot")), "The histograms below show the spectrum of values for the selected X and Y variables by whether or not users liked the song"),
    fluidRow(column(width=6, plotOutput("hist_1")),
             column(width=6, plotOutput("hist_2")))
  )
)
  


#### SERVER ####
server <- function(input, output) {

  output$scatterPlot <- renderPlotly({
    ggplot(spotify, aes_string(x=input$x_input, y=input$y_input, color=input$color_input)) + 
      geom_point(alpha=.5) + geom_smooth(method="lm", se=F, aes(group=1), colour="red") + 
      scale_color_brewer(palette = "Set2") +
      theme_minimal()
    ggplotly()
  })
  
  output$hist_1 <- renderPlot({
    ggplot(data=spotify, aes_string(x=input$x_input, fill="Target")) + 
      geom_histogram(alpha=.25, position = "identity") + theme_minimal() +
      scale_color_manual(values=c("darkturquoise", "darkgreen")) + 
      scale_fill_manual(values=c("darkturquoise", "darkgreen")) + ylab("Count")
  })
  
  output$hist_2 <- renderPlot({
    ggplot(data=spotify, aes_string(x=input$y_input, fill="Target")) + 
      geom_histogram(alpha=.25, position = "identity") + theme_minimal() +
      scale_color_manual(values=c("darkturquoise", "darkgreen")) + 
      scale_fill_manual(values=c("darkturquoise", "darkgreen")) + ylab("Count")
  })

}

# Run the application 
shinyApp(ui = ui, server = server)


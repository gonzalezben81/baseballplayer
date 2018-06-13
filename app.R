#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(readr)


Master <- read_csv("Master.csv")
Batting <- read_csv("Batting.csv")
Pitching <- read_csv("Pitching.csv")


# Define UI for application that draws a histogram
ui <- fluidPage(tags$head(includeScript("google-analytics.js")),
   
   # Application title
   titlePanel("Baseball Player Information"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         # textInput("first","Player First Name",value = "Babe"),
         # textInput("last","Player Last Name",value = "Ruth"),
         textInput(inputId = "name",label = "Player Name",value = "Babe Ruth" ),
         textOutput("given")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
        tabPanel(title = "Player Info",tableOutput("distPlot")),
        tabPanel(title = "Player Batting Statistics",dataTableOutput("statsplot")),
        tabPanel(title = "Player Pitching Statistics",dataTableOutput("pitchplot"))
        
      ))
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderTable({
     strsplit(x = input$name,split = " ")
    # playerinfo <- subset(Master,nameFirst == input$first & nameLast == input$last)
    playerinfo <-  subset(Master,NAMES == input$name)
    day <- playerinfo$birthDay
    month <- as.character(playerinfo$birthMonth)
    year <- playerinfo$birthYear
    city <- as.character(playerinfo$birthCity)
    state <- as.character(playerinfo$birthState)
    country <- as.character(playerinfo$birthCountry)
    weight <- playerinfo$weight
    height <- round((playerinfo$height/12),1)
    throws <- as.character(playerinfo$throws)
    bats <- as.character(playerinfo$bats)
    debut <- as.character(playerinfo$debut)
    final <- as.character(playerinfo$finalGame)
    
    
    
    infoplayer <- cbind(day,month,year,city,state,country,weight,height,throws,bats,debut,final)
    
    playerdata <- as.data.frame(infoplayer)
    colnames(playerdata) <- c("Day","Month","Year","City","State","Country","Weight","Height","Throws","Bats","Debut","Final Game")
    playerdata
   })
   
   output$statsplot <- renderDataTable({
     
     
    #playerinfo2 <- subset(Master,nameFirst == input$first & nameLast == input$last)
     
    playerinfo2 <- subset(Master,NAMES == input$name)
     playerone <- playerinfo2$playerID
     
     playerstats <- subset(Batting,Batting$playerID == playerone)
     
     playerstats <- as.data.frame(playerstats)
     
     playerstats <- playerstats[,c(2,4:14)]
     
     colnames(playerstats) <- c("Year","Team","League","G","AB","R","H","2B","3B","HR","RBI","SB")
     
     playerstats
     
        })
   
   output$pitchplot <- renderDataTable({
     
     
     #playerinfo <- subset(Master,nameFirst == input$first & nameLast == input$last)
     playerinfo <- subset(Master,NAMES == input$name)
     
     player <- playerinfo$playerID
     
     playerstats <- subset(Pitching,Pitching$playerID == player)
     
     playerstats <- as.data.frame(playerstats)
     
     playerstats <- playerstats[,c(2,4:17,20)]
     
     colnames(playerstats) <- c("Year","Team","League","W","L","G","GS","CG","SHO","SV","IPOUTS","H","ER","BB","SO","ERA")
     
     playerstats
     
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)


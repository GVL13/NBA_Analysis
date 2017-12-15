#CARMELO App v1
#Plot player ages distribution on each team
#Calculate BMI for each player
#Temp of home city vs team city
library(shiny)
library(tidyverse)
NBArosters <- read_csv("NBARosters.csv")
NBArosters <- NBArosters %>% mutate(
  kg = NBArosters$`#Weight`*0.45,
  inches = sapply(strsplit(NBArosters$`#Height`,"'|\""),
                  function(x){12*as.numeric(x[1]) + as.numeric(x[2])}),
  meters = inches*0.025,
  BMI = kg/(meters*meters)
)
allTeams <- c("All",unique(NBArosters$`#Team Abbr.`))

#############################

ui <- fluidPage(

 pageWithSidebar(
   headerPanel("Analysis of NBA Rosters"),
   sidebarPanel(
     selectInput(inputId = "question", label = h3("Analysis Type"), 
                 choices = c("Height vs Weight by Position","BMI vs Age and Position"), 
                 selected = "Height vs Weight by Position"),
     uiOutput("secondselection")),
   
  mainPanel(
     plotOutput("mainTable"))
 )
)

#############################

server <- function(input, output) {
  
  output$secondSelection <- renderUI({
    selectInput(inputId = "subDecision", label = h3("Team"),
      choices = c("All",allTeams),
      selected = "CLE")})
  
  output$mainTable <- renderPlot({
      if (input$question == "Height vs Weight by Position") {
        ggplot(data = {if(input$secondselection!="All") {filter(NBArosters, `#Team Abbr.` = input$secondselection)}
           else {NBArosters}},
           aes(meters,`#Weight`,color=`#Position`)) + geom_point()}
      else {
        ggplot(data = {if(input$secondselection!="All") {filter(NBArosters, `#Team Abbr.` = input$secondselection)}
           else {NBArosters}},
           aes(BMI,`#Age`,color=`#Position`)) + geom_point()}})}

shinyApp(ui = ui, server = server)


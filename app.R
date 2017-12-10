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
     selectInput("type", label = h3("Analysis Type"), 
                 choices = c("Height vs Weight by Position","BMI vs Age and Position"), 
                 selected = "Height vs Weight by Position")
   ),
   selectInput("subDecision", label = h3(
     if(Input$type=="Height vs Weight by Position")
     {"Height vs Weight by Position"}
     else {"BMI vs Age and Position"}
     ), choices = c("All",allTeams),selected = "CLE"),
   mainPanel(
     plotOutput(if(Input$type=="Height vs Weight by Position"){'value'}
                else {'value2'})
   )
 )
)

#############################

server <- function(input, output) {
  output$value <- renderPlot({
    ggplot(data = if(input$select!="All") {filter(NBArosters, `#Team Abbr.` == input$select)}
           else {NBArosters},
           aes(meters,`#Weight`,color=`#Position`)) + geom_point()
    #input$select is your variable for picking team name 
    })
  output$value2 <- renderPlot({
    ggplot(data = if(input$select!="All") {filter(NBArosters, `#Team Abbr.` == input$select)}
           else {NBArosters},
           aes(BMI,`#Age`,color=`#Position`)) + geom_point()
    #input$select is your variable for picking team name 
  })
}

shinyApp(ui = ui, server = server)


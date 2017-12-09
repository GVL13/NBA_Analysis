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
   headerPanel('Height vs Weight of NBA Players'),
   sidebarPanel(
     selectInput("select", label = h3("Team"), 
                 choices = allTeams, 
                 selected = "CLE")
   ),
   mainPanel(
     plotOutput('value')
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
}

shinyApp(ui = ui, server = server)


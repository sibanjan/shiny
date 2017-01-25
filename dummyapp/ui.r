# ui.R
library(shiny)
shinyUI(fluidPage(
  titlePanel("Simple Shiny Application"),
  sidebarLayout(
    sidebarPanel(
      p("Shiny Demo App"),
      selectInput("x", "Select X axis",choices = c("Gender","Dept")) 
    ),
    mainPanel(
          h4(textOutput("graph_vars")),
         plotOutput("plotgraphs"))   )
  ))   

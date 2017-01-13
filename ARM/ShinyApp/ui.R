#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(shiny)
library(arulesViz)

#ui.R

ui <- fluidPage(
  titlePanel("Association Rule Mining in Bakery Dataset"),
  sidebarPanel( 
    helpText("Show the bar chart/histogram with information of Association Rule Mining in Bakery Dataset"),

    
    selectInput("VarX",
                label = "Method:",
                choices = c("Scatter Plot", "Grouped", "Graph", "Parallel Coordinates"),
                selected = "Scatter Plot")
  ),
  mainPanel(plotOutput("bar")))
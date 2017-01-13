#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(shiny)
library(arulesViz)
library(arules)

setwd("C:/Users/NicholasTan/Desktop/Studies/Gamma - Sem 2/TDS3301-Data Mining/Assignment/Part 2/ARM")
sparseVector <- read.csv("1000-out1.csv", header = FALSE, fill = TRUE, col.names = c("Receipt_No.", "Food_1", "Food_2", "Food_3", "Food_4", "Food_5", "Food_6", "Food_7", "Food_8"))
fullBinaryVector <- read.csv("1000-out2.csv", header = FALSE)
itemTable <- read.csv("1000i.csv", header = FALSE)
foodNames <- c("Chocolate Cake", "Lemon Cake", "Casino Cake", "Opera Cake", "Strawberry Cake", "Truffle Cake", "Chocolate Eclair", "Coffee Eclair", "Vanilla Eclair", "Napoleon Cake", "Almond Tart", "Apple Pie", "Apple Tart", "Apricot Tart", "Berry Tart", "Blackberry Tart", "Blueberry Tart", "Chocolate Tart", "Cherry Tart", "Lemon Tart", "Pecan Tart", "Ganache Cookie", "Gongolais Cookie", "Raspberry Cookie", "Lemon Cookie", "Chocolate Meringue", "Vanilla Meringue", "Marzipan Cookie", "Tulie Cookie", "Walnut Cookie", "Almond Croissant", "Apple Croissant", "Apricot Croissant", "Cheese Croissant", "Chocolate Croissant", "Apricot Danish", "Apple Danish", "Almond Twist", "Almond Bear Claw", "Blueberry Danish", "Lemon Lemonade", "Raspberry Lemonade", "Orange Juice", "Green Tea", "Bottled Water", "Hot Coffee", "Chocolate Coffee", "Vanilla Frappuccino", "Cherry Soda", "Single Espresso")
foodID <- c(0:49)
lookUpTable <- data.frame(foodID, foodNames)
names(itemTable) <- c("Receipt_No.", "Quantity", "Food_No.")
names(fullBinaryVector) <- c("Receipt No.", foodNames)

#To make sure empty value is NA
is.na(sparseVector) <- !sparseVector

#To change all food ID to food name for easier navigation using a lookup table to find matching food and id
itemTable$Food_No. <- lookUpTable$foodNames[match(itemTable$Food_No., lookUpTable$foodID)]
names(itemTable)[names(itemTable) == 'Food_No.'] <- 'Food'
sparseVector$Food_1 <- lookUpTable$foodNames[match(sparseVector$Food_1, lookUpTable$foodID)]
sparseVector$Food_2 <- lookUpTable$foodNames[match(sparseVector$Food_2, lookUpTable$foodID)]
sparseVector$Food_3 <- lookUpTable$foodNames[match(sparseVector$Food_3, lookUpTable$foodID)]
sparseVector$Food_4 <- lookUpTable$foodNames[match(sparseVector$Food_4, lookUpTable$foodID)]
sparseVector$Food_5 <- lookUpTable$foodNames[match(sparseVector$Food_5, lookUpTable$foodID)]
sparseVector$Food_6 <- lookUpTable$foodNames[match(sparseVector$Food_6, lookUpTable$foodID)]
sparseVector$Food_7 <- lookUpTable$foodNames[match(sparseVector$Food_7, lookUpTable$foodID)]
sparseVector$Food_8 <- lookUpTable$foodNames[match(sparseVector$Food_8, lookUpTable$foodID)]

itemTable$Receipt_No. <- as.numeric(itemTable$Receipt_No.)

transTable <- as(split(itemTable$Food, itemTable$Receipt_No.), "transactions")

basket_rules <- apriori(transTable, parameter = list(sup = 0.02, conf = 0.75))

#Sort the rules by confidence
basket_rules_sorted <- sort(basket_rules, by = "confidence")

#Find redundant rules
subset_matrix <- is.subset(basket_rules_sorted, basket_rules_sorted)
subset_matrix[lower.tri(subset_matrix, diag=T)] <- NA
redundant <- colSums(subset_matrix, na.rm=T) >= 1

#Remove redundant rules
rules_pruned <- basket_rules_sorted[!redundant]
basket_rules <- rules_pruned

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$bar<- renderPlot({
    x <- input$VarX
    
    if(x == "Grouped")
    {
      plot(basket_rules, method = "grouped", control = list(k = 5))
    }
    else if(x == "Graph")
    {
      plot(basket_rules, method="graph", control=list(type="items"))
    }
    else if(x == "Parallel Coordinates")
    {
      plot(basket_rules, method="paracoord",  control=list(alpha=.5, reorder=TRUE))
    }
    else if(x == "Scatter Plot")
    {
      plot(basket_rules,measure=c("support","lift"),shading="lift")
    }
  })
})



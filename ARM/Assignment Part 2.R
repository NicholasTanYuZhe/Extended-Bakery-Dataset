foodNames <- c("Chocolate Cake", "Lemon Cake", "Casino Cake", "Opera Cake", "Strawberry Cake", "Truffle Cake", "Chocolate Eclair", "Coffee Eclair", "Vanilla Eclair", "Napoleon Cake", "Almond Tart", "Apple Pie", "Apple Tart", "Apricot Tart", "Berry Tart", "Blackberry Tart", "Blueberry Tart", "Chocolate Tart", "Cherry Tart", "Lemon Tart", "Pecan Tart", "Ganache Cookie", "Gongolais Cookie", "Raspberry Cookie", "Lemon Cookie", "Chocolate Meringue", "Vanilla Meringue", "Marzipan Cookie", "Tulie Cookie", "Walnut Cookie", "Almond Croissant", "Apple Croissant", "Apricot Croissant", "Cheese Croissant", "Chocolate Croissant", "Apricot Danish", "Apple Danish", "Almond Twist", "Almond Bear Claw", "Blueberry Danish", "Lemon Lemonade", "Raspberry Lemonade", "Orange Juice", "Green Tea", "Bottled Water", "Hot Coffee", "Chocolate Coffee", "Vanilla Frappuccino", "Cherry Soda", "Single Espresso")
foodID <- c(0:49)
lookUpTable <- data.frame(foodID, foodNames)

sparseVector <- read.csv("1000-out1.csv", header = FALSE, fill = TRUE, col.names = c("Receipt_No.", "Food_1", "Food_2", "Food_3", "Food_4", "Food_5", "Food_6", "Food_7", "Food_8"))
fullBinaryVector <- read.csv("1000-out2.csv", header = FALSE)
itemTable <- read.csv("1000i.csv", header = FALSE)
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
str(itemTable$Receipt_No.)

#install.packages("arules", dependencies = TRUE)
library(arules)
transTable <- as(split(itemTable$Food, itemTable$Receipt_No.), "transactions")
head(transTable)
inspect(transTable)

basket_rules <- apriori(transTable, parameter = list(sup = 0.02, conf = 0.75))
inspect(basket_rules)
head(basket_rules)

basket_rules <- sort(basket_rules, by = "confidence")

quality(basket_rules)$improvement <- interestMeasure(basket_rules, measure = "improvement")
inspect(basket_rules)

#install.packages("arulesViz")
library(arulesViz)
plot(basket_rules)
plot(basket_rules, method = "grouped", control = list(k = 5))
plot(basket_rules, method="graph", control=list(type="items"))
plot(basket_rules, method="paracoord",  control=list(alpha=.5, reorder=TRUE))
plot(basket_rules,measure=c("support","lift"),shading="confidence")
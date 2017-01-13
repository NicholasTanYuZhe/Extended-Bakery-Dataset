setwd("C:/Users/User-PC/Desktop/MMU/Degree/Gamma 2/Data Mining/Assignment/Part 2")
df_bakery <- read.csv("1000i.csv", header=FALSE, col.names = c("RecipeNo","Quantity","ItemNo"))
df_bakery$Quantity <- NULL

install.packages("plyr")
library(plyr)
library(dplyr)

df_bakeryList <- ddply(df_bakery,c("RecipeNo"),
function(df1)paste(df1$ItemNo, collapse = ","))
df_bakeryList$RecipeNo <- NULL
colnames(df_bakeryList) <- c("itemList")

write.csv(df_bakeryList,"ItemList.csv", row.names = TRUE)

library(arules)
txn = read.transactions(file="ItemList.csv", rm.duplicates= TRUE, format="basket",sep=",",cols=1);
inspect(txn)

basket_rules <- apriori(txn,parameter = list(sup = 0.01, conf = 0.5,target="rules"))
inspect(basket_rules)

library(arulesViz)
plot(basket_rules)

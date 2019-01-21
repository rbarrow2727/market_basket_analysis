install.packages("arules")
install.packages("arulesViz")
library(arules)
install.packages("grid")
install.packages("gdata")
install.packages("caTools")
library(arulesViz)


#import transactions dataset
transData <- read.transactions("C:/Users/Riley Barrow/Desktop/UT Data Class/Course 2/Task 4/MarketBasketAnalysis/ElectronidexTransactions2017.csv", 
                               format = "basket", sep = ",", rm.duplicates = TRUE)
#inspect transData
inspect(transData) # You can view the transactions. Is there a way to see a certain # of transactions?
length(transData) # Number of transactions.
size(transData) # Number of items per transaction
LIST(transData) # Lists the transactions by conversion (LIST must be capitalized)
itemLabels(transData)[1:125] # To see the item labels
summary(transData)

#visualize the data (creats bar grpahs)
itemFrequencyPlot(transData,
                  type = "relative",
                  topN = 10,
                  horiz = TRUE,
                  col = 'steelblue3',
                  xlab = '',
                  main = 'Item Frequency, Relative')

itemFrequencyPlot(transData,
                  type = "absolute",
                  topN = 10,
                  horiz = TRUE,
                  col = 'steelblue3',
                  xlab = '',
                  main = 'Item Frequency, Absolute')

#view items and transactions 
image(transData[1:200, 1:20])

#create a contigency table 
tbl <- crossTable(transData, sort = TRUE)
tbl
tbl['HP Laptop','Apple Earpods']
tbl[1:5,1:5]
crossTable(transData, measure = 'chi')['iMac', 'Apple MacBook Air']


#Apriori Algorithm
set.seed(2727)
apriori(transData)
AP1 <- apriori(transData, parameter =  list(supp = 0.0015, conf = 0.85, minlen = 2
                                            ))  #target = 'rules' / target = 'frequent'
AP1

  #view rules
  inspect(AP1)
  length(AP1)
  summary(AP1)
  sort(AP1)
  write(AP1)
  plot(AP1)
  sample(AP1)
  interestMeasure(AP1)
  
  #sort rules, by = ""
  inspect(sort(AP1, by = "count"))
  inspect(head(AP1, n = 3, by = "lift"))
  
  #reduce number of rules by filetering
  subAP1 <- AP1[quality(AP1)$confidence >= 0.85]
  subAP1
  
  #inspect sub AP1
  inspect(sort(subAP1, by = "confidence"))
  
  #plot subAP1
  plot(subAP1, method = "matrix", measure = "lift")
  plot(subAP1, method = "matrix3D", measure = "lift")
  
  #grouped rules Matrix
  plot(AP1, method = "grouped")
  plot(subAP1, method = "grouped")
  
  #increase the number of groups
  plot(AP1, method = "grouped", control = list(k = 20))
  
  #interactive group matrix
  sel <- plot(AP1, method = "grouped", interactive = TRUE)
  
  #graph based visualiztion
  AP1.sub2 <- head(AP1, n = 100, by = "lift")
  plot(AP1.sub2, method = "graph")
  #export grpah
  saveAsGraph(head(AP1, n = 100, by = "lift"), file = "subAP1rules.graphml")
  
  #parallel coordinates plot
  plot(AP1, method = "paracoord", control = list(reorder = TRUE)) #can remove reorder
    
  #double decker plot
  oneRule <- sample(AP1, 1)
  inspect(oneRule)
  plot(oneRule, method = "doubledecker", data = transData)
  
  #Visulization // ArulesViz
  head(quality(AP1))
  plot(AP1, measure = c("support", "lift"), shading = "confidence") #switches shading and organization
  plot(AP1, method = "two-key plot")
  
  #intetractive plot with buttons zoom, filter, & inspect
  sel <- plot(AP1, measure = c("support", "confidence"), shading = "lift", interactive = TRUE)

  #export rule to excel 
  testtest <- as(AP1, "data.frame")
  write.csv(testtest, ".0015.85rules.csv")

  #remove redundant rules
  is.redundant(AP1)
  subset.matrix <- is.subset(AP1, AP1, sparse = FALSE)
  subset.matrix
  
  #find specific items in the rules 
  ItemRules <- subset(AP1, items %in% "ViewSonic Monitor")
  inspect(ItemRules)
  is.redundant(ItemRules)
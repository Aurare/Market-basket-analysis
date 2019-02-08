library(arules)
library(arulesViz)
library(readr)
library(treemap)



setwd("~/Ubiqum/Module 2/Task 4")

Electro <- read_delim("ElectronidexWithoutImac.csv", 
                      ";", escape_double = FALSE, col_names = FALSE, 
                      trim_ws = TRUE)


trans<- read.transactions("CorrectedEletronidex.csv", format = "basket", sep=";", rm.duplicates = TRUE)


inspect(trans[1:15])
length(trans)
size(trans)

test<-LIST(trans)
test         
test
trans[test]


##############calculating the number of transaction with a k number of products#######

one<- size(trans)==1
one

trans[one]
listone<-LIST(trans[one])
listone


#####ploting ###################

itemFrequencyPlot(trans,topN=20,type="absolute")
itemFrequencyPlot(trans,topN=20,type="relative")

?itemFrequencyPlot

itemFrequencyPlot(trans[eleven],topN=20,type="absolute")
itemFrequencyPlot(trans[two], support=0.05, cex.names=0.8)

# [1] first position (row) [[1]] content of first position [[1]][1] content first element 
test[[1]][1]
lapply()


#image
?image

image(trans[one])

##fre items with eclat
freq.itemsets <- eclat(trans, parameter=list(supp=0.002, maxlen=2))
inspect(freq.itemsets %in% "HP Laptop")

??freq.itemsets

freq.itemsets

frequent<-freq.itemsets


#rules part one ##############################################
rules1 <- apriori(trans, 
                 parameter = list(support = 0.008
                                  , confidence = 0.6)) 

rules6 <- apriori(trans, 
                  parameter = list(support = 0.005
                                   , confidence = 0.7))
              
inspect(rules1)
summary(rules1)

inspect(sort(rules1, by = "Lift"))

inspect(head(sort(rules1, by ="lift"),10))

is.redundant(rules1)

plot(rules1, measure=c("support", "confidence"), shading="lift", interactive=FALSE)
inspect(sort(sort(rules1, by ="support"),by ="confidence")[1:5])

plot(rules1,method="graph",interactive=FALSE,shading="lift")


rules2 <- apriori(trans, 
                  parameter = list(support = 0.0025
                                   , confidence = 0.2)) 
inspect(head(sort(rules1, by ="lift"),20))



plot(rules1, method="paracoord", control=list(reorder=TRUE))

##cyberRules
CyberRules <- subset(rules2, lhs %in% "Microsoft Office Home and Student 2016" & lhs %in% "HP Laptop")
inspect(head(sort(CyberRules, by="lift"),10))

plot(CyberRules, method="paracoord", control=list(reorder=TRUE))
plot(CyberRules, measure=c("support", "confidence"), shading="lift", interactive=FALSE)
plot(CyberRules[1:10] ,method="graph",interactive=FALSE,shading="lift")
plot(CyberRules, method="grouped", measure="support")

##apple earpod
rules3 <- apriori(trans, 
                  parameter = list(support = 0.004
                                   , confidence = 0.4)) 

inspect(rules3)

Earpod <- subset(rules3, items %in% "Apple Earpods")
inspect(sort(Earpod, by="lift"))

plot(Earpod, method="paracoord", control=list(reorder=TRUE))

##macbook air
rules4 <- apriori(trans, 
                  parameter = list(support = 0.004
                                   , confidence = 0.2))
MBookAir <- subset(rules4, items %in% "Apple MacBook Air")
inspect(sort(MBookAir, by="lift"))
plot(MBookAir, method="paracoord", control=list(reorder=TRUE))

##test other enterpris combo
rules5 <- apriori(trans, 
                  parameter = list(support = 0.002
                                   , confidence = 0.3))

inspect(head(sort(rules5, by="lift"),20))
plot(rules5, method="paracoord", control=list(reorder=TRUE))

Dell <- subset(rules5, items %in% "Dell Laptop")
inspect(head(sort(Dell, by="lift"),20))

plot(Dell, method="paracoord", control=list(reorder=TRUE))


#####rules: variations ##############################

rules7 <- apriori(trans, 
                  parameter = list(support = 0.002
                                   , confidence = 0.4)) 
inspect(head(sort(rules7, by="lift"),20))

Dell7 <- subset(rules7, items %in% "iPad")
inspect(head(sort(Dell7, by="lift"),10))
Dell7

plot(Dell7, method="paracoord", control=list(reorder=TRUE))

DellRules <- sort(subset(rules7, subset = lhs %in%  "Dell Desktop"), by = "confidence")
inspect(DellRules)

inspect(sort(freq.itemsets, by="support"))


#######matrix######

plot(Dell7, method="grouped", measure="support")


######good combo for iPad#########

rules7 <- apriori(trans, 
                  parameter = list(support = 0.002
                                   , confidence = 0.4)) 
inspect(head(sort(rules7, by="lift"),20))

iPad <- subset(rules7, items %in% "iPad")
inspect(head(sort(iPad, by="lift"),10))


plot(iPad, method="paracoord", control=list(reorder=TRUE))

iPadRules <- sort(subset(rules7, subset = lhs %in%  "Dell Desktop"), by = "confidence")

plot(iPad, method="grouped", measure="support")

########ipad Pro ######
rules7 <- apriori(trans, 
                  parameter = list(support = 0.004
                                   , confidence = 0.4)) 
inspect(head(sort(rules7, by="lift"),20))

iPad <- subset(rules7, items %in% "iPad Pro")
inspect(head(sort(iPad, by="lift"),10))


plot(iPad, method="paracoord", control=list(reorder=TRUE))



plot(iPad, method="grouped", measure="support")


######Acer Aspire #######
rules7 <- apriori(trans, 
                  parameter = list(support = 0.003
                                   , confidence = 0.3)) 
inspect(head(sort(rules7, by="lift"),20))

Aspire <- subset(rules7, items %in% "Acer Aspire")
inspect(head(sort(Aspire, by="lift"),10))


plot(iPad, method="paracoord", control=list(reorder=TRUE))

plot(Aspire, method="grouped", measure="support")

######MBair #######
rules7 <- apriori(trans, 
                  parameter = list(support = 0.002
                                   , confidence = 0.5)) 
inspect(head(sort(rules7, by="lift"),20))

Air <- subset(rules7, items %in% "Apple MacBook Air")
inspect(head(sort(Air, by="lift"),10))


plot(Air, method="paracoord", control=list(reorder=TRUE))



plot(Air, method="grouped", measure="support")

#######mac book pro ######

rules7 <- apriori(trans, 
                  parameter = list(support = 0.003
                                   , confidence = 0.3)) 
inspect(head(sort(rules7, by="lift"),20))

Pro <- subset(rules7, items %in% "Dell Desktop")
inspect(head(sort(Pro, by="lift"),10))

plot(Pro, method="paracoord", control=list(reorder=TRUE))

plot(Pro, method="grouped", measure="support")

####### cyberpower ####

rules7 <- apriori(trans, 
                  parameter = list(support = 0.003
                                   , confidence = 0.4)) 
inspect(head(sort(rules7, by="lift"),20))

Pro <- subset(rules7, items %in% "CYBERPOWER Gamer Desktop")
inspect(head(sort(Pro, by="lift"),10))

plot(Pro, method="paracoord", control=list(reorder=TRUE))

plot(Pro, method="grouped", measure="support")

######## lenovo #####

rules7 <- apriori(trans, 
                  parameter = list(support = 0.003
                                   , confidence = 0.4)) 
inspect(head(sort(rules7, by="lift"),20))

Pro <- subset(rules7, items %in% "Lenovo Desktop Computer")
inspect(head(sort(Pro, by="lift"),10))

plot(Pro, method="paracoord", control=list(reorder=TRUE))

plot(Pro, method="grouped", measure="support")


######acer desktop ########
rules7 <- apriori(trans, 
                  parameter = list(support = 0.004
                                   , confidence = 0.4)) 
inspect(head(sort(rules7, by="lift"),20))

Pro <- subset(rules7, items %in% "Acer Desktop")
inspect(head(sort(Pro, by="lift"),10))

plot(Pro, method="paracoord", control=list(reorder=TRUE))

plot(Pro, method="grouped", measure="support")




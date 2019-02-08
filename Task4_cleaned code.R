##################################################################################
#############################--Market Basket Analysis--###########################
##################################################################################

##The definitive thresholds for all products are in the GoogleDoc M2T4


library(arules)
library(arulesViz)
library(readr)
library(treemap)

setwd("~/Ubiqum/Module 2/Task 4")#set directory

Electro <- read_delim("ElectronidexWithoutImac.csv",##file with iMac and HP removed 
                      ";", escape_double = FALSE, 
                      col_names = FALSE, 
                      trim_ws = TRUE)

##transforming the data into a sparse matrix
trans<- read.transactions("CorrectedEletronidex.csv",##file with the two empty rows removed
                            format = "basket", 
                            sep=";", 
                            rm.duplicates = TRUE)#remove the duplicates of one element that is contained several times in the same transactions

############--first overview of the data --############################################

inspect(trans[1:15]) #show the first 15 transactions 
length(trans) #gives to total number of transactions
size(trans) #gives the number of items contained in each item set
list<-LIST(trans) #display the data as a list 
summary(trans)#gives info about most frequent items, length distribution, mean etc. 

## List: [1] first position (row) [[1]] content of first position [[1]][1] content first element 
list[[1]][1]
lapply()

############--calculating the number of transaction with a k number of products#######

one<- size(trans)==1 #create a sparse matrix with the transactions that contain only one item per transactions
two<- size(trans)==2 #create a sparse matrix with the transactions that contain only two item per transactions

trans[!one] #look at all transactions, except the one that contain only one product

############--ploting frequency --###############################################################

##ploting the frequency of the items (relative or abosulute)
itemFrequencyPlot(trans,topN=20,type="absolute")
itemFrequencyPlot(trans,topN=20,type="relative")
itemFrequencyPlot(trans[one],topN=20,type="absolute")#plot with 1-item transactions
itemFrequencyPlot(trans[one], support=0.05, cex.names=0.8)#plot with a trehshild for support
?itemFrequencyPlot

##image
?image
image(trans[one]) #visual representation of the binary sparse matrix
image(trans[1:15])#subset selection

##frequent items with eclat
freq.itemsets <- eclat(trans, parameter=list(supp=0.002,#support treshold
                                             maxlen=2)) #max number of items in the basket

inspect(sort(freq.itemsets, by="support"))#combination of the function inspect and sort

freq.itemsets #gives the number of freq.itemsets

frequent<-freq.itemsets

####################--rules first exploration--##############################################

##Apriori algorithem, trying to find the rules with best support and confidence
rules1 <- apriori(trans, 
                  parameter = list(support = 0.007
                                   ,confidence = 0.6)) 

##showing the rules
inspect(rules1)#display the rules
summary(rules1)
inspect(sort(rules1, by = "Lift"))#sort rules by lift
inspect(head(sort(rules1, by ="lift"),10))#first 10 rules by lift
inspect(sort(sort(rules1, by ="support"),by ="confidence")[1:5]) #by support and confidence, [1:5]

##redudancy
is.redundant(rules1)#are there redudant rules? Gives TRUE/FALSE answer

##Scatter plot of the 3values
plot(rules1, measure=c("support", "confidence"), shading="lift", interactive=FALSE)

##Interactions graph, dots and arrows
plot(rules1,method="graph",interactive=FALSE,shading="lift")

##plot rules with arrows, position lhs and rhs
plot(rules1, method="paracoord", control=list(reorder=TRUE))

##############--cyberPower --#########################################################

##apriori algorithm
rules2 <- apriori(trans, 
                  parameter = list(support = 0.003
                                   ,confidence = 0.35))#thresholds found for file with HP and iMac

##subset function
CyberRules <- subset(rules2, items %in% "CYBERPOWER Gamer Desktop")
inspect(sort(CyberRules, by="lift"))

plot(CyberRules[1:5], method="paracoord", control=list(reorder=TRUE))
plot(CyberRules[1:5], measure=c("support", "confidence"), shading="lift", interactive=FALSE)
plot(CyberRules[1:5], method="graph",interactive=FALSE,shading="lift")

##bubble graph
plot(CyberRules[1:5], method="grouped", measure="support")

#############-- apple earpod --########################################################
rules3 <- apriori(trans, 
                  parameter = list(support = 0.004
                                   ,confidence = 0.4)) 

inspect(rules3)

Earpod <- subset(rules3, items %in% "Apple Earpods")
inspect(sort(Earpod, by="lift"))

plot(Earpod, method="paracoord", control=list(reorder=TRUE))

#############-- macbook air --######################################################
rules4 <- apriori(trans, 
                  parameter = list(support = 0.004
                                   , confidence = 0.2))
MBookAir <- subset(rules4, items %in% "Apple MacBook Air")
inspect(sort(MBookAir, by="lift"))
plot(MBookAir, method="paracoord", control=list(reorder=TRUE))

################-- Dell Desktop ################################################################
rules5 <- apriori(trans, 
                  parameter = list(support = 0.003
                                   ,confidence = 0.44))

inspect(head(sort(rules5, by="lift"),20))
plot(rules5, method="paracoord", control=list(reorder=TRUE))

Dell <- subset(rules5, items %in% "Dell Desktop")
inspect(head(sort(Dell, by="lift"),10))

plot(Dell, method="paracoord", control=list(reorder=TRUE))

plot(Dell, method="grouped", measure="support")




##################-- iPad--######################################################

rules7 <- apriori(trans, 
                  parameter = list(support = 0.002
                                   ,confidence = 0.4)) 

inspect(head(sort(rules7, by="lift"),20))

iPad <- subset(rules7, items %in% "iPad")
inspect(head(sort(iPad, by="lift"),10))


plot(iPad, method="paracoord", control=list(reorder=TRUE))

iPadRules <- sort(subset(rules7, subset = lhs %in%  "iPad"), by = "confidence")#iPad in the lhs

plot(iPad, method="grouped", measure="support")

#####################-- iPad Pro --##################################################
rules7 <- apriori(trans, 
                  parameter = list(support = 0.002
                                   , confidence = 0.4)) 
inspect(head(sort(rules7, by="lift"),20))

iPad <- subset(rules7, items %in% "iPad Pro")
inspect(head(sort(iPad, by="lift"),10))

plot(iPad, method="paracoord", control=list(reorder=TRUE))

plot(iPad, method="grouped", measure="support")

#####################-- Acer Aspire --###############################################
rules7 <- apriori(trans, 
                  parameter = list(support = 0.003
                                   ,confidence = 0.3))

inspect(head(sort(rules7, by="lift"),20))

Aspire <- subset(rules7, items %in% "Acer Aspire")
inspect(head(sort(Aspire, by="lift"),10))

plot(Aspire, method="paracoord", control=list(reorder=TRUE))

plot(Aspire, method="grouped", measure="support")

###################--MacBook Air -- ################################################
rules7 <- apriori(trans, 
                  parameter = list(support = 0.0025
                                   ,confidence = 0.4)) 
inspect(head(sort(rules7, by="lift"),20))

Air <- subset(rules7, items %in% "Apple MacBook Air")
inspect(head(sort(Air, by="lift"),10))


plot(Air, method="paracoord", control=list(reorder=TRUE))

plot(Air, method="grouped", measure="support")

#############-- MacBook Pro --######################################################

rules7 <- apriori(trans, 
                  parameter = list(support = 0.003
                                   ,confidence = 0.35)) 

inspect(head(sort(rules7, by="lift"),20))

Pro <- subset(rules7, items %in% "Apple MacBook Pro")
inspect(head(sort(Pro, by="lift"),10))

plot(Pro, method="paracoord", control=list(reorder=TRUE))

plot(Pro, method="grouped", measure="support")

###################-- cyberpower --###################################################

rules7 <- apriori(trans, 
                  parameter = list(support = 0.003
                                   ,confidence = 0.4)) 

inspect(head(sort(rules7, by="lift"),20))

Cyber <- subset(rules7, items %in% "CYBERPOWER Gamer Desktop")
inspect(head(sort(Cyber, by="lift"),10))

plot(Cyber, method="paracoord", control=list(reorder=TRUE))

plot(Cyber, method="grouped", measure="support")

############################-- lenovo --##############################################

rules7 <- apriori(trans, 
                  parameter = list(support = 0.003
                                   ,confidence = 0.4)) 

inspect(head(sort(rules7, by="lift"),20))

Pro <- subset(rules7, items %in% "Lenovo Desktop Computer")
inspect(head(sort(Pro, by="lift"),10))

plot(Pro, method="paracoord", control=list(reorder=TRUE))

plot(Pro, method="grouped", measure="support")

####################-- acer desktop --##################################################
rules7 <- apriori(trans, 
                  parameter = list(support = 0.003
                                   ,confidence = 0.4))
                                    
inspect(head(sort(rules7, by="lift"),20))

Pro <- subset(rules7, items %in% "Acer Desktop")
inspect(head(sort(Pro, by="lift"),10))

plot(Pro, method="paracoord", control=list(reorder=TRUE))

plot(Pro, method="grouped", measure="support")

######################-- microsoft keyboard --########################################

rules7 <- apriori(trans, 
                  parameter = list(support = 0.0035
                                   , confidence = 0.3)) 

inspect(head(sort(rules7, by="lift"),20))

Pro <- subset(rules7, items %in% "Backlit LED Gaming Keyboard")
inspect(head(sort(Pro, by="lift"),10))

plot(Pro, method="paracoord", control=list(reorder=TRUE))

plot(Pro, method="grouped", measure="support")

plot(Pro[1:5], method="graph", control=list(type="items")) 

#################-- tablet Fire--######################################################

rules7 <- apriori(trans, 
                  parameter = list(support = 0.002
                                   , confidence = 0.2)) 
inspect(head(sort(rules7, by="lift"),20))

Pro <- subset(rules7, items %in% "Kindle")

inspect(head(sort(Pro, by="lift"),10))

plot(Pro, method="paracoord", control=list(reorder=TRUE))

plot(Pro, method="grouped", measure="support")

plot(Pro[1:5], method="graph", control=list(type="items")) 

######################electronidex treemap df #######################################

ETreeMap <- read_delim("ElecForTreeMap.csv", #file in df format
                             ";", escape_double = FALSE, trim_ws = TRUE)

ETreeMap$X5<-NULL
ETreeMap$X6<-NULL

colnames(ETreeMap)

##treemap creation
treemap(ETreeMap, #Your data frame object
        index=c("Items"),  #A list of your categorical variables
        vSize = "Number",  #This is your quantitative variable
        type="index", #Type sets the organization and color scheme of your treemap
        #palette from the RColorBrewer presets or make your own.
        title="Electronidex", #Customize your title
        fontsize.title = 14 #Change the font size of the title
)

?palette               
palette()


############-- blackwel treemap df --################################################
Blackwell <- read_delim("BlackwellForTreeMap.csv", 
                                  ";", escape_double = FALSE, trim_ws = TRUE)
colnames(Blackwell)

treemap(Blackwell, #Your data frame object
        index=c(""),  #A list of your categorical variables
        vSize = "Number",  #This is your quantitative variable
        type="index", #Type sets the organization and color scheme of your treemap
        #palette from the RColorBrewer presets or make your own.
        title="Blackwell", #Customize your title
        fontsize.title = 14) #Change the font size of the title



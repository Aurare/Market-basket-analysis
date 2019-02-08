library(arules)
library(arulesViz)
library(readr)
library(treemap)

setwd("~/Ubiqum/Module 2/Task 4")

Electro <- read_delim("CorrectedEletronidex.csv", 
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

two<-size(trans)==2
trans[!two]

three<-size(trans)==3
trans[three]

four<-size(trans)==4
trans[four]

five<-size(trans)==5
trans[five]

six<-size(trans)==6
trans[six]

seven<-size(trans)==7
trans[seven]

eight<-size(trans)==8
trans[eight]

nine<-size(trans)==9
trans[nine]

ten<-size(trans)==10
trans[ten]

eleven<-size(trans)==11
trans[eleven]

twelve<-size(trans)==12
trans[twelve]

thirteen<-size(trans)==13
trans[thirteen]

fourteen<-size(trans)==14
trans[fourteen]

fifteen<-size(trans)==15
trans[fifteen]

sixteen<-size(trans)==16
trans[sixteen]

seventeen<-size(trans)==17
trans[seventeen]

eighteen<-size(trans)==18
trans[eighteen]

nineteen<-size(trans)==19
trans[nineteen]

twenty<-size(trans)==20
trans[twenty]

twentyone<-size(trans)==21
trans[twentyone]

twentytwo<-size(trans)==22
trans[twentytwo]

twentythree<-size(trans)==23
trans[twentythree]

tfour<-size(trans)==29
trans[tfour]

inspect(trans[tfour])

      
thirty<-size(trans)==30
trans[thirty]

LIST(trans)
inspect(trans)

size(trans)

hist(Electro)



one<-size(trans)==max(trans)
one
inspect(trans)

size(trans)

itemLabels(trans)

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
freq.itemsets <- eclat(trans[one], parameter=list(supp=0.002, maxlen=1))
inspect(freq.itemsets)


#rules
rules <- apriori(trans, parameter = list(support = 0.1, confidence = 0.8))
inspect(rules)



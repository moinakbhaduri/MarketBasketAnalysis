#--First, the necessary packages--#

#--library(arules)--#
#--library(arulesviz)--#
#--library(ggplot2)--#
#--library(ggrepel)--#

#--Let's create a hypothetical basket, summarising 18 receipts--#
market.basket=list(
  c("Milk","Cornflakes"),
  c("Beer"),
  c("Milk","Cornflakes"),
  c("Peanuts"),
  c("Milk","BreadRoll"),
  c("Milk","BreadRoll","Butter"),
  c("Cornflakes"),
  c("Milk","BreadRoll","Butter"),
  c("Chocolate"),
  c("Milk","BreadRoll","Butter"),
  c("BreadRoll","Butter"),
  c("BreadRoll","Butter"),
  c("Butter"),
  c("Butter"),
  c("MineralWater"),
  c("PlasticCurtlery","PlasticCups","Beer"),
  c("Cornflakes","OrganicBeans","OrganicEggs"),
  c("OrganicBeans","Cornflakes")
)

#--Let's attach transaction numbers, for convenience---#
names(market.basket) <- paste("T", c(1:length(market.basket)), sep = "")

#--We need to convert this to a "transaction" object--#
Transaction.binary=as(market.basket,"transactions")
#--Let's see how many unique items we have--#
itemLabels(Transaction.binary)
#--A neat summary (like the five-number summary, only in a transactional setting)--#
summary(Transaction.binary)
#--A quick graph. A black spot indicates the presence of an item--#
image(Transaction.binary)
#--A barplot showing which items are popular--#
itemFrequencyPlot(Transaction.binary,  cex.names=1)


#--We now move on complicated descriptives--#
#---First, create some association rules---#
rules <- apriori(Transaction.binary, 
                 parameter = list(supp=0.0, conf=0.0, #--what if I increase my support to 0.3? Decrease both my support and confidence to 0?---#
                                  maxlen=2,minlen=2, 
                                  target= "rules"))
summary(rules) #--again, an initial summary--#
inspect(rules) #--revealing the "if" and "then" parts in detail--#

#--Weeding out infrequent combinations--#
a=which(inspect(rules)$support==0)
b=which(inspect(rules)$confidence==0)
del=unique(a,b)

#--A summary of connections between/among frequent items--#
reduced=inspect(rules)[-del,]

#--plotting the "basket-complementary" bands--#
x=reduced$confidence[seq(1,length(reduced$confidence),2)]
y=reduced$confidence[seq(2,length(reduced$confidence),2)]

conf.data=data.frame(x,y)
rownames(conf.data)<-c("P.Cups,P.Curtlery","P.Cups,Beer","P.Curtlery,Beer","O.Eggs,O.Beans","O.Eggs,Cornflakes",
                       "O.Beans,Cornflakes","Cornflakes,Milk","Milk,Breadroll","Milk,Butter","Breadroll,Butter")
ggplot(conf.data, aes(x=x, y=y)) + geom_point()+geom_text_repel(label=rownames(conf.data))+xlim(0,1)+ylim(0,1)+
  geom_hline(yintercept=0.25,color="blue")+
  geom_vline(xintercept=0.25,color="blue")+
  geom_abline(intercept = 0, slope = 1, color="red", 
              linetype="dashed",size=1)+
  geom_abline(intercept = -0.08, slope = 1, color="green", 
              linetype="dashed",size=1)+
  geom_abline(intercept = 0.08, slope = 1, color="green", 
              linetype="dashed",size=1)+
  geom_abline(intercept = -0.13, slope = 1, color="orange", 
              linetype="dashed",size=1)+
  geom_abline(intercept = 0.13, slope = 1, color="orange", 
              linetype="dashed",size=1)+ 
  labs(y = "Confidence B->A", x = "Confidence A->B")+
  annotate(geom="text", x=0.5, y=1, label="Region B",
           color="red")+
  annotate(geom="text", x=0.15, y=1, label="Region A",
           color="red")+
  annotate(geom="text", x=0.5, y=0.15, label="Region C",
           color="red")+
  annotate(geom="text", x=0.15, y=0.15, label="Region D",
           color="red")+
  annotate(geom="text", x=0.10, y=0.8, label="Tolerance = 0.08",
           color="green")+
  annotate(geom="text", x=0.10, y=0.78, label="Tolerance = 0.13",
           color="orange")+
  ggtitle("Basket complementarity under varying tolerance")

      #########################################################################
     #--We can look at complicated rules by varying our support and confidence--#
      #########################################################################

#---We can look at more complicated rules---#
rules.general<-apriori(Transaction.binary, 
                       parameter = list(supp=0.01, conf=0.01, #--what if I increase my support to 0.3? Decrease both my support and confidence to 0?---#
                                        maxlen=5,
                                        target= "rules"))

plot(rules.general) #--may add jitter=0.5 in case rules overlap--#
plot(rules.general,method = "matrix")
plot(rules.general,method = "two-key plot")

plot(rules.general, engine = "plotly")
plot(rules.general, method = "graph",  engine = "htmlwidget")
#--A Shiny dashboard---#
#--A Shiny dashboard--#
ruleExplorer(rules.general, sidebarWidth = 2, graphHeight = '600px')

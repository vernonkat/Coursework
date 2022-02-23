#Load in the SkuMaster.csv dataset.  
data1 <- (read.csv("https://raw.githubusercontent.com/vernonkat/Coursework/main/HW3SKU%20Master.csv",fileEncoding="UTF-8-BOM"))

# We would only like to consider those observations for which the Cubic Feet per UOM is greater than zero and less than two, 
# and for which the Weight per UOM is greater than zero and less than 50.  
data2 <- data1[data1$UomCube<2 & data1$UomCube>0 & data1$UomWeight<50 & data1$UomWeight>0, ]

#Note also that only the factor levels of Case(CA), Each(EA), Pallet(PL), and Pound(PL) for the Units of Measure (UoM) are admissible, 
# all observations with other designations should be omitted.  
data3 <- data2[data2$Uom==c("CA","EA","PL","LB"), ]

# In addition, all rows with NA should be dropped.
library(tidyr)
dafr <- data3 %>% drop_na()
dafr<-droplevels(dafr)

# Create a boxplot on Weight per UOM. 
boxplot(dafr$UomWeight,xlab="Weight per UOM",main="UoM Weight",col="light Blue")

# How many observations lie above the upper whisker? Those outliers above the upper whisker are deemed valid and should be left in the dataframe.  
length(boxplot(dafr$UomWeight)$out)
#14 total outliers

# Create a scatterplot on the Units per Case and the Weight per UOM
plot(dafr$UomWeight,dafr$UnitsPerCase,xlab="Weight per UOM",ylab="Units per Case",main="Units per Case by Weight per UoM",pch = 5)

# Create a plot showing the frequency/count with which the levels of the factor Commodity occur
plot(dafr$Commodity,xlab="Commodity")
library(ggplot2)

ggplot(data=dafr,dafr$Commodity)
ggplot(data=dafr, aes(x=Commodity)) +
  geom_bar() +
  ggtitle("Frequency of commmodity") +
  theme(axis.text.x = element_text(angle = 90))

# Create a plot showing the frequency/count with which the levels of the Units of Measure occur
plot(dafr$Uom,xlab="Units of Measure")

# Create a side-by-side boxplot of Cubic Feet by UoM by the types of Flow. 
boxplot((dafr$UomCube)~(dafr$Flow),data=dafr,xlab="Flow",ylab="Cubic Feet by Uom",main="Cubic Feet by UoM by the Types of Flow",col=rainbow(4))

install.packages("qcc")
library(qcc)
pareto.chart(dafr$UomWeight,xlab=dafr$UomWeight)

#####Now restrict your attention to only those products that pass through the Direct to Store supply chain channel 
dafr2 <- dafr[dafr$Flow==c("DD"), ]
dafr2<-droplevels(dafr2)

# Create a boxplot on the Weight per UOM.  Which observations/rows appear to be outliers?  
boxplot(dafr2$UomWeight,xlab="Weight per UoM",col="yellow",main="Boxplot of Weight per UoM")
points(rep(1,length(dafr2$UomWeight)),dafr2$UomWeight,pch=16)
#no outliers by this measure, but there are some high numbers in other categories(Weight & Units per Case)

# After checking with the production managers, it is possible (but unlikely) to have Weights per UoM up to 30, and for disposable or plastic items to have Units per Case in the 1000's.  
#With this information, would you filter or keep these outlier in the dataframe?
#Given this information, our one extremes of 1000 plastic type and higher loads of Weights are within the normal range and should be kept

#Create a histogram on Weight per UoM
hist(dafr2$UomWeight,xlab="UoM Weight",main="Histogram of UoM Weight",col="Light Green",ylim=c(0,10))
axis(side=2,at=seq(0,100, 10),labels=seq(0,1000,100))
# Create a dotplot on Weight per Uom labeled with the SKU Number.  Which Sku has the highest Weight per Uom?
dotchart(dafr2$UomWeight,dafr2$SkuNbr,ylab="SKU",xlab="Weight per Uom",pch=5,main="SKU Weights by UoM")
#it is 06992111

# Create a stripchart for Weight per Uom by the Units of Measure (there should only be Each(EA) and Case(CA)). 
stripchart(dafr2$UomWeight~dafr2$Uom,xlab="UomWeight",ylab="Uom",pch=2,method="jitter",main="Weight per Uom by the Units of Measure")


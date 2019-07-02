
myData1 <- read.csv(file="~/Desktop/df5d.csv",
               head=TRUE, fileEncoding='UTF-8-BOM')
attach(myData1)
myData1$Pricecategory[Price > 1000000] <- "over 1 million"
myData1$Pricecategory[Price > 500000 & Price <= 750000] <- "500k-750k"
myData1$Pricecategory[Price > 750000 & Price <= 1000000] <- "750k-1000k"
myData1$Pricecategory[Price <= 500000] <- "below 500k"
myData1$Regionname[Regionname =='Northern Metropolitan'] <- 'Northern Metropolitan'
myData1$Regionname[Regionname =='Southern Metropolitan'] <- 'Southern Metropolitan'
myData1$Regionname[Regionname =='South-Eastern Metropolitan'] <- 'South-Eastern Metropolitan'
myData1$Regionname[Regionname =='Eastern Metropolitan'] <- 'Eastern Metropolitan'
myData1$Regionname[Regionname =='Western Metropolitan'] <- 'Western Metropolitan'
detach(myData1)
#Create data
name=past("region",c(myData1$Regionname))

feature=paste("Price ", c(myData1$Pricecategory) , sep="")
dat <- data.frame(name,feature)
dat <- with(dat, table(name, feature))

# Charge the circlize library
library(circlize)

# Make the circular plot
chordDiagram(as.data.frame(dat), transparency = 0.5)

mydata1 <- head(mydata1,3000)
line_fit <- lm(Price~Distance, data =mydata1)
summary(line_fit)
par(mfrow=c(2,2))
plot(line_fit)
x <- data.frame(Distance=5)
predict(line_fit,x,interval = "prediction",level = 0.95)



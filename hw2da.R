mydata<-read.table("C:/Users/ricknimo/Rfolder/hw2dadata.txt")
summary(mydata$V1)
c1 <- cut(mydata$V1, breaks = 10)
table(c1)


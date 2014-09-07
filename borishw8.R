dataframe <-read.table("borisdata.R", col.names = c ("x1", "x2", "x3", "x4", "name") )

dataframe$name <- NULL #eliminate name variable

means <- colMeans(dataframe) #this is a vector


rangex1 <- max(dataframe$x1) - min(dataframe$x1) 
rangex2 <- max(dataframe$x2) - min(dataframe$x2) 
rangex3 <- max(dataframe$x3) - min(dataframe$x3) 
rangex4 <- max(dataframe$x4) - min(dataframe$x4)


dataframe2 <- cbind( (dataframe$x1 - means["x1"]) / rangex1,
                     (dataframe$x2 - means["x2"]) / rangex2,
                     (dataframe$x3 - means["x3"]) / rangex3,
                     (dataframe$x4 - means["x4"]) / rangex4    ) 

fit <- kmeans(dataframe2, 3, iter.max=900 )
print(fit)
#fit <- kmeans(mydata, 3) # 3 cluster solution
#kmeans(x, centers, iter.max = 10, nstart = 1, algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"))
#kmeans(x, centers, iter.max = 10, nstart = 1, algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"))

#plot(dataframe2, col = fit$cluster)
# plot centers
#points(fit$centers, col = 1:3, pch = 8)

 
# get cluster means 
#print(aggregate(dataframe2,by=list(fit$cluster),FUN=mean)   )               

#range_span <- function(x) return(diff(range(x))) #define function
#meanx4 <- tapply(dataframe$x4, dataframe$name, mean) #apply to subsets 




############################
###   Quetelet Indices   ###
############################

#row1 <- c( mytable[1,1], mytable[1,2], mytable[1,3], mytable[1,4])/ sum(mytable[1, 1:4])
#row2 <- c( mytable[2,1], mytable[2,2], mytable[2,3], mytable[2,4])/ sum(mytable[2, 1:4])
#row3 <- c( mytable[3,1], mytable[3,2], mytable[3,3], mytable[3,4])/ sum(mytable[3, 1:4])

#Grandtotal <- sum(mytable[1:3,1:4])

#column1sum <- sum(mytable[1:3, 1] )
#column2sum <- sum(mytable[1:3, 2] )
#column3sum <- sum(mytable[1:3, 3] )
#column4sum <- sum(mytable[1:3, 4] )

#E1 <- column1sum/Grandtotal  #TsubColj/N
#E2 <- column2sum/Grandtotal
#E3 <- column3sum/Grandtotal
#E4 <- column4sum/Grandtotal

#Evector <- c (E1, E2, E3, E4) #vector of column probabilities

#Qrow1 <- row1/Evector -1
#Qrow2 <- row2/Evector -1
#Qrow3 <- row3/Evector -1

#Qmatrix <- rbind (Qrow1, Qrow2, Qrow3)

#out <- chisq.test(mytable)
#print(out)
 
#print (Qmatrix)

###########################
####  Pearson Indices  ####
###########################

#First,form the vectors of expected 

#rowsums <- margin.table(mytable, 1)

#colsums <- margin.table(mytable, 2)

#Row1Exp <- rowsums[1] * colsums / Grandtotal
#Row2Exp <- rowsums[2] * colsums / Grandtotal
#Row3Exp <- rowsums[3] * colsums / Grandtotal

#ExpMatrix <- rbind(Row1Exp, Row2Exp, Row3Exp)

#PearsonMatrix <- (mytable-ExpMatrix) / sqrt(ExpMatrix)

#PearsonIndices <- round (PearsonMatrix, digits=4)

#print(PearsonIndices)



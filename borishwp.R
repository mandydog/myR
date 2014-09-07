dataframe <-read.table("borisdata.R", col.names = c ("q1", "q2", "y", "x", "name") )

c1 <- cut( dataframe$x, breaks = c(0, .5, 1.5, 2, 3.5))
 
newdataframe <- cbind (dataframe, c1)

#print(newdataframe)

mytable <- table(newdataframe$name, newdataframe$c1)

print(mytable)

############################
###   Quetelet Indices   ###
############################

row1 <- c( mytable[1,1], mytable[1,2], mytable[1,3], mytable[1,4])/ sum(mytable[1, 1:4])
row2 <- c( mytable[2,1], mytable[2,2], mytable[2,3], mytable[2,4])/ sum(mytable[2, 1:4])
row3 <- c( mytable[3,1], mytable[3,2], mytable[3,3], mytable[3,4])/ sum(mytable[3, 1:4])

Grandtotal <- sum(mytable[1:3,1:4])

column1sum <- sum(mytable[1:3, 1] )
column2sum <- sum(mytable[1:3, 2] )
column3sum <- sum(mytable[1:3, 3] )
column4sum <- sum(mytable[1:3, 4] )

E1 <- column1sum/Grandtotal  #TsubColj/N
E2 <- column2sum/Grandtotal
E3 <- column3sum/Grandtotal
E4 <- column4sum/Grandtotal

Evector <- c (E1, E2, E3, E4) #vector of column probabilities

Qrow1 <- row1/Evector -1
Qrow2 <- row2/Evector -1
Qrow3 <- row3/Evector -1

Qmatrix <- rbind (Qrow1, Qrow2, Qrow3)

out <- chisq.test(mytable)
print(out)
 
print (Qmatrix)

###########################
####  Pearson Indices  ####
###########################

#First,form the vectors of expected 

Row1Sum <- sum(mytable[1, 1:4]) #TsubRowi
Row1Expected <- Row1Sum*Evector

Row2Sum <- sum(mytable[2, 1:4])
Row2Expected <- Row2Sum * Evector

Row3Sum <- sum(mytable[3, 1:4])
Row3Expected <- Row3Sum * Evector

###Calculate the square roots

SqrtRow1Expected <- sqrt(Row1Expected)
SqrtRow2Expected <- sqrt(Row2Expected)
SqrtRow3Expected <- sqrt(Row3Expected)


###Next, extract observed from mytable

Row1Observed <- c( mytable[1,1], mytable[1,2], mytable[1,3], mytable[1,4])
Row2Observed <- c( mytable[2,1], mytable[2,2], mytable[2,3], mytable[2,4])
Row3Observed <- c( mytable[3,1], mytable[3,2], mytable[3,3], mytable[3,4])

###Obtain the Difference of Observed and Expected

Row1Diff <- Row1Observed - Row1Expected
Row2Diff <- Row2Observed - Row2Expected
Row3Diff <- Row3Observed - Row3Expected

###Calculate Pearson Indices:

PearsonIndexRow1 <- Row1Diff/SqrtRow1Expected
PearsonIndexRow2 <- Row2Diff/SqrtRow2Expected
PearsonIndexRow3 <- Row3Diff/SqrtRow3Expected

PearsonMatrix <- rbind (PearsonIndexRow1, PearsonIndexRow2, PearsonIndexRow3)
print(PearsonMatrix)
#PearsonSqMatrix <- PearsonMatrix**2
#print(PearsonSqMatrix)
#checksum <- sum(PearsonSqMatrix)
#print(checksum)

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
Prow1 <- Row1Sum/Grandtotal
PkPlRow1 <- Evector*Prow1

Row2Sum <- sum(mytable[2, 1:4])
Prow2 <- Row2Sum/Grandtotal
PkPlRow2 <- Evector*Prow2

Row3Sum <- sum(mytable[3, 1:4])
Prow3 <- Row3Sum/Grandtotal
PkPlRow3 <- Evector*Prow3

PkPlMatrix <- rbind(PkPlRow1, PkPlRow2, PkPlRow3)

###Calculate the square roots

Sqrtmatrix <- sqrt(PkPlMatrix)

###Calculate Pearson Indices:

PearsonMatrix <- Qmatrix/Sqrtmatrix
print (PearsonMatrix)

PearsonSqMatrix <- PearsonMatrix**2

checksum <- sum(PearsonSqMatrix)
print(checksum)

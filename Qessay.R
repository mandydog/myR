dataframe <-read.csv("borisdata.csv")

############################
###   Quetelet Indices   ###
############################
######################################## 
rowsums <-  c(sum(dataframe[1, 1:4]), sum(dataframe[2, 1:4]),
              sum(dataframe[3, 1:4]), sum(dataframe[4, 1:4]))

colsums <-  c( sum(dataframe$Low), sum(dataframe$Low.Medium),
               sum(dataframe$High.Medium), sum(dataframe$High) )

Grandtotal <- sum(dataframe) 

Row1Q <- (dataframe[1,] / rowsums[1]) / (colsums /Grandtotal) -1
Row2Q <- (dataframe[2,] / rowsums[2]) / (colsums /Grandtotal) -1
Row3Q <- (dataframe[3,] / rowsums[3]) / (colsums /Grandtotal) -1
Row4Q <- (dataframe[4,] / rowsums[4]) / (colsums /Grandtotal) -1

Matrix <- rbind (Row1Q, Row2Q, Row3Q, Row4Q)
Qmatrix <- round(Matrix, digits=3)

 print (Qmatrix)



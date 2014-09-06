dataframe <-read.table("borisdata.R", col.names = c ("q1", "q2", "y", "x", "name") )

lmfit <- lm (dataframe$y ~ dataframe$x)
summary (lmfit)
summary (lmfit)$r.squared


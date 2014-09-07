
getmonitor <- function(id, directory, summarize = FALSE) {
             
temp = list.files(pattern="*.csv")

 dataframe <- read.csv(file=temp[id],head=TRUE,sep=",")

 if(summarize==TRUE) {
                         summary(dataframe)   }

                                                          }






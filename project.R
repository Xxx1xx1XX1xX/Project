library(openxlsx)
library(stringr)
setwd()

data_stud <- read.xlsx('changed_names.xlsx')




#функция для изменения столбца Hours_on_social_networks(Егор)
f <- function(A){
  for(i in 1:length(A)){
    if(str_length(A[i])>=3){
      A[i] <- substr(A[i],start = str_length(A[i])-str_length(A[i])+1,stop = str_length(A[i])-str_length(A[i])+3)
      if(A[i]=='~6ч'){
        A[i] <- '6'
      } else{
        A[i] <- unlist(strsplit(A[i],'ч'))
        A[i] <- as.integer(mean(as.numeric(unlist(strsplit(A[i],'-')))))
      }
      if(is.na(A[i])==TRUE){
        A[i] <- 1
      }
    }
  }
  return(as.numeric(A))
}



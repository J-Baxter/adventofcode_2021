# Advent Calendar 2021
# Day 3
library(tidyverse)

options(scipen = 999)
prob_3 <- read.table(file = './data/input_day3')
prob3_mat <-  sapply(as.character(prob_3$V1), str_pad,width = 12, side = 'left', pad = 0) %>%
  strsplit(.,"") %>% 
  do.call(rbind,.)

most_common <- c()
for (i in 1:ncol(prob3_mat)){
  most_common[i] <- table(prob3_mat[,i]) %>% 
    sort(.,decreasing=TRUE) %>% 
    names() %>%
    `[[` (1)
}

least_common <- c()
for (i in 1:ncol(prob3_mat)){
  least_common[i] <- table(prob3_mat[,i]) %>% 
    sort(.,decreasing=FALSE) %>% 
    names() %>%
    `[[` (1)
}


gamma_int <- paste(most_common,collapse="")  %>% strtoi(.,base = 2)
epsilon_int <- paste(least_common,collapse="") %>% strtoi(.,base = 2)
gamma_int * epsilon_int #3923414


oxygen <- prob3_mat
for(i in 1:12){
  if(length(oxygen)>12){
    tabs <- table(oxygen[,i])
    remaining <- dim(oxygen)[1]
    
    if(tabs[1] != tabs[2]){
      mc <- tabs %>% 
        sort(.,decreasing=TRUE) %>% 
        names() %>%
        `[[` (1)
      newoxygen <- oxygen[which(oxygen[,i]==mc),]
    }else{
      newoxygen <- oxygen[which(oxygen[,i]== '1'),]
    }
  }
  oxygen <- newoxygen
  
}

oxygen_int <- paste(oxygen,collapse="")  %>% strtoi(.,base = 2)

co2 <- prob3_mat
for(i in 1:12){
  if(length(co2)>12){
    tabs <- table(co2[,i])
    if(tabs[1] != tabs[2]){
      lc <- tabs %>% 
        sort(.,decreasing=FALSE) %>% 
        names() %>%
        `[[` (1)
      newco2 <- co2[which(co2[,i]==lc),]
    }else{
      newco2 <- co2[which(co2[,i]== '0'),]
    }
    co2 <- newco2
  }
  
}

co2_int <- paste(co2,collapse="")  %>% strtoi(.,base = 2)

co2_int * oxygen_int #5852595
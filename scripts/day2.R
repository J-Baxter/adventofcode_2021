# Advent Calendar 2021
# Day 2

rob_2 <- read.table(file = './data/input_day2')
grouped <- split.data.frame(prob_2, prob_2$V1)
depth <- sum(grouped$down$V2) - sum(grouped$up$V2) 
forward <- sum(grouped$forward$V2)

forward * depth

aim <- rep(0, nrow(prob_2))

for (i in 2:nrow(prob_2)){
  if(prob_2[i,1] == 'up'){
    aim[i] = aim[i-1] - prob_2[i,2]
  }else if(prob_2[i,1] == 'down'){
    aim[i] = aim[i-1] + prob_2[i,2]
  }else if(prob_2[i,1] == 'forward'){
    aim[i] = aim[i-1] 
  }
}

prob_2$aim <- aim
grouped_p2 <- split.data.frame(prob_2, prob_2$V1)
forward <- sum(grouped_p2$forward$V2)

new_depth <- rep(0, nrow(prob_2))

for (i in 2:nrow(prob_2)){
  if(prob_2[i,1] == 'forward'){
    new_depth[i] = new_depth[i-1] + prob_2[i, 2] * prob_2[i, 3]
  }else{
    new_depth[i] = new_depth[i-1]
  }
}

forward * new_depth[length(new_depth)]



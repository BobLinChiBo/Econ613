library(tidyverse)
library(ggplot2)
library(pracma)
rm(list = ls())
setwd("C:/Users/boblin/Documents/GitHub/Econ613/Games/G1/Game 1")
# Exercise 1
utility <- function(c, theta){
  if(theta != 1){
    result <- c^(1-theta) / (1-theta)
  }
  else{
    result <- log(c)

  }
  return(result)
}
theta <- seq(-5,5, by = 0.1)
result <- matrix(NA, nrow=length(theta))
j=0
for(i in theta){
  j=j+1
  result[j] = utility(50, i)
}
a <- matrix(c(theta,result), nrow = length(theta))
data <- as.data.frame(a)
data %>% ggplot(aes(x=V1,y=V2))+geom_line()

# Exercise 2
v1 <- c(48,48,48,0,40,64,52,16.97)
v2 <- c(40,64,52,16.97,32,80,56,33.94)
v3 <- c(32,80,56,33.94,24,96,60,50.91)
v4 <- c(24,96,60,50.91,16,112,64,67.88)
v5 <- c(16,112,64,67.88,8,120,64,79.2)
v6 <- c(48,48,48,0,42,66,54,16.97)
v7 <- c(42,66,54,16.97,36,84,60,33.94)
v8 <- c(36,84,60,33.94,30,102,66,50.91)
v9 <- c(30,102,66,50.91,24,120,72,67.88)
v10 <- c(24,120,72,67.88,16,128,72,79.2)
v11 <- c(48,48,48,0,38,62,50,16.97)
v12 <- c(38,62,50,16.97,28,76,52,33.94)
v13 <- c(28,76,52,33.94,18,90,54,50.91)
v14 <- c(18,90,54,50.91,8,104,56,67.88)
v15 <- c(8,104,56,67.88,0,112,56,79.2) 
v16 <- c(42,42,42,0,36,60,48,16.97)
v17 <- c(36,60,48,16.97,30,78,54,33.94)
v18 <- c(30,78,54,33.94,24,96,60,50.91)
v19 <- c(24,96,60,50.91,18,114,66,67.88)
v20 <- c(18,114,66,67.88,10,122,66,79.2)
v21 <- c(54,54,54,0,44,68,56,16.97)
v22 <- c(44,68,56,16.97,34,82,58,33.94)
v23 <- c(34,82,58,33.94,24,96,60,50.91)
v24 <- c(24,96,60,50.91,14,110,62,67.88)
v25 <- c(14,110,62,67.88,6,118,62,79.2)

df <- rbind(v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14,v15,
            v16,v17,v18,v19,v20,v21,v22,v23,v24,v25)
df <- as.data.frame(df)
x1 <-  df$V1
y1 <-  df$V2
x2 <-  df$V5
y2 <-  df$V6
expected_diff <- function(theta){
  El1 <- 0.5 * sapply(x1, utility, theta = theta) +  0.5 * sapply(y1, utility, theta = theta)
  El2 <- 0.5 * sapply(x2, utility, theta = theta)+ 0.5 * sapply(y2, utility, theta = theta)
  bifunc <- El1-El2
  return(bifunc)
}

distinct_expected_diff  <- function(theta, i){
  a <- expected_diff(theta)[i]
  return(a)
}
root <- rep(NA, 25)
for(j in 1:25){
  res <- bisect(distinct_expected_diff,a= -6,b = 6,i = j)
  root[j] <- res$root
}
root




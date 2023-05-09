library(tidyverse)
### This is just a script to toy around and see everything works fine
source("SeperatingVaccineEffects/AnalysisFunc.R")
source("SeperatingVaccineEffects/SimData.R")

# prob_S_A is a matrix. Its j-th row gives Pr(S=j|A=0) (first column) and  Pr(S=j|A=1) (second column)
# probs_B_S is a vector. The j-th entry is Pr(B=1|S=j-1)
# probs_Y_AB is a 2X2 matrix.  Pr(Y=1 | A=j, B = k) (rows for A)
prob_S_A0 <- c(0.7, 0.2, 0.1)
prob_S_A1 <- c(0.5, 0.3, 0.2)
probs_B_S <- c(0.2, 0.6, 0.8)
probs_Y_AB <- matrix(nr = 2, nc =2, byrow = T, c(0.1, 0.2, 0.02, 0.05))
simulated_data <- SimData(N_A0 = 10000, N_A1 = 10000, prob_S_A = rbind(prob_S_A0, prob_S_A1), 
        probs_B_S = probs_B_S, probs_Y_AB = probs_Y_AB)


Analysis(dataset = simulated_data, supp_S = 3)

simulated_data %>% as.data.frame %>% filter(A==0 & S==2 & B==1) %>% select(Y) %>% unlist() %>%
  as.numeric %>% mean

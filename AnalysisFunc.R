# dataset is a matrix of four columns with column names A,S,B,Y
# supp_S is the number of possible values for S (at least 2)
# Currrently report VE = 1 - RR 

Analysis <- function(dataset, supp_S)
{
  A <- dataset[,"A"]
  S <- dataset[,"S"]
  B <- dataset[,"B"]
  Y <- dataset[,"Y"]
  ### 
  # Biological effect assuming perfect blinding
  bio_effect <- 1 - mean(Y[A==1])/mean(Y[A==0])
  # estimate E(Y|A,S,B)
  EY_ASB <- matrix(nr = 2*2*supp_S, nc = 4)
  colnames(EY_ASB) <- c("A", "S", "B", "E(Y|A,S,B)")
  EY_ASB[, 1] <- rep(0:1, each = 2*supp_S)
  EY_ASB[, 2] <- rep(rep(0:(supp_S-1), each = 2), 2)
  EY_ASB[, 3] <- rep(rep(0:1), 2*supp_S)
  for(i in 1:(2*2*supp_S))
  {
    a <- EY_ASB[i, 1]
    s <- EY_ASB[i, 2]
    b <- EY_ASB[i, 3]
    EY_ASB[i, 4] <- mean(Y[A==a & S==s & B==b])
  }
  EY_ASB
  #Estimate Pr(S|B)
  ## Plug in the identification formula for E(Y^{a,m})
  #EY_a0m0 <- 
  
}
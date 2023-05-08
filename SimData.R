
###
# prob_S_A is a matrix. Its j-th row gives Pr(S=j|A=0) (first column) and  Pr(S=j|A=1) (second column)
# probs_B_S is a vector. The j-th entry is Pr(B=1|S=j)
# probs_Y_AB is a 2X2 matrix of Pr(Y=1 | A=j, B = k) (rows for A)

## DN: Should we actually simulate potential outcomes?
SimData <- function(N_A0, N_A1, prob_S_A, probs_B_S, probs_Y_AB)
{
  if ((sum(prob_S_A0) != 1) | (sum(prob_S_A0) != 1))
    stop("sum_j Pr(S=j|A=a) should be equal to 1 for a=0,1")
  N <- N_A0 + N_A1
  # Support size of S, assuming ordinal values and "0" indicates no side effects
  supp_S <- ncol(prob_S_A)
  # Simulate treatment
    A <- rep(0:1, times = c(N_A0, N_A1))
  # Simulate the side effects S given treatment A 
  S <- vector(length = N)
  S[A==0] <- sample(0:(supp_S - 1), size = N_A0, replace = T, prob = prob_S_A[1, ])
  S[A==1] <- sample(0:(supp_S - 1), size = N_A1, replace = T, prob = prob_S_A[2, ])
  # Simulate the belief B given the side effects S
  B <- vector(length = N)
  for (j in 0:(supp_S - 1))
  {
    n_Sj <- sum(S==j)
    B[S==j] <- rbinom(n = n_Sj, size = 1, prob = probs_B_S[j+1])
  }
  # Simulate the outcome Y given the belief B and the side effects S
  Y <- vector(length = N)
  n_A0B0 <- sum(A==0 & B==0)
  n_A0B1 <- sum(A==0 & B==1)
  n_A1B0 <- sum(A==1 & B==0)
  n_A1B1 <- sum(A==1 & B==1)
  Y[A==0 & B==0] <- rbinom(n = n_A0B0, size = 1, prob = probs_Y_AB[1, 1])
  Y[A==0 & B==1] <- rbinom(n = n_A0B1, size = 1, prob = probs_Y_AB[1, 2])
  Y[A==1 & B==0] <- rbinom(n = n_A1B0, size = 1, prob = probs_Y_AB[2, 1])
  Y[A==1 & B==1] <- rbinom(n = n_A1B1, size = 1, prob = probs_Y_AB[2, 2])
  return(cbind(A, S, B, Y))
}
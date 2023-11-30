#### VE hypothetical example for Stensrud, Nevo Obolski (Epidemiology, 2024)
### Modeled after Cowling et al. 2012 paper
set.seed(314)

CalcRisks <- function(risk_a0m0, ve_tot, ve_m0, ve_m1,
                      prS_a1, prS_a0, prB_s1, prB_s0)
{
  risk_a1m1 <- risk_a0m0*(1 - ve_tot)
  risk_a1m0 <- risk_a0m0*(1 - ve_m0)
  risk_a0m1 <- risk_a1m1/(1 - ve_m1)
  risk_a0mMinus1 <- risk_a0m1* ((1 - prS_a0)*prB_s0 + prS_a0*prB_s1) +
                    risk_a0m0* ((1 - prS_a0)*(1 - prB_s0) + prS_a0*(1 - prB_s1))
  risk_a1mMinus1 <- risk_a1m1* ((1 - prS_a1)*prB_s0 + prS_a1*prB_s1) +
    risk_a1m0* ((1 - prS_a1)*(1 - prB_s0) + prS_a1*(1 - prB_s1))
  return(list(risk_a0m0 = risk_a0m0, risk_a1m0 = risk_a1m0, risk_a0m1 = risk_a0m1, risk_a1m1 = risk_a1m1,
              risk_a0mMinus1 = risk_a0mMinus1, risk_a1mMinus1 = risk_a1mMinus1))
}
##### Setup ###
risk_a0m0 <- 0.1395
ve_tot <- 0.3; ve_m1 <- 0.6; ve_m0 <- 0.4
prS_a1 <- 0.5; prS_a0 <- 0.21
prB_s1 <- 0.7; prB_s0 <- 0.18

all_risks <- CalcRisks(risk_a0m0 = risk_a0m0, ve_tot = ve_tot, ve_m0 = ve_m0, ve_m1 = ve_m1,
          prS_a1 = prS_a1, prS_a0 = prS_a0, prB_s1 = prB_s1, prB_s0 = prB_s0)

tab_all_risks <- matrix(nr = 3, nc = 2, byrow = F, 
                        c(all_risks$risk_a0mMinus1, all_risks$risk_a0m0, all_risks$risk_a0m1,
                          all_risks$risk_a1mMinus1, all_risks$risk_a1m0, all_risks$risk_a1m1))
rownames(tab_all_risks) <- c("m = -1", "m = 0", "m = 1")
colnames(tab_all_risks) <- c("a = 0", "a = 1")
tab_all_risks

apply(tab_all_risks, 1, function(x) {1 - x[2]/x[1]})



# Sample size From the trial
n_A1 <- 479
n_A0 <- 317
n_sample <- n_A0 + n_A1

n_sim <- 1000
est_ve_minus1 <- est_ve_S0 <- est_ve_m0 <- est_ve_m1 <- est_ve_tot <- est_ve_S1 <- vector(length = n_sim)
est_risk_a0m0 <- est_risk_a0m1 <- est_risk_a1m0 <- est_risk_a1m1 <- vector(length = n_sim)

for(i in 1:n_sim)
{
A <- B <- S <- Y <- vector(length = n_sample)
A[1:n_A0] <- 0
A[(n_A0 + 1):n_sample] <- 1
# Simulate S|A
S[A==0] <- rbinom(n = n_A0, size = 1, prob = prS_a0)
S[A==1] <- rbinom(n = n_A1, size = 1, prob = prS_a1)
n_S0 <- sum(S==0)
n_S1 <- sum(S==1)
# Simulate B|S
B[S==0] <- rbinom(n = n_S0, size = 1, prob = prB_s0)
B[S==1] <- rbinom(n = n_S1, size = 1, prob = prB_s1)
n_A0B0 <- sum(A==0 & B==0)
n_A0B1 <- sum(A==0 & B==1)
n_A1B0 <- sum(A==1 & B==0)
n_A1B1 <- sum(A==1 & B==1)
# Simulate Y|A, B
Y[A==0 & B==0] <- rbinom(n = n_A0B0, size = 1, prob = all_risks$risk_a0m0)
Y[A==0 & B==1] <- rbinom(n = n_A0B1, size = 1, prob = all_risks$risk_a0m1)
Y[A==1 & B==0] <- rbinom(n = n_A1B0, size = 1, prob = all_risks$risk_a1m0)
Y[A==1 & B==1] <- rbinom(n = n_A1B1, size = 1, prob = all_risks$risk_a1m1)  

# Estimates; not that we assume S is not observed
est_ve_minus1[i] <- 1 - mean(Y[A==1])/mean(Y[A==0])
est_ve_S0[i] <- 1 - mean(Y[A==1 & S==0])/mean(Y[A==0 & S==0])
est_ve_S1[i] <- 1 - mean(Y[A==1 & S==1])/mean(Y[A==0 & S==1])
est_risk_a0m0[i] <- mean(Y[A==0 & S==0 & B==0])*mean(S[A==0]==0) + mean(Y[A==0 & S==1 & B==0])*mean(S[A==0]==1)
est_risk_a0m1[i] <- mean(Y[A==0 & S==0 & B==1])*mean(S[A==0]==0) + mean(Y[A==0 & S==1 & B==1])*mean(S[A==0]==1)
est_risk_a1m0[i] <- mean(Y[A==1 & S==0 & B==0])*mean(S[A==0]==0) + mean(Y[A==1 & S==1 & B==0])*mean(S[A==0]==1)
est_risk_a1m1[i] <- mean(Y[A==1 & S==0 & B==1])*mean(S[A==0]==0) + mean(Y[A==1 & S==1 & B==1])*mean(S[A==0]==1)
est_ve_m0[i] <- 1 - est_risk_a1m0[i]/est_risk_a0m0[i]
est_ve_m1[i] <- 1 - est_risk_a1m1[i]/est_risk_a0m1[i]
est_ve_tot[i] <- 1 - est_risk_a1m1[i]/est_risk_a0m0[i]
}

# Table 2
# Estimated
c(round(mean(est_ve_minus1), 3),
round(mean(est_ve_m0), 3),
round(mean(est_ve_m1), 3),
round(mean(est_ve_tot), 3),
round(mean(est_ve_S0), 3),
round(mean(est_ve_S1), 3))
#[1] 0.463 0.380 0.575 0.273 0.446 0.526

#Repeat with larger sample size
n_A1 <- 4790
n_A0 <- 3170
n_sample <- n_A0 + n_A1

n_sim <- 1000
est_ve_minus1 <- est_ve_S0 <- est_ve_m0 <- est_ve_m1 <- est_ve_tot <- est_ve_S1 <- vector(length = n_sim)
est_risk_a0m0 <- est_risk_a0m1 <- est_risk_a1m0 <- est_risk_a1m1 <- vector(length = n_sim)

for(i in 1:n_sim)
{
  A <- B <- S <- Y <- vector(length = n_sample)
  A[1:n_A0] <- 0
  A[(n_A0 + 1):n_sample] <- 1
  # Simulate S|A
  S[A==0] <- rbinom(n = n_A0, size = 1, prob = prS_a0)
  S[A==1] <- rbinom(n = n_A1, size = 1, prob = prS_a1)
  n_S0 <- sum(S==0)
  n_S1 <- sum(S==1)
  # Simulate B|S
  B[S==0] <- rbinom(n = n_S0, size = 1, prob = prB_s0)
  B[S==1] <- rbinom(n = n_S1, size = 1, prob = prB_s1)
  n_A0B0 <- sum(A==0 & B==0)
  n_A0B1 <- sum(A==0 & B==1)
  n_A1B0 <- sum(A==1 & B==0)
  n_A1B1 <- sum(A==1 & B==1)
  # Simulate Y|A, B
  Y[A==0 & B==0] <- rbinom(n = n_A0B0, size = 1, prob = all_risks$risk_a0m0)
  Y[A==0 & B==1] <- rbinom(n = n_A0B1, size = 1, prob = all_risks$risk_a0m1)
  Y[A==1 & B==0] <- rbinom(n = n_A1B0, size = 1, prob = all_risks$risk_a1m0)
  Y[A==1 & B==1] <- rbinom(n = n_A1B1, size = 1, prob = all_risks$risk_a1m1)  
  
  # Estimates; note that we assume S is observed
  est_ve_minus1[i] <- 1 - mean(Y[A==1])/mean(Y[A==0])
  est_ve_S0[i] <- 1 - mean(Y[A==1 & S==0])/mean(Y[A==0 & S==0])
  est_ve_S1[i] <- 1 - mean(Y[A==1 & S==1])/mean(Y[A==0 & S==1])
  est_risk_a0m0[i] <- mean(Y[A==0 & S==0 & B==0])*mean(S[A==0]==0) + mean(Y[A==0 & S==1 & B==0])*mean(S[A==0]==1)
  est_risk_a0m1[i] <- mean(Y[A==0 & S==0 & B==1])*mean(S[A==0]==0) + mean(Y[A==0 & S==1 & B==1])*mean(S[A==0]==1)
  est_risk_a1m0[i] <- mean(Y[A==1 & S==0 & B==0])*mean(S[A==0]==0) + mean(Y[A==1 & S==1 & B==0])*mean(S[A==0]==1)
  est_risk_a1m1[i] <- mean(Y[A==1 & S==0 & B==1])*mean(S[A==0]==0) + mean(Y[A==1 & S==1 & B==1])*mean(S[A==0]==1)
  est_ve_m0[i] <- 1 - est_risk_a1m0[i]/est_risk_a0m0[i]
  est_ve_m1[i] <- 1 - est_risk_a1m1[i]/est_risk_a0m1[i]
  est_ve_tot[i] <- 1 - est_risk_a1m1[i]/est_risk_a0m0[i]
}

# Table 2
# Estimated
c(round(mean(est_ve_minus1), 3),
  round(mean(est_ve_m0), 3),
  round(mean(est_ve_m1), 3),
  round(mean(est_ve_tot), 3),
  round(mean(est_ve_S0), 3),
  round(mean(est_ve_S1), 3))
#[1] 0.462 0.378 0.580 0.277 0.443 0.532

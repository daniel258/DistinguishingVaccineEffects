#### VE hypothetical example for Stensrud et al (2027)
### Modeled after Cowling et al. 2012 paper

#risk_a0m0, ve_tot, ve_m0, ve_m1,
#prS_a1, prS_a0, prB_s1, prB_s0

CalcRisks <- function(risk_a0m0, ve_tot, ve_m0, ve_m1,
                      prS_a1, prS_a0, prB_s1, prB_s0)
{
  risk_a1m1 <- risk_a0m0*(1 - ve_tot)
  risk_a1m0 <- risk_a0m0*(1 - ve_m0)
  risk_a0m1 <- risk_a1m1/(1 - ve_m1)
  risk_a0mMinus1 <- risk_a0m1* ((1 - prS_a0)*prB_s0 + prS_a0*prB_s1) +
                    risk_a1m0* ((1 - prS_a1)*(1 - prB_s0) + prS_a1*prB_s1)

  return(list(risk_a0m0 = risk_a0m0, risk_a1m0 = risk_a1m0, risk_a0m1 = risk_a0m1, risk_a1m1= risk_a1m1))
}


#### Flu A 
##incidence in pand A (serology)
#incplacA=0.17
#inctreatA=0.09
### Constants
# From the trial
N_A1 <- 479
N_A0 <- 317
Nsample <- N_A1 + N_A0
# Pr(Y=1|A=a)
prob_Y1A1 <- 0.09
prob_Y1A0 <- 0.17
# Here assume that B=1 if and only if 
#pain/soreness of any severity was present (see above)
# Pr(B=1|A=a)
prob_B1A1 <- 0.41 + 0.08 + 0.01
prob_B1A0 <- 0.19 + 0.02
# Not from the trial
thetaB_trt <- 1.25
thetaB_untrt <- 1.75

# Calculate Pr(Y=1|A,B) and put in a matrix
# rows: B values columns: A values

prob_Y_A0B0 <- prob_Y1A0/(1 + (thetaB_untrt -1)*prob_B1A0)
prob_Y_A0B1 <- prob_Y_A0B0*thetaB_untrt
prob_Y_A1B0 <- prob_Y1A1/(1 + (thetaB_trt -1)*prob_B1A1)
prob_Y_A1B1 <- prob_Y_A1B0*thetaB_trt

### Create hypothetical dataset
### SET A
A <- rep(0:1, times = c(N_A0, N_A1))

### Simulate B given A

B <- vector(length = Nsample)
B[A==1] <- rbinom(N_A1, 1, prob_B1A1)
B[A==0] <- rbinom(N_A0, 1, prob_B1A0)

### Simulate Y given A,B
N_A0B0 <- sum(A==0 & B==0) 
N_A0B1 <- sum(A==0 & B==1)
N_A1B0 <- sum(A==1 & B==0)
N_A1B1 <- sum(A==1 & B==1)
N_A0B0 + N_A0B1 + N_A1B0 + N_A1B1

Y <- vector(length = Nsample)
Y[A==0 & B==0] <- rbinom(N_A0B0, 1, prob_Y_A0B0)
Y[A==0 & B==1] <- rbinom(N_A0B1, 1, prob_Y_A0B1)
Y[A==1 & B==0] <- rbinom(N_A1B0, 1, prob_Y_A1B0)
Y[A==1 & B==1] <- rbinom(N_A1B1, 1, prob_Y_A1B1)

mean(Y[A==1])
mean(Y[A==0])
# 1-RR 
1 - prob_Y_A1B1/prob_Y_A0B0
1 - prob_Y_A1B1/prob_Y_A0B1
1 - prob_Y_A1B1/prob_Y_A0B1

mat <- matrix(nr = 2, nc = 2, c(prob_Y_A0B0, prob_Y_A0B1, prob_Y_A1B0, prob_Y_A1B1))
rownames(mat) <- c("M=0", "M=1")
colnames(mat) <- c("A=0", "A=1")
round(mat,2)
1 - prob_Y_A1B1/prob_Y_A0B0
1 - prob_Y_A1B1/prob_Y_A0B1
1 - prob_Y_A1B1/prob_Y_A0B1

# prevtreat=p(B=1|A=1)*p(Y=1|A=1,B=1)+p(B=0|A=1)*p(Y=1|A=1,B=0)=0.03
# prevtreat=0.5*p(Y=1|A=1,B=1)+0.5*p(Y=1|A=1,B=0)=0.03
# prevtreat=0.5*RRunblindingtreat*p(Y=1|A=1,B=0)+0.5*p(Y=1|A=1,B=0)=0.03
# p(Y=1|A=1,B=0)=0.03/(0.5(1+RRunblindingtreat))

 
# RRunblinding=p(Y=1|A=a,B=1)/p(Y=1|A=a,B=0) [name is theta_B in the latax]

# prevplac=p(B=1|A=0)*p(Y=1|A=0,B=1)+p(B=0|A=0)*p(Y=1|A=0,B=0)=0.08
# prevplac=0.21*p(Y=1|A=0,B=1)+0.79*p(Y=1|A=0,B=0)=0.08
# p(Y=1|A=0,B=0)=0.08/(0.21*RRunblindingplac+0.79))




#### constants ####

##incidence in seasonal B (PCR)
incplacB=0.08
inctreatB=0.03
##incidence in pand A (serology)
incplacA=0.17
inctreatA=0.09





##two outcomes flu A and B
##Ntreated=479
##Nplac=317
##incidence in seasonal B (PCR)
#incplacB=0.08
#inctreatB=0.03
##incidence in pand A (serology)
#incplacA=0.17
#inctreatA=0.09

##adverse effects (Table S1)
## pain/soreness
#AEplacmild=0.19; AEplacmod=0.02;AEplacsev=0
#AEtreatmild=0.0.41; AEtreatmod=0.08;AEtreatsev=0.01


# PrevTrtFunc <- function(probBA, probYAB)
# {
#   # ProbBA - vector of length 2 Pr(B=1|A=a)
#   # ProbYBA - matrix 2X2, first col A=0, second A=1,
#   # first row B  Pr(B=1|A=a)
# }
```

```{r}
### how does blinding affect exposure? ####
RRunblindingtreat=1.5
RRunblindingplac=1.5
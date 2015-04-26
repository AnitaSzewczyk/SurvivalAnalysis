##PD 3.1

dane <- read.table("C:\\SurvivalAnalysis\\PD3\\reconstitution.dat", header=TRUE, sep=",")
library(survival)
AG <- coxph(Surv(Time, Status)~Drug + Heifer + cluster(Cowid), data = dane)
summary(AG)

# Call:
#    coxph(formula = Surv(Time, Status) ~ Drug + Heifer + cluster(Cowid), 
#          data = dane)
# 
# n= 200, number of events= 162 
# 
# coef exp(coef) se(coef) robust se     z Pr(>|z|)  
# Drug   0.3371    1.4008   0.1577    0.1370 2.460   0.0139 *
#    Heifer 0.2261    1.2536   0.1703    0.2076 1.089   0.2761  
# ---
#    Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# exp(coef) exp(-coef) lower .95 upper .95
# Drug       1.401     0.7139    1.0709     1.832
# Heifer     1.254     0.7977    0.8346     1.883
# 
# Concordance= 0.572  (se = 0.023 )
# Rsquare= 0.032   (max possible= 1 )
# Likelihood ratio test= 6.44  on 2 df,   p=0.03996
# Wald test            = 7.28  on 2 df,   p=0.0263
# Score (logrank) test = 6.55  on 2 df,   p=0.03779,   Robust = 7.03  p=0.02969
# 
# (Note: the likelihood ratio and score tests assume independence of
#  observations within a cluster, the Wald and robust score tests do not).



## PD 3.2
data <- read.table("C:\\SurvivalAnalysis\\PD3\\culling.dat", header=TRUE, sep=",")
frailty <- coxph(Surv(Time, Status)~Timeassess + LogSCC + frailty(Herd), data = data)
summary(frailty)

# Call:
#    coxph(formula = Surv(Time, Status) ~ Timeassess + LogSCC + frailty(Herd), 
#          data = data)
# 
# n= 13835, number of events= 2729 
# 
# coef     se(coef) se2      Chisq  DF    p      
# Timeassess    0.002466 0.006951 0.006868   0.13   1.0 7.2e-01
# LogSCC        0.069244 0.015549 0.015317  19.83   1.0 8.5e-06
# frailty(Herd)                            357.62 309.7 3.1e-02
# 
# exp(coef) exp(-coef) lower .95 upper .95
# Timeassess     1.002     0.9975    0.9889     1.016
# LogSCC         1.072     0.9331    1.0395     1.105
# 
# Iterations: 8 outer, 30 Newton-Raphson
# Variance of random effect= 0.1302054   I-likelihood = -25493.5 
# Degrees of freedom for terms=   1.0   1.0 309.7 
# Concordance= 0.742  (se = 0.006 )
# Likelihood ratio test= 661.7  on 311.7 df,   p=0

library(tidyverse)
library(pwr)
library(VGAM)

n =20
(boundary.20 <- qt(1-0.05,df = n-1))

n =30
(boundary.30 <- qt(1-0.05,df = n-1))

# 1c
R <- 10000
alpha <- 0.05
n<- 30

error.counter <- 0

for(i in 1:R){
  sample <- rlaplace(n, location= 0, scale =4)
  t.test20 <- t.test(sample[1:20], mu = 0)
  t.20 <- t.test20$statistic
  
  t.test.full <- t.test(sample, mu = 0)
  t.30 <- t.test.full$statistic
  
  # reject null or not at month 20? 
  # else check at month 30
  if (t.20 > boundary.20) {
    error.counter <- error.counter + 1
  } else if (t.30 > boundary.30) {
    error.counter <- error.counter + 1
  }
  
}

# estimate type 1 error rate
(t1.error.rate <- error.counter / R)

################
# Question 2   #
################

R <- 10000 # trials
alpha <- 0.05 # significance level
n =15 # sample size

real.mean10.2 <- 10/(10+2)
real.mean2.10 <- 2/(10+2)
real.mean10.10 <- 10/(10+10)

means <- c(real.mean10.2, real.mean2.10, real.mean10.10)

# each beta counter will track the left, right, and two tailed tests
error.counter10.2 <- c(0,0,0)
error.counter2.10 <- c(0,0,0)
error.counter10.10 <- c(0,0,0)

for(i in 1:R){
  sample.beta1 <- rbeta(n, 10, 2)
  sample.beta2 <- rbeta(n, 2, 10)
  sample.beta3 <- rbeta(n, 10, 10)
  
  ## Beta(10,2) tests 
  t.left1 <- t.test(sample.beta1, mu= means[1], alternative="less")
  t.right1 <- t.test(sample.beta1, mu= means[1], alternative="greater")
  t.both1 <- t.test(sample.beta1, mu= means[1], alternative="two.sided")
  
  error.counter10.2[1] <- error.counter10.2[1] + (t.left1$p.value < alpha)
  error.counter10.2[2] <- error.counter10.2[2] + (t.right1$p.value < alpha)
  error.counter10.2[3] <- error.counter10.2[3] + (t.both1$p.value < alpha)
  
  ## Beta(2,10) tests 
  t.left2 <- t.test(sample.beta2, mu= means[2], alternative="less")
  t.right2 <- t.test(sample.beta2, mu= means[2], alternative="greater")
  t.both2 <- t.test(sample.beta2, mu= means[2], alternative="two.sided")
  
  error.counter2.10[1] <- error.counter2.10[1] + (t.left2$p.value < alpha)
  error.counter2.10[2] <- error.counter2.10[2] + (t.right2$p.value < alpha)
  error.counter2.10[3] <- error.counter2.10[3] + (t.both2$p.value < alpha)
  
  ## Beta(10,10) tests 
  t.left3 <- t.test(sample.beta3, mu= means[3], alternative="less")
  t.right3 <- t.test(sample.beta3, mu= means[3], alternative="greater")
  t.both3 <- t.test(sample.beta3, mu= means[3], alternative="two.sided")
  
  error.counter10.10[1] <- error.counter10.10[1] + (t.left3$p.value < alpha)
  error.counter10.10[2] <- error.counter10.10[2] + (t.right3$p.value < alpha)
  error.counter10.10[3] <- error.counter10.10[3] + (t.both3$p.value < alpha)
  
  
}

# Compute Type I error rates
(type1_error_rate_10_2 <- error.counter10.2 / R)
(type1_error_rate_2_10 <- error.counter2.10 / R)
(type1_error_rate_10_10 <- error.counter10.10 / R)



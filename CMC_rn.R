n <- 10
m <- 1
gamma <- 200000
beta <- seq(0.6,1.5,0.1)
k <- rep(0.25,10)

F_bar <- function(x,k,beta){
  return(exp(-(x/beta)^k))
}

j_prob <- c()

Tprime2 <- numeric(m)

for (i in 1:m){
  
  f_x <- rweibull(n, shape = k, scale = beta)
  
  j_prob <- (F_bar(gamma,k,beta) / sum(F_bar(gamma,k,beta)))
  J <- sample(1:length(j_prob), size = 1, prob = j_prob)
  
  M_n2 <- max(f_x[-J])
  S_n2 <- sum(f_x[-J])
  max_val_cmc2 <- max(M_n2, gamma-S_n2)
  
  Tprime2[i] <- (1/j_prob[J])*F_bar(max_val_cmc2,k[J],beta[J])
  
}

time_elapsed_cmc_2 <- system.time(for (i in 1:m){
  
  f_x <- rweibull(n, shape = k, scale = beta)
  
  j_prob <- (F_bar(gamma,k,beta) / sum(F_bar(gamma,k,beta)))
  J <- sample(1:length(j_prob), size = 1, prob = j_prob)
  
  M_n2 <- max(f_x[-J])
  S_n2 <- sum(f_x[-J])
  max_val_cmc2 <- max(M_n2, gamma-S_n2)
  
  Tprime2[i] <- (1/j_prob[J])*F_bar(max_val_cmc2,k[J],beta[J])
  
})

alpha_cmc_2 <- mean(Tprime2)
var_cmc_2 <- var(Tprime2)
eff_cmc_2 <- alpha_cmc_2 / var_cmc_2

alpha_cmc_2
var_cmc_2
eff_cmc_2
time_elapsed_cmc_2[3]

eff_rn <- (var_cmc_2*time_elapsed_cmc_2[3])/(var_is*time_elapsed_is[3])
eff_rn

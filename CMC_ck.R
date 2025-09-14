n <- 10
m <- 10000
gamma <- 200000
beta <- seq(0.6,1.5,0.1)
k <- rep(0.25,10)

F_bar <- function(x, k, beta) {
  return(exp(-(x / beta)^k))
}

Tprime1 <- numeric(m)

for (i in 1:m){
  
  f_x <- rweibull(n, shape = k, scale = beta)
  T_prime <- 0
  
  for (j in 1:n){
    
    M_n1 <- max(f_x[-j])
    S_n1 <- sum(f_x[-j])
    max_val_cmc1 <- max(gamma-S_n1, M_n1)
    
    T_prime <- T_prime + F_bar(max_val_cmc1, k[j], beta[j])
    
  }
  
  Tprime1[i] <- T_prime
  
}

time_elapsed_cmc_1 <- system.time(for (i in 1:m){
  
  f_x <- rweibull(n, shape = k, scale = beta)
  T_prime <- 0
  
  for (j in 1:n){
    
    M_n1 <- max(f_x[-j])
    S_n1 <- sum(f_x[-j])
    max_val_cmc1 <- max(gamma-S_n1, M_n1)
    
    T_prime <- T_prime + F_bar(max_val_cmc1, k[j], beta[j])
    
  }
  
  Tprime1[i] <- T_prime
  
})

alpha_cmc_1 <- mean(Tprime1)
var_cmc_1 <- var(Tprime1)
eff_cmc_1 <- (alpha_cmc_1*(1-alpha_cmc_1)) / var_cmc_1

alpha_cmc_1
var_cmc_1
eff_cmc_1
time_elapsed_cmc_1[3]

eff_ck <- (var_cmc_1*time_elapsed_cmc_1[3])/(var_is*time_elapsed_is[3])
eff_ck

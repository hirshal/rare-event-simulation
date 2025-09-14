n <- 10
m <- 100000
gamma <- 35
beta <- seq(0.6,1.5,0.1)
k <- c(rep(0.8,5),rep(0.9,5))

cdf <- function(x){
  return(1-exp(-(x/beta)^k))
}

capital_lambda <- function(x,k,beta){
  return((x/beta)^k)
}

indicator_is <- numeric(m)
L_is <- numeric(m)
T_gamma <- numeric(m)

theta <- max(0,1 - (n/capital_lambda(gamma,k,beta)))
beta_prime <- beta / ((1-theta)^(1/k))

for (i in 1:m) {
  
  g_x <- rweibull(n, shape = k, scale = beta_prime)
  
  sum_g_x <- sum(g_x)
  
  indicator_is[i] <- as.numeric(sum_g_x > gamma)
  
  L_is[i] <- prod((1 / (1 - theta)) * exp(-theta * capital_lambda(g_x,k,beta)))
  
  T_gamma[i] <- indicator_is[i] * L_is[i]
  
}

time_elapsed_is <- system.time(for (i in 1:m) {
  
  g_x <- rweibull(n, shape = k, scale = beta_prime)
  
  sum_g_x <- sum(g_x)
  
  indicator_is[i] <- as.numeric(sum_g_x > gamma)
  
  L_is[i] <- prod((1 / (1 - theta)) * exp(-theta * capital_lambda(g_x,k,beta)))
  
  T_gamma[i] <- indicator_is[i] * L_is[i]
  
})

alpha_is <- mean(T_gamma)
var_is <- var(T_gamma)
eff_is <- (alpha_is*(1-alpha_is)) / var_is

alpha_is
var_is
eff_is
time_elapsed_is[3]


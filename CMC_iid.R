n <- 10
m <- 100000
gamma <- 40
beta <- 1
k <- 0.8

cdf <- function(x){
  return(1-exp(-(x/beta)^k))
}

F_bar <- function(x){
  return(1-cdf(x))
}

alpha_vec <- c()

for (i in 1:m){
  
  f_x <- rweibull(n, shape = k, scale = beta)
    
  M_iid <- max(f_x[1:n-1])
  S_iid <- sum(f_x[1:n-1])
      
  alpha_vec[i] <- n * F_bar(max(M_iid, gamma-S_iid))
  
}

alpha_cmc <- mean(alpha_vec)
var_cmc <- var(alpha_vec)
eff_cmc <- alpha_cmc / var_cmc

alpha_cmc
var_cmc
eff_cmc

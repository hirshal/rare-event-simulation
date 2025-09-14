library(ggplot2)

n <- 10
m <- 100000
beta <- seq(0.6,1.5,0.1)
#k <- c(rep(0.8,2),rep(1,8))
k <- rep(0.25,10)

samples_required_is <- function(gamma){
  
  for (i in 1:m) {
    
    theta <- max(0,1 - (n/capital_lambda(gamma,k,beta)))
    beta_prime <- beta / ((1-theta)^(1/k))
    
    g_x <- rweibull(n, shape = k, scale = beta_prime)
    
    sum_g_x <- sum(g_x)
    
    indicator_is[i] <- as.numeric(sum_g_x > gamma)
    
    L_is[i] <- prod((1 / (1 - theta)) * exp(-theta * capital_lambda(g_x,k,beta)))
    
    T_gamma[i] <- indicator_is[i] * L_is[i]
    
  }
  
  alpha_is <- mean(T_gamma)
  var_is <- var(T_gamma)
  
  return(ceiling((1.96^2*var_is)/(0.05^2*alpha_is^2)))
}

samples_required_mc <- function(gamma){
  
  theta <- max(0,1 - (n/capital_lambda(gamma,k,beta)))
  beta_prime <- beta / ((1-theta)^(1/k))
  
  for (i in 1:m) {
    
    g_x <- rweibull(n, shape = k, scale = beta_prime)
    
    sum_g_x <- sum(g_x)
    
    indicator_is[i] <- as.numeric(sum_g_x > gamma)
    
    L_is[i] <- prod((1 / (1 - theta)) * exp(-theta * capital_lambda(g_x,k,beta)))
    
    T_gamma[i] <- indicator_is[i] * L_is[i]
    
  }
  
  alpha_is <- mean(T_gamma)
  
  return(ceiling((1.96^2*alpha_is*(1-alpha_is))/(0.05^2*alpha_is^2)))
  
}

samples_required_cmc <- function(gamma){
  
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
  
  alpha_cmc_1 <- mean(Tprime1)
  var_cmc_1 <- var(Tprime1)
  
  return(ceiling((1.96^2*var_cmc_1)/(0.05^2*alpha_cmc_1^2)))
  
}

samples_required_mc(45)
samples_required_is(45)
samples_required_cmc(45)

#x <- c(10,15,20,25,30,35,40,45,50)
x <- c(10000,250000,50000,75000,100000,125000,150000,175000,200000)

y_mc <- c()
y_is <- c()
y_cmc <- c()

for (i in 1:length(x)){
  y_mc[i] <- samples_required_mc(x[i])
  y_is[i] <- samples_required_is(x[i])
  y_cmc[i] <- samples_required_cmc(x[i])
}

df <- data.frame(gamma = x, y = c(y_is, y_cmc, y_mc), 
                 type = c(rep("Importance Sampling",length(x)),rep("Conditional MC",length(x)),rep("Naive MC",length(x))))

graph <- ggplot(data=df, aes(x=gamma, y=y, color=type)) + geom_line()

graph + scale_y_log10() + xlab("Gamma") + ylab("Computations Required (M*)") + 
  labs(color="MC Type") + theme(legend.position = c(0.88,0.13))

graph1 + geom_line(lwd = 1)
graph2 + geom_line(lwd = 1) 
graph3 + geom_line(lwd = 1) 


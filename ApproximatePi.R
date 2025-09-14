
approximate_pi <- function(n){
  
  x <- runif(n,-1,1)
  y <- runif(n,-1,1)
  
  colours <- c()
  for (i in 1:length(x)){
    distance <- sqrt(x[i]^2+y[i]^2)
    if (distance < 1){
      color <- "red"
    }
    else{
      color <- "black"
    }
    colours <- c(colours,color)
  }
  
  plot(x,y,xlim = c(-1,1),ylim=c(-1,1),pch=20,col=colours)
  
  red_propn <- table(colours)["red"]
  
  pi_hat <- (red_propn / n) * 4
  
  print(c(noquote(n),noquote("sample Monte Carlo Approximation of Pi:"), noquote(pi_hat)))
  
  pi-pi_hat
  
}
  
approximate_pi(10000)

#want to plot pi - pi_hat for increasing n

pi_approx <- function(n){
 
  x <- runif(n, -1, 1)
  y <- runif(n, -1, 1)
  
  in_circle <- 0
  
  for (i in 1:length(x)){
    dist <- sqrt(x[i]**2 + y[i]**2)
    if (dist < 1){
      in_circle <- in_circle + 1
    } 
  }
  
  (in_circle / n) * 4
  
  
}

pi_approx(10000)

plot_pi_hat <- function(n){
  
  pi_hats <- c()
  pi_const <- c()
  
  for (i in seq(n)){
    pi_hats[i] <- pi_approx(i)
    pi_const[i] <- pi
  }
  
  plot(pi_hats,pch=".",xlab = "Number of MC Simulations", ylab="MC Estimate of Pi",ylim=c(0,4))
  lines(pi_const, col="red")
  
}

plot_pi_hat(10000)

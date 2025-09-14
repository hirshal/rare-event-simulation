set.seed(071124)

#first approximate far right-tail probability of standard normal distribution
n <- 1000000
RightTail <- 5

Z <- rnorm(n)

X <- 0 + (Z>RightTail) # indicator function - counts simulations > RightTail

EX <- mean(X)
EX # 0 as expected

exact <- 1-pnorm(RightTail,0,1)
exact

varX <- var(X)
varX

SE <- sqrt(varX) / sqrt(n)

CI <- c(EX-1.96*SE, EX+1.96*SE) #construct 95% confidence interval
CI

# introduce IS

m <- 1000 #100x fewer simulations than standard normal distribution

Z_i <- rnorm(m, RightTail, 1) # let our g(x) be a N(µ, 1) distribution

X_i <- 0 + (Z_i>RightTail) * exp(-RightTail*Z_i + (RightTail^2)/2)
# indicator function multiplied by likelihood function f(x)/g(x)

EX_i <- mean(X_i)
EX_i

varX_i <- var(X_i)
varX_i

SE_i <- stdevX_i / m

CI_i <- c(EX_i-1.96*SE_i, EX_i+1.96*SE_i)
CI_i #more accurate 95% confidence interval of right tail probability
# generated with 1000x fewer simulations! 


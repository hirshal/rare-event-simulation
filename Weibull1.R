set.seed(251124)

#first approximate far right-tail probability of sum of n Weibull distributions
n <- 10
m <- 1000
RightTail <- 10
lambda <- 0.8
k <- 1
X <- c()

for (i in 1:m){
  f_x <- rweibull(n, lambda, k)
  X[i] <- 0 + (sum(f_x)>RightTail) # indicator function - counts simulations > RightTail
}

EX <- mean(X)
EX # naive MC estimator

exact <- 1-pweibull(RightTail,0.8,1)
exact

varX <- var(X)
varX

SE <- sqrt(varX) / sqrt(m)

CI <- c(EX-1.96*SE, EX+1.96*SE) #construct 95% confidence interval
CI

# introduce IS
X_i <- c()
L_i <- c()
F_x <- 1-exp(-(f_x/lambda)^k)
lambda_x <- f_x/(1-F_x)
LAMBDA_x <- -log(1-F_x)


for (i in 1:m){
  Z_i <- rweibull(n, shift, 1)
  X_i[i] <- 0 + (sum(Z_i)>RightTail)
  L_i[i] <- exp(-shift*sum(Z_i) + n*(shift^2)/2)
}

X_is <- X_i*L_i

# indicator function multiplied by likelihood function f(x)/g(x)

EX_is <- mean(X_is)
EX_is

varX_is <- var(X_is)
varX_is

SE_i <- sqrt(varX_is) / sqrt(m)

CI_i <- c(EX_is-1.96*SE_i, EX_is+1.96*SE_i)
CI_i #variance reduced 95% confidence interval of right tail probability

var_red <- varX/varX_is
var_red

#now try and replicate examples from the paper
set.seed(251124)

#first approximate far right-tail probability of sum of n standard normal distributions
n <- 10
m <- 100000
RightTail <- 15
X <- c()

for (i in 1:m){
  Z <- rnorm(n)
  X[i] <- 0 + (sum(Z)>RightTail) # indicator function - counts simulations > RightTail
}

EX <- mean(X)
EX # naive MC estimator

exact <- 1-pnorm(RightTail,0,sqrt(n))
exact

varX <- var(X)
varX

SE <- sqrt(varX) / sqrt(m)

CI <- c(EX-1.96*SE, EX+1.96*SE) #construct 95% confidence interval
CI

# introduce IS
X_i <- c()
L_i <- c()
shift <- RightTail/n


for (i in 1:m){
  Z_i <- rnorm(n, shift, 1) # g(x) is a N(µ/n, 1) distribution (s.t. sum centred around µ)
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


set.seed(141124)

#first approximate far right-tail probability of standard normal distribution
n1 <- 1000
m1 <- 100

RightTail <- 5

generateZ <- function(n){
  v <- c()
  for (i in 1:n){
    v[i] <- rnorm(1)
  }
  return(v)
}

df <- data.frame(x=rep(0,n1))

for (y in 1:m1){
  df[,y] <- generateZ(n1)
}

Z <- c()

for (i in 1:m1){
  Z[i] <- sum(df[,i])
}

indicator <- 0 + (Z > RightTail)

EX <- mean(indicator)
EX # 0 as expected

exact <- 1-pnorm(RightTail,0,1)
exact

varX <- var(indicator)
varX

SE <- sqrt(varX) / sqrt(n1)

CI <- c(EX-1.96*SE, EX+1.96*SE) #construct 95% confidence interval
CI

# introduce IS

generateZ_is <- function(n){
  v <- c()
  for (i in 1:n){
    v[i] <- rnorm(1,RightTail,1)
  }
  return(v)
}

df_is <- data.frame(x=rep(0,n1))

for (i in 1:m1){
  df_is[,i] <- generateZ_is(n1)
}

Z_is <- c()

for (i in 1:m1){
  Z_is[i] <- sum(df_is[,i])
}

L_is <- c()

for(j in 1:m1){
  for (i in 1:n1){
    vector[i] <- exp(-df_is[i,j]*RightTail+(RightTail^2)/2)
  }
  L_is[j] <- prod(vector)
}

indicator_is <- 0 + (Z_is > RightTail)

X_is <- indicator_is * L_is

EX_is <- mean(X_is)
EX_is 

exact <- 1-pnorm(RightTail,0,1)
exact

varX_is <- var(X_is)
varX_is

SE_is <- sqrt(varX_is) / sqrt(n1)

CI_is <- c(EX_is-1.96*SE_is, EX_is+1.96*SE_is) #construct 95% confidence interval
CI_is

#############################
n2 <- 1000 

Z_i <- rnorm(n2, RightTail, 1) # let our g(x) be a N(µ, 1) distribution

X_i <- 0 + (Z_i>RightTail) * exp(-RightTail*Z_i + (RightTail^2)/2)
# indicator function multiplied by likelihood function f(x)/g(x)

EX_i <- mean(X_i)
EX_i

varX_i <- var(X_i)
varX_i

SE_i <- stdevX_i / m

CI_i <- c(EX_i-1.96*SE_i, EX_i+1.96*SE_i)
CI_i #more accurate 95% confidence interval of right tail probability

varX/varX_i




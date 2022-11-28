## HW 9 ## 

library(tidyverse)
# Danny Szydlowski and Tyler Butts # 

## Generate a time series of abundance 
## for a population growing according to the discrete time logistic growth model

r = 0.2 
K = 100
time = seq(from=1,to=50)
N = array(dim = c(1, length(time))); N[1] = 5
  for (t in time[2:length(time)]){
    N[t] = N[t-1] + r*N[t-1]*(1-N[t-1]/K)
  }
N

############### Q1 Plot the Data ##############################
plot(x = seq(1, 50), 
          y = N,
          type = 'o', pch = 20, cex = 2)

x = seq(1, 50)

############## Q2 - generate a model predicted time series of abundance #########################
# Pretend you don't know the true r or K and adjust until model fits data # 
dat = data.frame(x = x, y = as.vector(N))

plot(x, y, data = dat)
y = as.vector(N)

# Approximate 
# model 1
r = 0.5 
f = function(x, r, K, N0) {K / (1 + (K/N0 - 1) * exp(-r*x))}
plot(x, y)


pstart = c(r=r, K=max(y), N0=y[1])
mod1 = nls(y ~ f(x, r, K, N0), start = pstart, trace = T)
# model converged 
summary(mod1)
lines(x, f(x, r=r, K=max(y), N0=y[1]), col = 'blue')

# model 2 
r = 2

pstart = c(r=r, K=max(y), N0=y[1])
mod2 = nls(y ~ f(x, r, K, N0), start = pstart, trace = T)
lines(x, f(x, r=r, K=max(y), N0=y[1]), col = 'green')

# model converged 
summary(mod2)

# model 3
r = .1

pstart = c(r=r, K=max(y), N0=y[1])
mod3 = nls(y ~ f(x, r, K, N0), start = pstart, trace = T)
lines(x, f(x, r=r, K=max(y), N0=y[1]), col = 'purple')

# model 4 (true value )
r = .2

pstart = c(r=r, K=max(y), N0=y[1])
mod4 = nls(y ~ f(x, r, K, N0), start = pstart, trace = T)
lines(x, f(x, r=r, K=max(y), N0=y[1]), col = 'black')

# model converged 
summary(mod4)

################### Q3 ##############################


# Function to calculate the negative log-likelihood
calc_nll <- function(r, K, N_obs){
  n_pred <- N_obs[t] = N_obs[t-1] + r*N_obs[t-1]*(1-N_obs[t-1]/K)
  nll <- calc_nll(N_obs, y_pred, sigma)
}

# Setting grid search around a r of 0.2 and a K of 100 # 
r = seq(0, 0.5, 1) 
K = seq(50, 100, 150)
N_obs = N

# True parameters
r <- 0.2
K <- 100


# Input Data 
r = 0.2 
K = 100
time = seq(from=1,to=50)
N = array(dim = c(1, length(time))); N[1] = 5
for (t in time[2:length(time)]){
  N[t] = N[t-1] + r*N[t-1]*(1-N[t-1]/K)
}
y_obs = as.vector(N)
y_obs


# Function to calculate the negative log-likelihood
calc_nll <- function(r, K, N_obs){
  n_pred <- N_obs[t] = N_obs[t-1] + r*N_obs[t-1]*(1-N_obs[t-1]/K)
  nll <- calc_nll(N_obs, y_pred, sigma)
}

# Objective function
obj_func <- function(par1, y_obs, y_pred){
  r <- par1[1]
  K <- par1[2]
  y_pred <- N[t-1] + r*N[t-1]*(1-N[t-1]/K)
  sigma = length(y_obs)
  nll <- calc_nll(y_obs, y_pred, sigma)
}

sigma = 3
y_pred = NA
# Estimate parameters using optim()
optfit <- optim(par=c(0,1,2), fn=obj_func(par = c(0,1,2)), y_obs = y_obs, y_pred = y_pred)
optcoefs <- optfit$par1

########################### Q4 ######################################

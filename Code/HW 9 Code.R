## HW 9 ## 

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


############## Q2 - generate a model predicted time series of abundance #########################
# Pretend you don't know the true r or K and adjust until model fits data # 
install.packages('latex2exp')
library(latex2exp)

# Base Plot # 
plot(x = seq(1, 50), 
     y = N, main = 'Estimationg non-linear model between x and y',
     type = 'o', pch = 20, cex = 2)

x = as.vector(seq(1,50))
y = as.vector(N)

lm=lm(y~x)
abline(lm$coefficients, col = 'navy', lwd = 4)


x2=x*x
lm2=lm(y~x+x2)

xbar=sort(x)
xbar2=xbar*xbar

ybar=lm2$coefficients[1]+xbar*lm2$coefficients[2]+xbar2*lm2$coefficients[3]

lines(y=ybar, x=xbar, pch=19, col="orange", lwd=4) 

xlog=log10(x)
lm3=lm(y~x+xlog)

xbar=sort(x)
xbarLog=log10(xbar)

ybar=lm3$coefficients[1]+xbar*lm3$coefficients[2]+xbarLog*lm3$coefficients[3]

lines(y=ybar, x=xbar, pch=19, col="maroon", lwd=4) 

legend("topleft", legend=c(TeX('$y=\\beta_0+\\beta_1 x+\\epsilon$'),
                           TeX('$y=\\beta_0+\\beta_1 x+\\beta_2 x^2+\\epsilon$'),
                           TeX('$y=\\beta_0+\\beta_1 x+\\beta_2 log(x)+\\epsilon$')),
       col=c("navy","orange","maroon"), 
       lty=1, box.lty=0, cex=.5)

# None of the fits look particularly good - will now try 

################### Q3 ##############################


# Function to calculate the negative log-likelihood
# Nicer function for calculating NLL
N = as.vector(N)
N

calc_nll <- function(r, K, sigma, N){
  n_pred <- N[t] = N[t-1] + r*N[t-1]*(1-N[t-1]/K)
  nll <- calc_nll(N, y_pred, sigma)
}
calc_nll 

# Estimate NLL for varying slopes # 
r = seq(0, 0.5, 1) 
K = seq(50, 100, 150)

# setting grid search around a slope of 0.8 and intercept of 5
b1_slope <- seq(0.001, 1.6, 0.1)
b0_intercept <- seq(0, 10, 0.1)
# add a sigma term here, error around .06 from Q1 model
sigma_value = seq(0.001, 1, 0.01)
nll_array <- array(data=NA, dim=c(length(b0_intercept), length(b1_slope), length(sigma_value)), 
                   dimnames=list(b0_intercept, b1_slope, sigma_value))

# Begin grid search: 
i <- 1
j <- 1
k <- 1
for(i in 1:length(b0_intercept)){
  b0_curr <- b0_intercept[i]
  for(j in 1:length(b1_slope)){
    b1_curr <- b1_slope[j]
    for(k in 1:length(sigma_value)){
      sigma <- sigma_value[k]
      predValue <- b0_curr + b1_curr*x
      nll <- negll_calc(y, predValue, sigma)
      nll_array[i,j,k] <- nll
    }
  }
}

# Best params
min_nll_index <- which(nll_array==min(nll_array), arr.ind=T)
min_nll_coefs <- c(b0_intercept[min_nll_index[1]], 
                   b1_slope[min_nll_index[2]],
                   sigma_value[min_nll_index[2]])
min_nll_coefs
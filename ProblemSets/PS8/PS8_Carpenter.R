library(nloptr)
library(tidyverse)
library(stargazer)

# ----------------------------------------------------------------------------------

# SETUP MATRIX
# ----------------------------------------------------------------------------------
  set.seed(100)
  N     <- 100000 # Rows
  K     <- 10     # Columns
  sigma <- 0.5
  
  
# CREATE NORMALLY DISTRIBUTED MATRIX
# ----------------------------------------------------------------------------------
  X     <- matrix(rnorm(N*K,mean=0,sd=sigma),N,K)
  X[,1] <- 1       # first column now all = 1's
  eps   <- rnorm(N,mean=0,sd=sigma^2)
  beta  <- c(1.5, -1, -0.25, 0.75, 3.5, -2, 0.5, 1, 1.25, 2)
  Y     <- X%*%beta + eps

  
# 1. OLS ESTIMATE
# ----------------------------------------------------------------------------------
  est.OLS <- lm(Y~X -1)
  result1 <- est.OLS 
  out1 <- print(est.OLS)

  
# 2. GRADIENT DESCENT
# ----------------------------------------------------------------------------------
  ## Create Objective Function
    objfun <- function(beta,y,X) 
      {
        return (sum((y-X%*%beta)^2))
      }
  
  ## Create Gradient Function
    gradient <- function(beta,y,X) 
      {
        return ( as.vector(-2*t(X)%*%(y-X%*%beta)))
      }
    
  ## Parameters for Algorithm
    options <- list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"=1.0e-6,"maxeval"=1e3)
  
  ## Optimization
    result2 <- nloptr(x0 = beta, eval_f = objfun, eval_grad_f = gradient, opts = options, y=Y, X=X)
    print(result2)
    
    
# 3. NELDER MEAD 
# ----------------------------------------------------------------------------------
  ## Create Objective Function
    objfun  <- function(theta,y,X) 
      {
        beta    <- theta[1:(length(theta)-1)]
        sig     <- theta[length(theta)]
        loglike <- -sum( -.5*(log(2*pi*(sig^2)) + ((y-X%*%beta)/sig)^2)) # negative because it is minimizing 
        return (loglike)
      }
  
  ## Initital Values
    theta0 <- runif(dim(X)[2]+1) #start at uniform random numbers equal to number of coefficients
    theta0 <- append(as.vector(summary(est.OLS)$coefficients[,1]),runif(1))
    
  ## Parameters for Algorithm
    options <- list("algorithm"="NLOPT_LN_NELDERMEAD","xtol_rel"=1.0e-6,"maxeval"=1e4)
    
  ## Optimization
    result3 <- nloptr(x0 = theta0, eval_f = objfun, opts = options, y=Y, X=X)
    print(result3)
    betahat  <- result3$solution[1:(length(result$solution)-1)]
    sigmahat <- result3$solution[length(result$solution)]
    

# 4. GRADIENT DESCENT (B.MLE)
# ----------------------------------------------------------------------------------
  ## Objective Function
    objfun  <- function(theta,y,X) 
      {
        beta    <- theta[1:(length(theta)-1)]
        sig     <- theta[length(theta)]
        loglike <- -sum( -.5*(log(2*pi*(sig^2)) + ((y-X%*%beta)/sig)^2) ) 
        return (loglike)
      }
    
    ## Gradient of the objective function
      gradient <- function (theta,y,X) 
        {
          grad     <- as.vector(rep(0,length(theta)))
          beta     <- theta [1:(length(theta)-1)]
          sig      <- theta [length(theta)]
          grad[1:(length(theta)-1)] <- -t(X)%*%(y - X%*%beta)/(sig^2)
          grad[length(theta)]       <- dim(X)[1]/sig-crossprod (y-X%*%beta)/(sig^3)
          return ( grad )
        }

    ## Initital Values
      theta0 <- runif(dim(X)[2]+1) #start at uniform random numbers equal to number of coefficients
      theta0 <- append(as.vector(summary(est.OLS)$coefficients[,1]),runif(1))
      
    ## Parameters for Algorithm
      options <- list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"=1.0e-6,"maxeval"=1e4)
      
    ## Optimization
      result4 <- nloptr( x0=theta0,eval_f=objfun,eval_grad_f=gradient,opts=options,y=Y,X=X)
      print(result4)
      betahat  <- result4$solution[1:(length(result$solution)-1)]
      sigmahat <- result4$solution[length(result$solution)]
     
       
# 5. LM FUNCTION
# ----------------------------------------------------------------------------------    
  est.lm <- lm(Y ~ X)
  result5 <- summary(est.lm)
  print(result5)

  
# STARGAZER OUTPUT
# ----------------------------------------------------------------------------------    

  stargazer(beta,
            out1,
            print(result2$x0),
            print(result3$x0),
            print(result4$x0),
            print(result5$residuals))
    
    
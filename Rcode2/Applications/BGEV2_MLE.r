

#*************************************************************************Description*************************************************************************************************#
# MLE for BGEV regression with xi/= zero
# Reference
# Authors et al. (2024). Title.
#***ARGUMENTS***#
# y - response variable.
# X -  regressor matrix for the median model.
# method - method passed to optim. Default is BFGS.
# maxit - maximum number of iterations. Default is 200.
#**************************************************************************************************************************************************************************************#


#------------------------->FUNCTION STARTS HERE<--------------------------------#
MLE_BGEV2 <- function(y, X, method="BFGS", maxit=200){
#****Packages required****#
if(!suppressWarnings(require(evd))){suppressWarnings(install.packages("evd"));suppressWarnings(library(evd))}#used for GEV functions
if(!suppressWarnings(require(EnvStats))){suppressWarnings(install.packages("EnvStats"));suppressWarnings(library(EnvStats))}#used for
if(!suppressWarnings(require(VGAM))){suppressWarnings(install.packages("VGAM"));suppressWarnings(library(VGAM))}#used for gev estimates
if(!suppressWarnings(require(BBmisc))){suppressWarnings(install.packages("BBmisc"));suppressWarnings(library(BBmisc))} #used for is.error function

#**********density function**********#
dbgev2 <- function(y, m, sigma, xi, delta){ #xi dif 0
  # Compute auxiliary variables:
  mu <-  m - sign((sigma/xi)*((-log(0.5))^(-xi) -1))*(abs((sigma/xi)*((-log(0.5))^(-xi) -1))^(1/(delta+1)))# m is median  
  T      <- (y-mu)*(abs(y-mu)^delta)
  derivate_T <- (delta + 1)*(abs(y-mu)^delta)
  # Compute density points
  pdf    <- dgev(T, loc=0, scale=sigma, shape=-xi)*derivate_T
  # Return Value
  return(pdf)
}

#*************Function to be maximized**********************#
log_likelihood2 <- function(theta) 
{
  kk1 <- ncol(X)
  beta <- theta[1:kk1]
  sigma <- theta[(kk1+1.0)]
  xi <- theta[kk1+2.0]
  delta <- theta[(kk1+3.0)] 
  eta <- as.vector(X%*%beta)                                                 
  m <- eta #identity link                            	
  log_lik <- sum(log(dbgev2(y, m, sigma, xi, delta)))#function to be maximized  
  return(log_lik)
}


#----------------------------------------------------------------------------------------------------------------#
#------------------------------------->Estimation procedure starts here<-----------------------------------------#
#----------------------------------------------------------------------------------------------------------------#
   
kk1 <- ncol(X);  n <- nrow(X)
fitgev <-  suppressWarnings(vglm(y ~ X[,2:kk1], gevff(perc = NULL))) #starting values via GEV regression
estimates <- as.numeric(coef(fitgev)); beta1hat <- estimates[1]; sigmahat <- exp(estimates[2]); xihat <- exp(estimates[3])-0.5;
betascovhat <- estimates[4:(kk1+2)]
betahat <- c(beta1hat, betascovhat)
thetaStart <- c(betahat, sigmahat, xihat, 0) #starting values

 #-------->maximization<------------#
 res <- suppressWarnings(tryCatch(optim(thetaStart, log_likelihood2, hessian = T, control=list(fnscale=-1,maxit=maxit), gr = NULL, method=method),
 error=function(e) {e}))
      if(is.error(res)){#starts else in case of error in the estimation procedure
           results <- list(convergence="no")
      }
      else
            {
                               est_bgev <- res$par
                               log.lik_bgev <- res$value
                               V_bgev <- -solve(res$hessian)	
                               se_bgev <-  suppressWarnings(t(sqrt(diag(V_bgev))))	 
                               z_bgev <- as.matrix(est_bgev/se_bgev)
                               pvalue_bgev <- apply(z_bgev,2,function(x)(2.0*(1.0-pnorm(mean(abs(x))))))
                               betahat_bgev <- est_bgev[1:kk1] 
                               sigmahat_bgev <- est_bgev[(kk1+1.0)]
                               xihat_bgev <- est_bgev[(kk1+2.0)]
                               deltahat_bgev <- est_bgev[(kk1+3.0)]   
                                                             
                               results <- list(beta = betahat_bgev, sigma = sigmahat_bgev, xi = xihat_bgev, delta = deltahat_bgev, se_beta = se_bgev[1:kk1],
                               se_sigma = se_bgev[kk1+1.0], se_xi = se_bgev[kk1+2.0], se_delta = se_bgev[kk1+3.0], z_beta = z_bgev[1:kk1], z_sigma = z_bgev[kk1+1.0],
                               z_xi = z_bgev[kk1+2.0], z_delta = z_bgev[kk1+3.0], pvalue_beta = pvalue_bgev[1:kk1], pvalue_sigma = pvalue_bgev[kk1+1.0],
                               pvalue_xi = pvalue_bgev[kk1+2.0], pvalue_delta = pvalue_bgev[kk1+3.0],  Initial_Values = thetaStart, convergence= res$convergence)

            }
                               return(results)

     }# ends function
#------------------------->FUNCTION ENDS HERE<--------------------------------#
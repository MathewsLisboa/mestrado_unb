#*************************************************************************Description*************************************************************************************************#
#Create normal probability plots of quantile residuals with simulated envelope to assess the goodness of BGEV regression (xi /=0) fit under MLE 
#***ARGUMENTS***#
# y - response variable.
# X -  regressor matrix for the median submodel.
# theta - vector of parameter estimates (via MLE) of the observed sample. 
# main.title - main title for the plot. Default is "Envelope".
# faixa.fixed - range of residuals values (optional). Default is NULL.
# labels.fixed - labels of the observations used to create the plot (optional). Default is NULL meaning that all observations are used.
#**************************************************************************************************************************************************************************************#

envelope_BGEV2 <- function(y, X, theta, main.title = '', faixa.fixed = NULL, labels.fixed = NULL) { 
source("BGEV2_MLE.r")
B <- 100; #number of replicates
kk1 <- ncol(X);  n <- nrow(X)
#***parameters for parametric bootstrap***#
beta_p <- theta[1:kk1]
sigma_p <- theta[kk1+1.0]    
xi_p <- theta[kk1+2.0] 	   
delta_p <- theta[kk1+3.0] 
etahat_p <- X%*%beta_p
m_p <- etahat_p
#************************************************************#

pbgev2 <- function(y, m, sigma, xi, delta){ 
  # Compute auxiliary variables:
  mu <-  m - sign((sigma/xi)*((-log(0.5))^(-xi) -1))*(abs((sigma/xi)*((-log(0.5))^(-xi) -1))^(1/(delta+1)))# m is median  
  T      <- (y-mu)*(abs(y-mu)^delta)
  # Compute 
  cdf    <- pgev(T, loc=0, scale=sigma, shape=-xi)
  # Return Value
  return(cdf)
}
qbgev2   <- function(p, m, sigma, xi, delta){
  # Compute distribution points according to their sign
  mu <-  m - sign((sigma/xi)*((-log(0.5))^(-xi) -1))*(abs((sigma/xi)*((-log(0.5))^(-xi) -1))^(1/(delta+1)))# m is median  
  quantile <- sign(qgevd(p, 0, sigma, -xi))*(  (abs(qgevd(p, 0, sigma, -xi)))^(1/(delta + 1))  ) + mu
  # Return Value
  return(quantile)
}
rbgev2 <- function(n, m, sigma, xi, delta){
  # Compute auxiliary variables:
  U <- runif(n)
  # Compute random numbers
  mu <-  m - sign((sigma/xi)*((-log(0.5))^(-xi) -1))*(abs((sigma/xi)*((-log(0.5))^(-xi) -1))^(1/(delta+1)))# m is median  
  rnumber <- qbgev2(U, m, sigma, -xi, delta)
  # Return Value
  return(rnumber)
}

Menvelope_rq <- matrix(numeric(0),nrow=n,ncol=B)

#------------> quantile residuals for the observed sample<--------------#

rq <- qnorm(pbgev2(y, m_p, sigma_p, xi_p, delta_p))

suppressWarnings(set.seed(c(1994,1991), kind="Marsaglia-Multicarry")) #to generate same plot

    for(j in 1:B){		
        ygen <- rbgev2(n,  m_p, sigma_p, xi_p, delta_p)
        fit_b <- MLE_BGEV2(y=ygen, X=X, method="BFGS")
            while(fit_b$convergence=="no"){# in case of error in the estimation procedure
              ygen <- rbgev2(n,  m_p, sigma_p, xi_p, delta_p)
              fit_b <- MLE_BGEV2(y=ygen, X=X, method="BFGS")
              print(fit_b$convergence)
            }
        print(j)
        rq_b <- qnorm(pbgev2(ygen, X%*%as.numeric(fit_b$beta), fit_b$sigma, fit_b$xi, fit_b$delta))
        Menvelope_rq[,j] = rq_b
    }
Menvelope_rq <- apply(Menvelope_rq,2,sort);          
res_rq <-    rq;    
res_min_rq  <-    as.numeric(t(apply(t(Menvelope_rq), 2,quantile, probs =0.05)));         
res_mean_rq <-    as.numeric(t(apply(t(Menvelope_rq), 2,quantile, probs =0.5)));                              
res_max_rq  <-    as.numeric(t(apply(t(Menvelope_rq), 2,quantile, probs =0.95)));           
faixa <- range(res_rq,res_min_rq,res_max_rq)
if(is.vector(faixa.fixed)) faixa <- faixa.fixed
if(is.vector(labels.fixed)) labels <- labels.fixed
par(mar=c(5.0,5.0,4.0,2.0))
v <- qqnorm(res_rq, main=main.title, xlab="Normal quantiles", ylab="Residuals", ylim=faixa, pch=16, cex=1.0, cex.lab=2.0, cex.axis=1.5, cex.main=2.0)
#identify(v$x,v$y,labels,cex =1.3) #identify points in the plot
par(new=T)
#
qqnorm(res_min_rq,axes=F,main = "",xlab="",ylab="",type="l",ylim=faixa,lty=1,lwd=2.0)
par(new=T)
qqnorm(res_max_rq,axes=F,main = "",xlab="",ylab="", type="l",ylim=faixa,lty=1,lwd=2.0)
par(new=T)
qqnorm(res_mean_rq,axes=F,xlab="",main = "", ylab="", type="l",ylim=faixa,lty=2,lwd=2.0)
}#ends function

#*************************************************************************Description*************************************************************************************************#
#Create normal probability plots of quantile residuals with simulated envelope to assess the goodness of BGEV regression fit under MLE 
#***ARGUMENTS***#
# y - response variable.
# X -  regressor matrix for the median submodel.
# theta - vector of parameter estimates (via MLE) of the observed sample. 
# main.title - main title for the plot. Default is "Envelope".
# faixa.fixed - range of residuals values (optional). Default is NULL.
# labels.fixed - labels of the observations used to create the plot (optional). Default is NULL meaning that all observations are used.
#**************************************************************************************************************************************************************************************#

envelope_BGEV <- function(y, X, theta, main.title ='', faixa.fixed = NULL, labels.fixed = NULL) { 
source("BGEV_MLE.r")
B <- 100; #number of replicates
kk1 <- ncol(X);  n <- nrow(X)
#***parameters for parametric bootstrap***#
beta_p <- theta[1:kk1]
sigma_p <- theta[kk1+1.0]    	   
delta_p <- theta[kk1+2.0] 
etahat_p <- X%*%beta_p
m_p <- etahat_p
#************************************************************#

pbgev <- function(y, m, sigma, delta){ 
  # Compute auxiliary variables:
  mu <- m - (-sigma*log(-log(0.5)))^(1/(delta+1)) # m is median
  T      <- (y-mu)*(abs(y-mu)^delta)
  # Compute 
  cdf    <- pgev(T, loc=0, scale=sigma, shape=0)
  # Return Value
  return(cdf)
}
qbgev   <- function(p, m, sigma, delta){
  # Compute distribution points according to their sign
  mu <- m - (-sigma*log(-log(0.5)))^(1/(delta+1)) # m is median
  quantile <- sign(qgevd(p, 0, sigma, 0))*(abs(qgevd(p, 0, sigma, 0)))^(1/(delta + 1)) + mu
  # Return Value
  return(quantile)
}
rbgev <- function(n, m, sigma, delta){
  # Compute auxiliary variables:
  U <- runif(n)
  # Compute random numbers
  mu <- m - (-sigma*log(-log(0.5)))^(1/(delta+1)) # m is median
  rnumber <- qbgev(U, m, sigma, delta)
  # Return Value
  return(rnumber)
}

Menvelope_rq <- matrix(numeric(0),nrow=n,ncol=B)

#------------> quantile residuals for the observed sample<--------------#

rq <- qnorm(pbgev(y, m_p, sigma_p, delta_p))

suppressWarnings(set.seed(c(1994,1991), kind="Marsaglia-Multicarry")) #to generate same plot

    for(j in 1:B){		
        ygen <- rbgev(n,  m_p, sigma_p, delta_p)
        fit_b <- MLE_BGEV(y=ygen, X=X, method="BFGS")
        rq_b <- qnorm(pbgev(ygen, X%*%as.numeric(fit_b$beta), fit_b$sigma, fit_b$delta))
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
}
#ends function

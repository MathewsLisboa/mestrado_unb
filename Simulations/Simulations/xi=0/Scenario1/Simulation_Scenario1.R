source("BGEV_MLE.r")
library(EnvStats)
library(evd)
if(!suppressWarnings(require(BBmisc))){suppressWarnings(install.packages("BBmisc"));suppressWarnings(library(BBmisc))} #used for is.error function

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


sink("Results_Scenario1.txt")

names_results1 <- c("Estimates_Scenario1_n50", "Estimates_Scenario1_n100", "Estimates_Scenario1_n500")
names_results2 <- c("SE_Estimates_Scenario1_n50", "SE_Estimates_Scenario1_n100", "SE_Estimates_Scenario1_n500")

########################################################################
#######################Global Variables#################################
########################################################################
 
VN <- c(50, 100, 500) #Sample sizes
VBETA <- c(1.2, 2.0, 1.5) #true beta values
VSIGMA <- 1 #true sigma value
VDELTA <- 0.5 #true delta value

VTHETA <- c(VBETA, VSIGMA, VDELTA) # true theta values
NREP <- 10000 #number of Monte Carlo replicates
kk1 <- length(VBETA)

#*************generated covariates******************#
se1 = 19 ; se2 = 94 #random seed
suppressWarnings(set.seed(c(se1,se2), kind="Marsaglia-Multicarry")) #To ensure repeatability of the experiment

for(l in 1:length(VN))#Loop for the sample size
{
N = VN[l]
x1 <- runif(N); x2 <- rbinom(N, size=1, prob=0.4)
X <- matrix(c(rep(1,N), x1, x2), ncol=3, byrow=F); #regressor matrix for the median model
eta <- as.vector(X%*%VBETA)	
m <- eta
#summary(m)

#***Initializing vectors to store the estimates ***#
thetahat <- matrix(NA, NREP, kk1 + 2)
se_thetahat <- matrix(NA, NREP, kk1 + 2)
z_thetahat <- matrix(NA, NREP, kk1 + 2)

cont <- 0 # Counter for Monte Carlo replicates 
f1 <- 0 # Counter for failure in parameter estimation
f2 <- 0 # Counter for failure in hessian estimation
while(cont < NREP)
{
cont <- cont + 1 
perc <- cont/NREP
#***To print the progress of the simulation***#
if(perc == 0.25 || perc == 0.5 || perc == 0.75 || perc ==1) cat("Perc. Replic. MC =",perc*100,"%","\n")
                                          
 y <- rbgev(n=N, m=m, sigma=VSIGMA, delta=VDELTA)                                           

 # regression fit
 fitMLE   <- tryCatch(MLE_BGEV(y=y, X=X, method="BFGS"), error=function(e) {e})
         
  if(is.error(fitMLE)){#in case of error in the estimation procedure
      cont <- cont - 1
      f1 <- f1 + 1 
  }
  else
      {

                if(anyNA(c(fitMLE$se_beta, fitMLE$se_sigma, fitMLE$se_delta))){#to avoid hessian problems
                   cont <- cont - 1
                   f2 <- f2 + 1
                }
                else
                {
                  if(fitMLE$convergence == 0 || fitMLE$convergence == 1){  #estimates only if there is convergence

                        #***************parameter estimates*************************#
                         thetahat[cont,] <- c(fitMLE$beta, fitMLE$sigma, fitMLE$delta)
 
                        #***************asymptotic standard error estimates*************************#
                         se_thetahat[cont,] <- c(fitMLE$se_beta, fitMLE$se_sigma, fitMLE$se_delta)
  
                        #***************z-statistics*************************#
                          z_thetahat[cont, ] <- (thetahat[cont,] - VTHETA)/se_thetahat[cont,]
                   }
                   else
                      { 
                        cont <- cont - 1
                        f1 <- f1 + 1 
                       }
                 }
    }
 }#closes MC replicates

#******************results***************************#

#**************mean of estimates*************#
M_thetahat <- apply(thetahat,2,mean)

#************** bias of estimates*************#
B_thetahat <- (M_thetahat - VTHETA)

#**************mean of se estimates*************#
M_se_thetahat <- apply(se_thetahat,2,mean)

#**************root mean squared error of estimates*************#
RMSE_thetahat <- sqrt(apply(apply( t(thetahat) - VTHETA, 1, function(x) x^2), 2, mean) )

#**************empirical null levels of Wald tests*************#
LEVEL_MLE <- apply(abs(z_thetahat)>1.959964, 2, mean)

###############################################Outputs################################################

out_estimates_MLE <- cbind(M_thetahat, B_thetahat,RMSE_thetahat, M_se_thetahat, LEVEL_MLE)
fail_rate1 <- round((f1/NREP)*100,3)
fail_rate2 <- round((f2/NREP)*100,3)
cat(" ========================================================================================== \n")
cat(" TRUE VALUES ","\n")
cat(" Sample size ",N ,"\n")
out_VTHETA <- rbind(VTHETA);colnames(out_VTHETA)=c("beta1", "beta2", "beta3", "sigma", "delta");rownames(out_VTHETA)=c("");print(out_VTHETA)
Summ_m <- round(c(min(m), max(m), median(m)),3)
out_Sm <- rbind(Summ_m);colnames(out_Sm)=c("m_min", "m_max", "m_median");rownames(out_Sm)=c("");print(out_Sm)
cat(" ========================================================================================== \n")
cat(" ================================ MLE estimates for BGEV regression ================================ \n")
output_mle = rbind(out_estimates_MLE);colnames(output_mle)=c("Mean estimates","Bias","RMSE", "Mean SE", "Null level");rownames(output_mle)=c("beta1", "beta2", "beta3", "sigma", "delta");print(output_mle)
cat(" ========================================================================================== \n")
cat("Percentage of estimation failure=", fail_rate1,"%","\n")
cat("Percentage of hessian failure=", fail_rate2,"%","\n")
cat(" ========================================================================================== \n")

#******Print replicates*****#
write.table(cbind(thetahat), 
file= paste(names_results1[l], ".txt", sep = ""), append = FALSE, sep = " ", dec = ".", row.names = FALSE,
col.names = c("MLE_beta1", "MLE_beta2", "MLE_beta3", "MLE_sigma", "MLE_delta"))

write.table(cbind(se_thetahat), 
file= paste(names_results2[l], ".txt", sep = ""), append = FALSE, sep = " ", dec = ".", row.names = FALSE,
col.names = c("se_MLE_beta1", "se_MLE_beta2", "se_MLE_beta3", "se_MLE_sigma", "se_MLE_delta"))

}

sink()

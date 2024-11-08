rm(list=ls())

data50 <- read.table("Estimates_Scenario4_n50.txt", h=T)
data100 <- read.table("Estimates_Scenario4_n100.txt", h=T)
data500 <- read.table("Estimates_Scenario4_n500.txt", h=T)

#**************************MLE estimates******************************#

MLE_beta1_n50 <- data50$MLE_beta1;
MLE_beta2_n50 <- data50$MLE_beta2; 
MLE_beta3_n50 <- data50$MLE_beta3; 
MLE_sigma_n50 <- data50$MLE_sigma; 
MLE_delta_n50 <- data50$MLE_delta; 
MLE_xi_n50 <- data50$MLE_xi;

MLE_beta1_n100 <- data100$MLE_beta1;
MLE_beta2_n100 <- data100$MLE_beta2; 
MLE_beta3_n100 <- data100$MLE_beta3; 
MLE_sigma_n100 <- data100$MLE_sigma; 
MLE_delta_n100 <- data100$MLE_delta;
MLE_xi_n100 <- data100$MLE_xi;

MLE_beta1_n500 <- data500$MLE_beta1;
MLE_beta2_n500 <- data500$MLE_beta2; 
MLE_beta3_n500 <- data500$MLE_beta3; 
MLE_sigma_n500 <- data500$MLE_sigma; 
MLE_delta_n500 <- data500$MLE_delta;
MLE_xi_n500 <- data500$MLE_xi;

#********************MLE estimates of beta1****************#
boxplot(MLE_beta1_n50,MLE_beta1_n100,MLE_beta1_n500, main=expression( paste("MLE for ", beta[1])),
xlab="Sample size",ylab="",names=c(50,100,500),outline=T,
ylim=c(2.4,5.15), cex=1.0,cex.lab=2.0,cex.axis=1.5,cex.main=2.0, pch=16, col=NULL)
abline(4.0,0,col=2,lty=2,lwd=2)

#********************MLE estimates of beta2****************#
boxplot(MLE_beta2_n50,MLE_beta2_n100,MLE_beta2_n500, main=expression( paste("MLE for ", beta[2])),
xlab="Sample size",ylab="",names=c(50,100,500),outline=T,
ylim=c(0.1,4.03), cex=1.0,cex.lab=2.0,cex.axis=1.5,cex.main=2.0, pch=16, col=NULL)
abline(2.0,0,col=2,lty=2,lwd=2)

#********************MLE estimates of beta3****************#
boxplot(MLE_beta3_n50,MLE_beta3_n100,MLE_beta3_n500, main=expression( paste("MLE for ", beta[3])),
xlab="Sample size",ylab="",names=c(50,100,500),outline=T,
ylim=c(1.47,4.14), cex=1.0,cex.lab=2.0,cex.axis=1.5,cex.main=2.0, pch=16, col=NULL)
abline(3.0,0,col=2,lty=2,lwd=2)

#********************MLE estimates of xi****************#
boxplot(MLE_xi_n50,MLE_xi_n100,MLE_xi_n500, main=expression( paste("MLE for ", xi)),
xlab="Sample size",ylab="",names=c(50,100,500),outline=T,
ylim=c(-2.55,1.06), cex=1.0,cex.lab=2.0,cex.axis=1.5,cex.main=2.0, pch=16, col=NULL)
abline(-0.25,0,col=2,lty=2,lwd=2)

#********************MLE estimates of sigma****************#
boxplot(MLE_sigma_n50,MLE_sigma_n100,MLE_sigma_n500, main=expression( paste("MLE for ", sigma)),
xlab="Sample size",ylab="",names=c(50,100,500),outline=T,
ylim=c(0.14,2.34), cex=1.0,cex.lab=2.0,cex.axis=1.5,cex.main=2.0, pch=16, col=NULL)
abline(1.0,0,col=2,lty=2,lwd=2)

#********************MLE estimates of delta****************#
boxplot(MLE_delta_n50,MLE_delta_n100,MLE_delta_n500, main=expression( paste("MLE for ", delta)),
xlab="Sample size",ylab="",names=c(50,100,500),outline=T,
ylim=c(-0.42,3.81), cex=1.0,cex.lab=2.0,cex.axis=1.5,cex.main=2.0, pch=16, col=NULL)
abline(1.0,0,col=2,lty=2,lwd=2)









rm(list=ls())

data50 <- read.table("Estimates_Scenario2_n50.txt", h=T)
data100 <- read.table("Estimates_Scenario2_n100.txt", h=T)
data500 <- read.table("Estimates_Scenario2_n500.txt", h=T)

#**************************MLE estimates******************************#

MLE_beta1_n50 <- data50$MLE_beta1;
MLE_beta2_n50 <- data50$MLE_beta2; 
MLE_beta3_n50 <- data50$MLE_beta3; 
MLE_sigma_n50 <- data50$MLE_sigma; 
MLE_delta_n50 <- data50$MLE_delta; 

MLE_beta1_n100 <- data100$MLE_beta1;
MLE_beta2_n100 <- data100$MLE_beta2; 
MLE_beta3_n100 <- data100$MLE_beta3; 
MLE_sigma_n100 <- data100$MLE_sigma; 
MLE_delta_n100 <- data100$MLE_delta;

MLE_beta1_n500 <- data500$MLE_beta1;
MLE_beta2_n500 <- data500$MLE_beta2; 
MLE_beta3_n500 <- data500$MLE_beta3; 
MLE_sigma_n500 <- data500$MLE_sigma; 
MLE_delta_n500 <- data500$MLE_delta;


#********************MLE estimates of beta1****************#
boxplot(MLE_beta1_n50,MLE_beta1_n100,MLE_beta1_n500, main=expression( paste("MLE for ", beta[1])),
xlab="Sample size",ylab="",names=c(50,100,500),outline=T,
ylim=c(2.45,5.05), cex=1.0,cex.lab=2.0,cex.axis=1.5,cex.main=2.0, pch=16, col=NULL)
abline(4.0,0,col=2,lty=2,lwd=2)

#********************MLE estimates of beta2****************#
boxplot(MLE_beta2_n50,MLE_beta2_n100,MLE_beta2_n500, main=expression( paste("MLE for ", beta[2])),
xlab="Sample size",ylab="",names=c(50,100,500),outline=T,
ylim=c(0.35,3.9), cex=1.0,cex.lab=2.0,cex.axis=1.5,cex.main=2.0, pch=16, col=NULL)
abline(2.0,0,col=2,lty=2,lwd=2)

#********************MLE estimates of beta3****************#
boxplot(MLE_beta3_n50,MLE_beta3_n100,MLE_beta3_n500, main=expression( paste("MLE for ", beta[3])),
xlab="Sample size",ylab="",names=c(50,100,500),outline=T,
ylim=c(2.1,4.15), cex=1.0,cex.lab=2.0,cex.axis=1.5,cex.main=2.0, pch=16, col=NULL)
abline(3.0,0,col=2,lty=2,lwd=2)

#********************MLE estimates of sigma****************#
boxplot(MLE_sigma_n50,MLE_sigma_n100,MLE_sigma_n500, main=expression( paste("MLE for ", sigma)),
xlab="Sample size",ylab="",names=c(50,100,500),outline=T,
ylim=c(0.45,1.8), cex=1.0,cex.lab=2.0,cex.axis=1.5,cex.main=2.0, pch=16, col=NULL)
abline(1.0,0,col=2,lty=2,lwd=2)

#********************MLE estimates of delta****************#
boxplot(MLE_delta_n50,MLE_delta_n100,MLE_delta_n500, main=expression( paste("MLE for ", delta)),
xlab="Sample size",ylab="",names=c(50,100,500),outline=T,
ylim=c(-0.48,2.78), cex=1.0,cex.lab=2.0,cex.axis=1.5,cex.main=2.0, pch=16, col=NULL)
abline(1.0,0,col=2,lty=2,lwd=2)












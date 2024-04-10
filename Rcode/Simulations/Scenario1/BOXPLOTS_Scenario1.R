rm(list=ls())

data30 <- read.table("Estimates_Scenario1_n30.txt", h=T)
data60 <- read.table("Estimates_Scenario1_n60.txt", h=T)
data100 <- read.table("Estimates_Scenario1_n100.txt", h=T)
data500 <- read.table("Estimates_Scenario1_n500.txt", h=T)

#**************************MLE estimates******************************#
MLE_beta1_n30 <- data30$MLE_beta1;
MLE_beta2_n30 <- data30$MLE_beta2; 
MLE_beta3_n30 <- data30$MLE_beta3; 
MLE_sigma_n30 <- data30$MLE_sigma; 
MLE_delta_n30 <- data30$MLE_delta; 

MLE_beta1_n60 <- data60$MLE_beta1;
MLE_beta2_n60 <- data60$MLE_beta2; 
MLE_beta3_n60 <- data60$MLE_beta3; 
MLE_sigma_n60 <- data60$MLE_sigma; 
MLE_delta_n60 <- data60$MLE_delta; 

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



par(mfrow = c(3,2))
#********************MLE estimates of beta1****************#
boxplot(MLE_beta1_n30,MLE_beta1_n60,MLE_beta1_n100,MLE_beta1_n500, main=expression( paste("Estimativas para ", beta[1])),
xlab="n",ylab="",names=c(30,60,100,500),outline=T,
ylim=c(2,5.5), cex=1.0,cex.lab=2.0,cex.axis=1.5,cex.main=2.0, pch=16, col=NULL)
abline(4.0,0,col=2,lty=2,lwd=2)

#********************MLE estimates of beta2****************#
boxplot(MLE_beta2_n30,MLE_beta2_n60,MLE_beta2_n100,MLE_beta2_n500, main=expression( paste("Estimativas para ", beta[2])),
xlab="n",ylab="",names=c(30,60,100,500),outline=T,
ylim=c(-0.5,4), cex=1.0,cex.lab=2.0,cex.axis=1.5,cex.main=2.0, pch=16, col=NULL)
abline(2.0,0,col=2,lty=2,lwd=2)

#********************MLE estimates of beta3****************#
boxplot(MLE_beta3_n30,MLE_beta3_n60,MLE_beta3_n100,MLE_beta3_n500, main=expression( paste("Estimativas para ", beta[3])),
xlab="n",ylab="",names=c(30,60,100,500),outline=T,
ylim=c(0,5), cex=1.0,cex.lab=2.0,cex.axis=1.5,cex.main=2.0, pch=16, col=NULL)
abline(3.0,0,col=2,lty=2,lwd=2)

#********************MLE estimates of sigma****************#
boxplot(MLE_sigma_n30,MLE_sigma_n60,MLE_sigma_n100,MLE_sigma_n500, main=expression( paste("Estimativas para ", sigma)),
xlab="n",ylab="",names=c(30,60,100,500),outline=T,
ylim=c(0.35,2.25), cex=1.0,cex.lab=2.0,cex.axis=1.5,cex.main=2.0, pch=16, col=NULL)
abline(1.0,0,col=2,lty=2,lwd=2)

#********************MLE estimates of delta****************#
boxplot(MLE_delta_n30,MLE_delta_n60,MLE_delta_n100,MLE_delta_n500, main=expression( paste("Estimativas para ", delta)),
xlab="n",ylab="",names=c(30,60,100,500),outline=T,
ylim=c(-1,4), cex=1.0,cex.lab=2.0,cex.axis=1.5,cex.main=2.0, pch=16, col=NULL)
abline(1.0,0,col=2,lty=2,lwd=2)

### 

library(cowplot)





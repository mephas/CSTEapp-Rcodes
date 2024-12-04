##
## Reproducible R codes to generate data and results
## 
rm(ls=list())

## install.packages(CSTE)
library(CSTE)
library(dplyr)

# Example 1

est.aids = read.csv("aids-est.csv")
pred.aids = read.csv("aids-pred.csv")

## estimate CSTE
fit = cste_bin(x = est.aids[,-c(1:2)], y = est.aids$outcome, z=est.aids$treat, 
               lam = 0, nknots = 2, max.iter =1000, eps = 0.001)
res = cste_bin_SCB(x=est.aids[,-c(1:2)], fit, h=0.05, alpha = 0.05)
plot(res$or_x, res$fit_x, col = "#F8766D", type="l", lwd=2.5, lty = 3, xlim = range(res$or_x),
     xlab = expression(X*hat(beta)[1]), ylab = expression(g1(X*hat(beta)[1])),
     ylim=c(-5,5))#xlim=c(-4,4)
lines(res$or_x, res$lower_bound, lwd=2, col = "#00BFC4", lty=2)
lines(res$or_x, res$upper_bound, lwd=2, col = "#00BFC4", lty=2)
abline(h=0, col = "grey")

## predict new subjects
fit.pred = predict_cste_bin(fit, pred.aids)
newor = as.matrix(pred.aids) %*% matrix(fit$beta1, ncol=1)

plot(res$or_x, res$fit_x, col = "#F8766D", type="l", lwd=2.5, lty = 3, xlim = range(res$or_x),
     xlab = expression(X*hat(beta)[1]), ylab = expression(g1(X*hat(beta)[1])),
     ylim=c(-5,5))#xlim=c(-4,4)
lines(res$or_x, res$lower_bound, lwd=2, col = "#00BFC4", lty=2)
lines(res$or_x, res$upper_bound, lwd=2, col = "#00BFC4", lty=2)
abline(v = newor, lty = 2, col = "grey")
abline(h=0, col = "grey")
# points(newor, fit.pred$g1)

# Example 2

library(mvtnorm)
library(sigmoid)
n = 2000
p = 20
set.seed(100) 
# generate X
sigma = outer(1:p, 1:p, function(i, j){ 2^(-abs(i-j)) } )
X = rmvnorm(n, mean = rep(0,p), sigma = sigma)
X = relu(X + 2) - 2; X = 2 - relu(2 - X) 
# generate Z
Z = rbinom(n, 1, 0.5) 
# generate Y
beta1 = rep(0, p); beta1[1:3] = rep(1/sqrt(3), 3)
beta2 = rep(0, p); beta2[1:2] = c(1, -2)/sqrt(5)
mu1 = X %*% beta1; mu2 = X %*% beta2
g1 = mu1*(1 - mu1); g2 = exp(mu2)      
prob = sigmoid(g1*Z+ g2)
Y = rbinom(n, 1, prob)

## data is inserted in the app
data1 = data.frame(Treat = Z, Y = Y, X=round(X,3))

## find the optimal model between 0.001,0.002,...,0.01
fit.s = select_cste_bin(X,Y,Z,lam_seq = seq(0.001,0.01,0.001), nknots = 2, 
                       beta_ini = NULL, max.iter =1000, eps = 0.001)
fit = fit.s$optimal
fit$bic

## Compute the true Xbeta1 and g1
beta1 = rep(0,20)
beta1[1:3] = rep(1/sqrt(3),3)
mu1 = X %*% beta1
g1 = mu1*(1 - mu1)

## Plot the estimated CSTE curve
png("plot1.png", width = 1200, height = 600)
par(mfrow = c(1,2))
## true mu1 and estimated g1
plot(mu1, fit$g1, col = "#00BFC4", cex = 0.5, xlim = c(-2.5,2.5),ylim = c(-10,5),
     xlab = expression(X*beta[1]), ylab = expression(CSTE==g1(X*beta[1])))
## true mu1 and true g1
points(mu1, g1, cex = 0.4, ylim = c(-8,3), col="#F8766D")
legend("topleft", legend=c("Estimated CSTE curve","True CSTE curve"), pch = c(1,1), 
       col=c("#00BFC4","#F8766D"))
title(expression("The CSTE curve against the true"~X*beta[1]))

## Estimate the 95% SCB
res = cste_bin_SCB(X, fit, h=0.05, alpha = 0.05)
## Plot 95% SCB
plot(res$or_x, res$fit_x, col = "#F8766D", type="l", lwd=2.5, lty = 3, 
     ylim = c(-10,5), xlim = c(-2.5,2.5),
     xlab = expression(X*hat(beta)[1]), ylab = expression(CSTE==g1(X*hat(beta)[1])))
lines(res$or_x, res$lower_bound, lwd=2, col = "#00BFC4", lty=2)
lines(res$or_x, res$upper_bound, lwd=2, col = "#00BFC4", lty=2)
lines(mu1[order(mu1)], g1[order(mu1)], col="blue")
legend("topleft", legend=c("Estimated CSTE curve","95% CI","True CSTE curve"), 
       lwd=c(2.5,2,1), lty=c(3,2,1), col=c("#F8766D","#00BFC4","blue"))
title(expression("The CSTE curve against the estimated"~X*hat(beta)[1]))
par(mfrow = c(1,1))
dev.off()
## prediction
n = 15
p = 20
set.seed(123) 
# generate X
sigma = outer(1:p, 1:p, function(i, j){ 2^(-abs(i-j)) } )
X = rmvnorm(n, mean = rep(0,p), sigma = sigma)
X = relu(X + 2) - 2; X = 2 - relu(2 - X) 
data3 = data.frame(X=round(X,3))
pfit = predict_cste_bin(fit, data3)
newor = as.matrix(data3) %*% matrix(fit$beta1, ncol=1)

plot(res$or_x, res$fit_x, col = "#F8766D", type="l", lwd=2.5, lty = 3, xlim = range(res$or_x),
     xlab = expression(X*hat(beta)[1]), ylab = expression(g1(X*hat(beta)[1])),
     ylim=c(-5,5))#xlim=c(-4,4)
lines(res$or_x, res$lower_bound, lwd=2, col = "#00BFC4", lty=2)
lines(res$or_x, res$upper_bound, lwd=2, col = "#00BFC4", lty=2)
abline(v = newor, lty = 2, col = "grey")
abline(h=0, col = "grey")
# points(newor, pfit$g1)

# Example 3
data2 = read.csv("leukemiaPKU-surv.csv")
biomarker = data$AGE
biomarker2 = (biomarker - min(biomarker)) / (max(biomarker) - min(biomarker))
treat = data$TRANSPLANT
censor = data$OS
time = data$OST
data2 = data.frame(treat=treat,
                   censor=censor,time=time,biomarker2=biomarker2)
res = cste_surv_SCB(c(1), x=data2$biomarker2,
                    y=data2$time,z=as.matrix(data2$treat), s=data2$censor, 
                    h = 0.5, m = 50, alpha = 0.05)
ord = order(biomarker)
plot(biomarker[ord], res[ord,2], col = '#F8766D', type = "l", lwd = 2,
     ylim = c(-10,10),
     ylab=expression(beta(x)), xlab = 'X')
lines(biomarker[ord], res[ord,1], lwd = 2, col = '#00BFC4', lty = 2)
lines(biomarker[ord], res[ord,3], lwd = 2, col = '#00BFC4', lty = 2)
abline(h = 0, lty = 2, cex = 0.2)
legend("topright", legend = c("Estimates", "95% CI"),
       lwd = c(2,2), lty = c(1, 2), col = c('#F8766D','#00BFC4'))



# Example 4
# 
set.seed(123)
n = 100
b1 = sample(c(0,1),n,TRUE,prob=c(0.7,0.3))
b2 = sample(c(0,1),n,TRUE,prob=c(0.5,0.5))
z1 = b1
z2 = (1-b1)*b2
z = cbind(z1,z2)
x = runif(n)
x2 = (x - min(x)) / (max(x) - min(x))
beta1 = -1-exp(x)
beta2 = -exp(x)
beta = cbind(beta1,beta2)
t = (-3/0.6/exp(beta1*z1+beta2*z2+x*x)*log(1-runif(n)))^(1/3)
a = 0.23 #exponential parameter
c = -log(1-runif(n))/a/x
s = as.numeric(t<c)
y = s*t+(1-s)*c
data = data.frame(Treat1 = z1, Treat2 = z2, Censor = s, Time = round(y,2), Biomarker = round(x2,3))

## estimate the CSTE curve
png("plot2.png", width = 1200, height = 600)
par(mfrow = c(1,2))
res1 = cste_surv_SCB(c(1,0),
                     x = data$Biomarker, y = data$Time, 
                     z = as.matrix(cbind(data$Treat1,data$Treat2)), s = data$Censor, 
                     h = 0.5, m = 50, alpha = 0.05)  
ord = order(x)
plot(x[ord], res1[ord,2], col = '#F8766D', type = "l", lwd = 2,
     ylim = c(-10,10),
     ylab=expression(beta(x)), xlab = 'X')
lines(x[ord], res1[ord,1], lwd = 2, col = '#00BFC4', lty = 2)
lines(x[ord], res1[ord,3], lwd = 2, col = '#00BFC4', lty = 2)
lines(x[ord], beta[ord,1], col="blue")
abline(h = 0, lty = 2, cex = 0.2)
legend("topright", legend = c("Estimated CSTE curve", "95% CI","True CSTE curve"),
       lwd = c(2,2), lty = c(1, 2), col = c('#F8766D','#00BFC4',"blue"))


res2 = cste_surv_SCB(c(0,1),
                     x = data$Biomarker, y = data$Time, 
                     z = as.matrix(cbind(data$Treat1,data$Treat2)), s = data$Censor, 
                     h = 0.5, m = 50, alpha = 0.05) 
ord = order(x)
plot(x[ord], res2[ord,2], col = '#F8766D', type = "l", lwd = 2,
     ylim = c(-10,10),
     ylab=expression(beta(x)), xlab = 'X')
lines(x[ord], res2[ord,1], lwd = 2, col = '#00BFC4', lty = 2)
lines(x[ord], res2[ord,3], lwd = 2, col = '#00BFC4', lty = 2)
lines(x[ord], beta[ord,1], col="blue")
abline(h = 0, lty = 2, cex = 0.2)
legend("topright", legend = c("Estimated CSTE curve", "95% CI", "True CSTE curve"),
       lwd = c(2,2), lty = c(1, 2), col = c('#F8766D','#00BFC4',"blue"))
par(mfrow = c(1,1))
dev.off()
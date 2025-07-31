## data creation
## 
# Example 1
# data = read.csv("data-AIDS.csv")%>%as.data.frame()
# z = data$treat
# y = data$cid
# x = scale(data[,c(4,5,21,22,23,24),drop=F])  ## scaled covariates
# aids=data.frame(treat = data$treat, outcome = data$cid, x)
# set.seed(123)
# id = sample(2100)
# est.aids = aids[id,]
# pred.aids = aids[-id,-c(1:2)]
# 
# ## save data
# write.csv(est.aids, "aids-est.csv", row.names = F)
# write.csv(pred.aids, "aids-pred.csv", row.names = F)

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

## data for prediction
n = 15
p = 20
set.seed(123) 
# generate X
sigma = outer(1:p, 1:p, function(i, j){ 2^(-abs(i-j)) } )
X = rmvnorm(n, mean = rep(0,p), sigma = sigma)
X = relu(X + 2) - 2; X = 2 - relu(2 - X) 
data3 = data.frame(X=round(X,3))



## Example 3
## 
# set.seed(123)
# data = read.csv("leukemiaPKU.csv")%>%na.omit()%>%
#   group_by(TRANSPLANT)%>%sample_n(40)
# data = data[,c(3,19,20,5)]
# write.csv(data,"leukemiaPKU-surv.csv", row.names = F)

## Example 4
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
data2 = data.frame(Treat1 = z1, Treat2 = z2, Censor = s, Time = round(y,2), Biomarker = round(x2,3))
data4 = data.frame(Treat = z1+2*z2, Censor = s, Time = round(y,2), Biomarker = round(x2,3))

save(data1,data2,data3,data4,file = "example.RData")

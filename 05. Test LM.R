
################################### Library ####################################
library("quantmod")
library("astsa")
library("readxl")
library("tidyverse")
library("fImport")
library("fBasics")
library("rugarch")
library("highfrequency")
library("xts")
library("zoo")
library("chron")


load("CRM1secdata.RData")


#################################### Test LM ###################################

RV=colSums(secdata^2)
m=c(60)
N=dim(secdata)[1]
T=dim(secdata)[2]
mm=median(RV)
d=diag(1,N/m,N/m) %x% t(rep(1,m))
r=d %*% secdata
N=dim(r)[1]

# RV Estimation
RV=colSums(r^2)


mu1 = sqrt(2/pi)
k = 270
r_BPV = abs(r)
n = T*k
beta = -log(-log(0.999))
S = 1/(mu1*(sqrt(2*log(n))))
C = (2*log(n))^(1/2)/mu1 - (log(pi)+log(log(n)))/(2*mu1*((2*log(n))^(1/2)))
a = NULL
rend = NULL
sigma_hat = NULL
dist_i = NULL
L_matrix = NULL
test_LM_i = NULL
test_LM = NULL
for (i in k:N) {
  rend = (r_BPV[(i-k+2):(i-1),]*r_BPV[(i-k+1):(i-2),])
  a = colSums(rend)
  sigma_hat = sqrt((1/(k-2))*a)
  L = r[i,]/sigma_hat
  L_matrix = cbind(L_matrix, L)
  dist_i = (abs(L)-C)/S
  test_LM_i = sum(dist_i>beta)
  test_LM = c(test_LM,test_LM_i)
}
sum(test_LM)


#### Residuals


m2=matrix(rep(nM,T),ncol=T,byrow=FALSE)
rs2=r/m2

for (i in k:N) {
  rend = (r_BPV[(i-k+2):(i-1),]*r_BPV[(i-k+1):(i-2),])
  a = colSums(rend)
  sigma_hat = sqrt((1/(k-2))*a)
  L = rs2[i,]/sigma_hat
  L_matrix = cbind(L_matrix, L)
  dist_i = (abs(L)-C)/S
  test_LM_i = sum(dist_i>beta)
  test_LM = c(test_LM,test_LM_i)
}
sum(test_LM)








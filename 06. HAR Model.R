############################### Carico librerie ################################

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
library("forecast")

################################## RV Model ################################## 

load("CRM1secdata.RData")

# Calcolo nuovamente la volatilità realizzata per dati ad un minuto
RV=colSums(secdata^2)
m=c(60)
N=dim(secdata)[1]
T=dim(secdata)[2]
mm=median(RV)
d=diag(1,N/m,N/m) %x% t(rep(1,m))
r=d %*% secdata
N=dim(r)[1]
RV=colSums(r^2)


# costruiamo i regressori
RV=as.timeSeries(RV)
RVlag=lag(RV,k=1:22)
RV1=RVlag[,1]
RV5=rowSums(RVlag[,1:5])/5
RV22=rowSums(RVlag)/22

# stima del modello HAR
outHAR=lm(RV ~ RV1+RV5+RV22)  
summary(outHAR)

# QQ - Plot
qqnorm(outHAR$residuals)
qqline(outHAR$residuals, col = "red")

# Correlogramma dei residui al quadrato
acf((outHAR$residuals)^2, main = "Correlogramma residui al quadrato")


# modello sui log
# costruiamo i regressori
RVlag=lag(log(RV),k=1:22)
RV1l=RVlag[,1]
RV5l=rowSums(RVlag[,1:5])/5
RV22l=rowSums(RVlag)/22
# stima del modello HAR
outHARlog=lm(log(RV) ~ RV1l+RV5l+RV22l)  
summary(outHARlog)


# QQ - Plot
qqnorm(outHARlog$residuals)
qqline(outHARlog$residuals, col = "red")


# Residui


par(mfrow = c(2,1))
acf(outHAR$residuals^2, main = "ACF residui modello HAR")
acf(outHARlog$residuals^2, main = "ACF residui modello HARlog")
par(mfrow = c(1,1))

?acf
################################ Modello HAR-CJ ################################ 

# Per calcolare il modello HAR-CJ è necessario decomporre RV in CV e J2
J2POS=colSums(((r^2)*idJ)*(r>0))
J2NEG=colSums(((r^2)*idJ)*(r<0))
J2=J2POS+J2NEG
CVPOS=colSums(((r^2)*(1-idJ))*(r>0))
CVNEG=colSums(((r^2)*(1-idJ))*(r<0))
CV=CVPOS+CVNEG

# A questo punto è necessario costruire i regressori 
CV = as.timeSeries(CV)
CVlag = lag(CV, k=1:22)
CV1 = CVlag[,1]
CV5l=rowSums(CVlag[,1:5])/5
CV22l=rowSums(CVlag)/22

J2 = as.timeSeries(J2)
J2lag = lag(J2, k=1:22)
J2.1 = J2lag[,1]
J2.5l=rowSums(J2lag[,1:5])/5
J2.22l=rowSums(J2lag)/22

# Posso stimare il modello

outHAR_CJ=lm(RV ~ CV1 + CV5l + CV22l + J2.1 + J2.5l + J2.22l)  
summary(outHAR_CJ)


# QQ - Plot
qqnorm(outHAR_CJ$residuals)
qqline(outHAR_CJ$residuals, col = "red")

# modello sui log
# costruiamo i regressori
CVlag=lag(log(CV),k=1:22)
CV1log=RVlag[,1]
CV5log=rowSums(RVlag[,1:5])/5
CV22log=rowSums(RVlag)/22

Jlag=lag(log(1+J2),k=1:22)
J1log=RVlag[,1]
J5log=rowSums(Jlag[,1:5])/5
J22log=rowSums(Jlag)/22


# stima del modello HAR
outHAR_CJ_log=lm(log(RV) ~ CV1log+CV5log+CV22log+J1log+J5log+J22log)  
summary(outHAR_CJ_log)



#################### MODELLI HAR-CJ: good and bad volatility ###################

# Decomposizione RV in CV e J2, i quali possono essere intesi come CV+,CV-,J2+,J2-
J2POS=colSums(((r^2)*idJ)*(r>0))
J2NEG=colSums(((r^2)*idJ)*(r<0))
J2=J2POS+J2NEG
CVPOS=colSums(((r^2)*(1-idJ))*(r>0))
CVNEG=colSums(((r^2)*(1-idJ))*(r<0))
CV=CVPOS+CVNEG

# A questo punto, dopo aver scomposto la volatilità realizzata (RV) e definito tutte
# le quantità è possibile calcolare la good volatility (GVOL) e la bad volatility (BVOL)

GVOL = CVPOS + J2POS
BVOL = CVNEG + J2NEG

# Anche in questo caso devo costruire i regressori

GVOL = as.timeSeries(GVOL)
GVOLlag = lag(GVOL, k=1:22)
GVOL1 = GVOLlag[,1]

BVOL = as.timeSeries(BVOL)
BVOLlag = lag(BVOL, k=1:22)
BVOL1 = BVOLlag[,1]

# A questo punto posso stimare il modello:

outHAR.GB=lm(RV ~ GVOL1+BVOL1+RV5+RV22)  
summary(outHAR.GB)

# QQ-Plot residui
qqnorm(outHAR.GB$residuals)
qqline(outHAR.GB$residuals, col = "red")


## modello logaritmico
GVOL_log = as.timeSeries(GVOL)
GVOLlag_log = lag(log(GVOL_log), k=1:22)
GVOL1_log = GVOLlag_log[,1]

BVOL_log = as.timeSeries(BVOL)
BVOLlag_log = lag(log(BVOL_log), k=1:22)
BVOL1_log = BVOLlag_log[,1]

outHAR_GB_log=lm(log(RV) ~ GVOL1_log+BVOL1_log+RV5l+RV22l)  
summary(outHAR_GB_log)

# residui
par(mfrow = c(2,1))
# QQ-Plot residui
qqnorm(outHAR.GB$residuals, main = "Modello good and bad")
qqline(outHAR.GB$residuals, col = "red")
qqnorm(outHAR_GB_log$residuals, main = "Modello-log good and bad")
qqline(outHAR_GB_log$residuals, col = "red")
############################# Model Confidence Set #############################
#install.packages("MCS")
library("MCS")

?MCSprocedure

pred.HAR = predict(outHAR)
pred.HAR.log = predict(outHARlog)
pred.HAR_CJ = predict(outHAR_CJ)
pred.HAR.GB = predict(outHAR.GB)
pred.HAR.GB.log = predict(outHAR_GB_log)

predictions = cbind(pred.HAR, pred.HAR.log, pred.HAR_CJ, pred.HAR.GB, pred.HAR.GB.log)
predictions = as.matrix(predictions)
mcs_result = MCSprocedure(predictions, alpha = 0.05, B = 5000, statistic='Tmax',cl=NULL)

print(mcs_result)
summary(mcs_result)



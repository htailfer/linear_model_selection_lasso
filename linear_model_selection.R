library('openxlsx')
rm(list=ls())
##### Load and plot data
r = info <- read.xlsx("data-amazon.xlsx", sheet = 1, detectDates = TRUE, skipEmptyRows = FALSE)
SValue = r$close
SDate = r$date
plot(as.Date(SDate, "%Y-%m-%d"), SValue, lwd=4, type="l") 

'%!in%' <- function(x,y)!('%in%'(x,y)) 

myreturn <- function(Price, TRef, Lag=1, Dir=0, Len=252) {
  if (Dir == 0) {
    # backward: t wrt t-lag, t within [TRef-Len+1, TRef]
    Ret = Price[(TRef-Len+1):TRef] / Price[(TRef-Len+1-Lag):(TRef-Lag)] - 1
  }
  else {
    # forward: t+lag wrt t+1, t within [TRef, TRef+Len-1]
    Ret = Price[(TRef+Lag):(TRef+Len-1+Lag)] / Price[(TRef+1):(TRef+Len)] - 1
  }
  return (Ret)
}

myma <- function(data, TRef, kerlen, Len=252){
  R = data[(TRef-Len+1):TRef]
  for (k in 1:(kerlen-1)) {
    R = R + data[(TRef-Len+1-k):(TRef-k)]
  }
  R = R / kerlen
  return (R)
}

WinLen = 252 * 2 # window length = 2Y
predDays = 10 # predict return of t+10 wrt t+1
StartIdx = WinLen + 252
R = myreturn(SValue, StartIdx, Lag=1, Dir=0, Len=WinLen + 128 - 1) # past daily return from StartIdx-Len+1 to StartIdx
Yt = myreturn(SValue, StartIdx-WinLen+1, Lag=predDays, Dir=1, Len=WinLen) # future returns
Ycen = Yt - mean(Yt)
Ftm1 = R[(length(R)-WinLen+1):length(R)]
Ftm2 = myma(R, length(R), kerlen=2, Len=WinLen)
Ftm4 = myma(R, length(R), kerlen=4, Len=WinLen)
Ftm8 = myma(R, length(R), kerlen=8, Len=WinLen)
Ftm16 = myma(R, length(R), kerlen=16, Len=WinLen)
Ftm32 = myma(R, length(R), kerlen=32, Len=WinLen)
Ftm64 = myma(R, length(R), kerlen=64, Len=WinLen)
Ftm128 = myma(R, length(R), kerlen=128, Len=WinLen)
M = cbind(Ftm1, Ftm2, Ftm4, Ftm8, Ftm16, Ftm32, Ftm64, Ftm128)

##Q1
barplot(cor(Yt,M))

##Q2
muM=colMeans(M)
stdM=apply(M,2,sd)
Mnorm=sweep(sweep(M,2,FUN='-',muM),2,FUN='/',stdM)

##Q3
linModelFull = lm(y~., data=data.frame(Mnorm,y=Yt))
summary(linModelFull)

library(car)
vif(linModelFull)
linModelRed=lm(y~Ftm2+Ftm4+Ftm8+Ftm16+Ftm32+Ftm64+Ftm128,data=data.frame(Mnorm,y=Yt))
summary(linModelRed)

linModelRed=lm(y~Ftm2+Ftm4+Ftm8+Ftm16+Ftm64+Ftm128,data=data.frame(Mnorm,y=Yt))
summary(linModelRed)

linModelRed=lm(y~Ftm4+Ftm8+Ftm16+Ftm64+Ftm128,data=data.frame(Mnorm,y=Yt))
summary(linModelRed)

linModelRed=lm(y~Ftm4+Ftm8+Ftm16+Ftm128,data=data.frame(Mnorm,y=Yt))
summary(linModelRed)

linModelRed=lm(y~Ftm8+Ftm16+Ftm128,data=data.frame(Mnorm,y=Yt))
summary(linModelRed)

linModelRed=lm(y~Ftm8+Ftm128,data=data.frame(Mnorm,y=Yt))
summary(linModelRed)

anova(linModelFull,linModelRed)
install.packages('glmnet')
library(glmnet)

rdg=cv.glmnet(Mnorm,Ycen,alpha=0)
lso=cv.glmnet(Mnorm,Ycen,alpha=1)
plot(rdg$glmnet.fit, "lambda", label=TRUE, lwd=8)
plot(lso$glmnet.fit, "lambda", label=TRUE, lwd=8)
coefLso = coef(lso, s='lambda.min') # Lasso parameter from minimizing cross validation error
varsLso = row.names(coefLso)[which(coefLso!=0)]
varsLso = varsLso[varsLso %!in% '(Intercept)']
formLso = as.formula(paste("y~", paste(varsLso, collapse="+")))

linModelLasso=lm(formLso,data=data.frame(Mnorm,y=Yt))
summary(linModelLasso)

anova(linModelFull,linModelLasso)

cat("AIC_full = ", -2*logLik(linModelFull)+2*length(linModelFull$coefficients),
    "BIC_full = ", -2*logLik(linModelFull)+length(linModelFull$coefficients)*log(WinLen),
    "AICc_full = ", -2*logLik(linModelFull)+2*length(linModelFull$coefficients)*WinLen/(WinLen-length(linModelFull$coefficients)-1))

cat("AIC_red = ", -2*logLik(linModelRed)+2*length(linModelRed$coefficients),
    "BIC_red= ", -2*logLik(linModelRed)+length(linModelRed$coefficients)*log(WinLen),
    "AICc_red = ", -2*logLik(linModelRed)+2*length(linModelRed$coefficients)*WinLen/(WinLen-length(linModelRed$coefficients)-1))

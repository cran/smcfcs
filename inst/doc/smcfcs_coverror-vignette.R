## -----------------------------------------------------------------------------
set.seed(1234)
n <- 1000
x <- rnorm(n)
w <- x+rnorm(n)
y <- x+rnorm(n)
x[(n*0.1):n] <- NA
simData <- data.frame(x,w,y)

## -----------------------------------------------------------------------------
library(smcfcs)
imps <- smcfcs(simData, smtype="lm", smformula="y~x",
                     method=c("norm", "", ""),m=5)

## -----------------------------------------------------------------------------
predMat <- array(0, dim=c(3,3))
predMat[1,2] <- 1

## -----------------------------------------------------------------------------
imps <- smcfcs(simData, smtype="lm", smformula="y~x",
                     method=c("norm", "", ""),m=5,
               predictorMatrix=predMat)

## -----------------------------------------------------------------------------
library(mitools)
impobj <- imputationList(imps$impDatasets)
models <- with(impobj, lm(y~x))
summary(MIcombine(models))

## -----------------------------------------------------------------------------
x <- rnorm(n)
w1 <- x+rnorm(n)
w2 <- x+rnorm(n)
w2[(n*0.1):n] <- NA
y <- x+rnorm(n)
x <- rep(NA,n)
simData <- data.frame(x,w1,w2,y)

## -----------------------------------------------------------------------------
errMat <- array(0, dim=c(4,4))
errMat[1,c(2,3)] <- 1
imps <- smcfcs(simData, smtype="lm", smformula="y~x",
                     method=c("latnorm", "", "",""),m=5,
               errorProneMatrix=errMat)

## -----------------------------------------------------------------------------
impobj <- imputationList(imps$impDatasets)
models <- with(impobj, lm(y~x))
summary(MIcombine(models))

## -----------------------------------------------------------------------------
summary(imps$impDatasets[[1]])

## ---- fig.width=6-------------------------------------------------------------
imps <- smcfcs(simData, smtype="lm", smformula="y~x",
                     method=c("latnorm", "", "",""),m=1,numit=100,
               errorProneMatrix=errMat)
plot(imps$smCoefIter[1,2,])

## -----------------------------------------------------------------------------
x <- rnorm(n)
x1 <- x+rnorm(n)
x2 <- x+rnorm(n)
w2[(n*0.1):n] <- NA
z <- x+rnorm(n)
z1 <- z+0.1*rnorm(n)
z2 <- z+0.1*rnorm(n)
y <- x-z+rnorm(n)
x <- rep(NA,n)
z <- rep(NA,n)
simData <- data.frame(x,x1,x2,z,z1,z2,y)

errMat <- array(0, dim=c(7,7))
errMat[1,c(2,3)] <- 1
errMat[4,c(5,6)] <- 1
imps <- smcfcs(simData, smtype="lm", smformula="y~x+z",
                     method=c("latnorm", "", "","latnorm", "", "", ""),m=5,
               errorProneMatrix=errMat)

## -----------------------------------------------------------------------------
impobj <- imputationList(imps$impDatasets)
models <- with(impobj, lm(y~x+z))
summary(MIcombine(models))


## -----------------------------------------------------------------------------
library(smcfcs)
ex_linquad[1:10, ]

## -----------------------------------------------------------------------------
set.seed(123)
# impute missing values in x, compatibly with quadratic substantive model
imps <- smcfcs(originaldata=ex_linquad, smtype = "lm", smformula = "y~z+x+I(x^2)", method = c("", "", "norm", ""))

## -----------------------------------------------------------------------------
# fit substantive model
library(mitools)
impobj <- imputationList(imps$impDatasets)
models <- with(impobj, lm(y ~ z + x + I(x^2)))
summary(MIcombine(models))

## -----------------------------------------------------------------------------
# impute missing values in x, compatibly with model for y which omits the quadratic effect
imps <- smcfcs(ex_linquad, smtype = "lm", smformula = "y~z+x", method = c("", "", "norm", ""))

## -----------------------------------------------------------------------------
# fit substantive model
impobj <- imputationList(imps$impDatasets)
models <- with(impobj, lm(y ~ z + x + I(x^2)))
summary(MIcombine(models))

## -----------------------------------------------------------------------------
# impute, including v as a covariate in the substantive/outcome model
imps <- smcfcs(ex_linquad, smtype = "lm", smformula = "y~z+x+I(x^2)+v", method = c("", "", "norm", ""))
# fit substantive model, which omits v
impobj <- imputationList(imps$impDatasets)
models <- with(impobj, lm(y ~ z + x + I(x^2)))
summary(MIcombine(models))

## -----------------------------------------------------------------------------
predMatrix <- array(0, dim = c(ncol(ex_linquad), ncol(ex_linquad)))
predMatrix[3, ] <- c(0, 1, 0, 1)
imps <- smcfcs(ex_linquad, smtype = "lm", smformula = "y~z+x+I(x^2)", method = c("", "", "norm", ""), predictorMatrix = predMatrix)
impobj <- imputationList(imps$impDatasets)
models <- with(impobj, lm(y ~ z + x + I(x^2)))
summary(MIcombine(models))

## ----fig.width = 6, fig.height = 4--------------------------------------------
# impute once with a larger number of iterations than the default 10
imps <- smcfcs(ex_linquad, smtype = "lm", smformula = "y~z+x+I(x^2)", method = c("", "", "norm", ""), predictorMatrix = predMatrix, m = 1, numit = 100)
# plot estimates of the parameters of the substantive model against iteration number
plot(imps)


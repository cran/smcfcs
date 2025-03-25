## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(smcfcs)
summary(ex_coarsening)
head(ex_coarsening)

## -----------------------------------------------------------------------------
table(ex_coarsening$x,ex_coarsening$xobs,useNA = "ifany")

## -----------------------------------------------------------------------------
restrictionsX = c("xobs = a/c ~ a + c",
                  "xobs = b/c ~ b + c")
restrictions = append(list(restrictionsX), as.list(c("", "", "")))

## ----results=FALSE, warning=FALSE---------------------------------------------
set.seed(68204812)
imps <- smcfcs(originaldata=ex_coarsening,
               smtype="lm",
               smformula = "y~z+x",
               method = c("mlogit","", "", ""),
               restrictions = restrictions
)

## -----------------------------------------------------------------------------
head(imps$impDatasets[[1]])

## -----------------------------------------------------------------------------
table(imps$impDatasets[[1]]$x,imps$impDatasets[[1]]$xobs,useNA = "ifany")


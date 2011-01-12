## transformations.R
##
## Pre-processing of Vis-NIR spectra
##

## SNV
## Barnes et al., 1989
##
snv <- function(x){
  .snv <- function(x){(x - mean(x))/sd(x)}
  nspec <- nrow(x)
  if (is.null(nspec)){
    res <- .snv(x)
  } 
  else {
    require(plyr)
    res <- aaply(.data=x, .margins=1, .fun=.snv)
  }
  res
}

## RNV
## Guo et al, 1999
rnv <- function(x, r){
  .rnv <- function(x, r){
    pct <- as.numeric(quantile(x=x, probs=r, na.rm=TRUE))
    (x - pct)/(sd(x[x <= pct]))
  }
  nspec <- nrow(x)
  if (is.null(nspec)){
    res <- .rnv(x, r)
  } 
  else {
    require(plyr)
    res <- aaply(.data=x, .margins=1, .fun=.rnv, r=r)
  }
  res
}
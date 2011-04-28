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

## Continuum removal
##
remove_continuum <- function(obj){

# # using plyr
#   require(plyr)
#   require(reshape)
#   res <- aaply(.data=spectra(obj), .margins=1, .fun=function(x) {
#     ch.index <- sort(chull(x))
#     ch <- data.frame(wl=wl(obj)[ch.index], nir=x[ch.index])
#     ch <- approx(x=ch$wl, y=ch$nir, xout=wl(obj)) 
#     x - ch$y
#     })

  # using a for loop
  sp <- spectra(obj)
  res <- sp

  for (i in 1:nrow(sp)) {
    ch.index <- sort(chull(sp[i,]))
    ch <- data.frame(wl=wl(obj)[ch.index], nir=sp[i, ch.index])
    ch <- approx(x=ch$wl, y=ch$nir, xout=wl(obj))
    res[i, ] <- sp[i, ] - ch$y
  }

  obj@nir <- res
  obj
}

## from Raphael
continuum.removal <- function(obj, wavelength=wl(obj), method="linear"){
  require(signal)

  spc <- spectra(obj)
  cr <- spc

  for (i in 1:nrow(spc)){
    id <- sort(chull(wavelength, spc[i,]))
    cr[i, ] <- spc[i,] - interp1(wavelength[id], spc[i,id], wavelength, method=method, extrap=TRUE)
  }
  obj@nir <- cr
  obj
}

## for baseline, see the baseline package

## what about derivatives? See KernSmooth package. (locpoly function)

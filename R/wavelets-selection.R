## wavelets selection
##
## from Viscarra-Rossel and Lark, 2010
##
## ====================================
##
## for each wavelength i :
##     let X be a vector of all the wavelet coefficients for all the samples at that wavelength
##     let V[i] be the variance of X
## 
## Then V is a vector of length = number of wavelengths. (the concatenation of all V[i])
## 
## We then order V to select the wavelengths. This selection is based on a parsimony principle
##
wavelet_selection <- function(obj){
  require(plyr)
   
  # compute a matrix of wavelet coefs for each spectra
  .mraCoefs <- function(x, ...){
    require(waveslim)
    require(stringr)
    res <- mra(x, ...)
    # we only return the wavelet coefs (not the scaling coefs)
    laply(res[str_detect(names(res), "D")], identity, .progress='none')
  }
  
  # returns a list of coefs matrix
  cat('Multi-resolution analysis... \t')
  wave_coefs <- aaply(.data=spectra(obj), .margins=1, .fun=.mraCoefs, wf = "la8", J = floor(log2(length(obj))), .progress='text')

#   # creating a vector of variances - OBVIOUSLY NOT THE WAY IT SHOULD BE USED!
#   vars <- do.call('rbind', wave_coefs)
#   cat('Computing variances... \t')
#   vars <- aaply(.data=vars, .margins=2, .fun=var, .progress='text')
#   vars_order <- order(vars, decreasing=TRUE)
#   data.frame(wl=wl(obj)[vars_order], var=vars[vars_order])

  # creating matrix of variances (of size NumberOfScales x NumberOfWavelengths)
  vars <- aaply(.data=wave_coefs, .margins=c(2,3), .fun=var, .progress='text')

  # ordering the matrix
  vars_orders <- aaply(.data=vars, .margins=2, .fun=order, decreasing=TRUE)
  scales <- aaply(vars_orders, 1, function(x) which.max(x))

  # making a data.frame to recreate Fig 5 a-b
  require(reshape2)
  require(ggplot2)
  vars.long <- as.data.frame(vars)
  vars.long$scale <- 2^(1:nrow(vars.long))
  vars.long <- reshape2::melt(vars.long, id.vars='scale', value.name="variance")
  vars.long$variable <- as.numeric(as.character(vars.long$variable))
  vars.long <- vars.long[do.call("order", list(vars.long[, 'value', drop = FALSE], decreasing=TRUE)), , drop = FALSE]
  vars.long$order <- 1:nrow(vars.long)
  vars.plot <- ggplot(vars.long) + geom_point(aes(x=order, y=value, colour=scale))

  # making a data.frame to recreate Fig5 c-d
  

  # result - coefs to keep
  coefs <- NULL
  for (j in 1:ncol(vars))
    coefs[j] <- vars[scales[j], j]

  return(vars.long)
}
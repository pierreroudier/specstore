#' Aggregates the spectral information of a Spectra object using 
#' an aggregation function chosen by the user.if some data is also
#' present, it is aggregated using the same function.
#'
#' @param obj an object inheriting from class Spectra
#' @param fun an aggregation function
#' @param ... expressions evaluated in the context of \code{fun}
#'
aggregate_spectra <- function(obj, fun = mean, ...){
  require(plyr)

  # making up an id name from the aggregation function
  id <- as.character(substitute(fun))[1]
  
  # applying the function to the spectra
  nir <- aaply(.data=spectra(obj), .margins=2, .fun=fun, ...)

  res <- Spectra(wl=wl(obj), nir=nir, id=id, units=get_units(obj))

  # if there is some data
  if (is(obj, 'SpectraDataFrame')) {
    data <- aaply(.data=get_data(obj), .margins=2, .fun=fun, ...)
    res <- SpectraDataFrame(res, data=data.frame(matrix(data, nrow=1, dimnames=list(id, names(data)))))
  }
  
  res
}
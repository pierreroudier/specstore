aggregate_spectra <- function(obj,fun=mean, ...){
  
  require(plyr)
  
  res <- obj

  # making up an id from the aggregation function
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
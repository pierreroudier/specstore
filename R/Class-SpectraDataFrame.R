setClass(
  Class='SpectraDataFrame',
  representation=representation(
    'Spectra',
    data='data.frame'
  ),
  prototype=prototype(
    wl=numeric(),
    nir=matrix(),
    id=as.character(NA),
    units=as.character(NA),
    data=data.frame()
  ),
  validity = function(object) {
    if (ncol(object@data) == 0)
      stop("data.frame is empty: use Spectra() to create spectra-only object")
    if (nrow(object@data) != nrow(object@nir))
      stop("number of rows in data.frame and spectra don't match")
    return(TRUE)
  }
)
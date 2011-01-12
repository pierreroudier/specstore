## 
## S4 class defs for storage of soil spectra
##

## Class that stores the whole spectra collection
setClass(
  Class='Spectra',
  representation=representation(
    wl='numeric',
    nir='matrix',
    id='character'
  ),
  prototype=prototype(
    wl=numeric(),
    nir=matrix(),
    id=as.character(NA)
  ),
  validity = function(object) {
    if (!inherits(object@wl, "numeric"))
      stop("wl should be of class numeric")
    if (nrow(object@nir) != length(object@id))
      stop("number of individuals and number of rows in the spectra matrix don't match")
    if (ncol(object@nir) != length(object@wl))
      stop("number of columns in the spectra matrix and number of observed wavelengths don't match")
    return(TRUE)
  }
)

## Class that stores the whole spectra collection along with associated data
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
    data=data.frame()
  ),
  validity = function(object) {
    if (!inherits(object@wl, "numeric"))
      stop("wl should be of class numeric")
    if (!inherits(object@data, "data.frame"))
      stop("data should be of class data.frame")
    if (nrow(object@nir) != length(object@id))
      stop("number of individuals and number of rows in the spectra matrix don't match")
    if (ncol(object@nir) != length(object@wl))
      stop("number of columns in the spectra matrix and number of observed wavelengths don't match")
    if (ncol(object@data) == 0)
      stop("data.frame is empty: use Spectra() to create spectra-only object")
    if (nrow(object@data) != length(object@id))
      stop("number of rows in data.frame and Spectra don't match")
    return(TRUE)
  }
)

## 
## Class initializers
##

# setMethod(
#   f='initialize', 
#   signature='Spectra', 
#   definition=function(.Object, wl=0, spec=list(new(".Spectra"))){
#     .Object@wl <- wl
#     .Object@spec <- spec
#     # call inspector to check for horizon level errors
#     validObject(.Object)
#     return(.Object)
#   }
# )

## basic printing methods for class Spectra
setMethod(
  f='show', 
  signature='Spectra',
  definition=function(object){
    cat("Collection of ", length(object@id),"spectra\n", sep='')
    cat("Wavelength range: ", min(object@wl),"-",max(object@wl),'\n', sep="")
#     cat("Properties available: ", unique(unlist(strsplit(sapply(object@spec, function(x){names(x@data)}),''))),'\n', sep="")
  }
)

##
## Setters
##
setGeneric(
  'spectra',
  package='specstore', 
  def=function(object, value){
    standardGeneric('spectra')
  }
)

# init like this: spectra(foo) <- id ~ wl + ref
setMethod(
  f='spectra',
  signature='data.frame',
  definition=function(object, value){}
#   definition=function(object, value){
#     if (inherits(value, "formula")){
#       mf <- model.frame(value, object)
#     }
#     else 
#       stop('Please use the ~wavelength+reflectance initializer.')
#   }
)
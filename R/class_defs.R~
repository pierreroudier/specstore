## 
## S4 class defs for storage of soil spectra
##

## Class that store a single spectra (not to be used directly)
setClass(
  Class='.Spectra',
  representation=representation(
    ref='numeric',
    id='character',
    data='data.frame'
  ),
  prototype=prototype(
    ref=0,
    id="",
    data=data.frame()
  )
)

## Class that stores the whole spectra collection
setClass(
  Class='Spectra',
  representation=representation(
    wl='numeric',
    spec='list'
  ),
  prototype=prototype(
    wl=0,
    spec=list(new(".Spectra"))
  )
)

## 
## Class initializers
##

setMethod(
  f='initialize', 
  signature='.Spectra', 
  definition=function(.Object, ref=0, id="", data=data.frame()){
    .Object@ref <- ref
    .Object@id <- id
    .Object@data <- data
    # call inspector to check for horizon level errors
    validObject(.Object)
    return(.Object)
  }
)

setMethod(
  f='initialize', 
  signature='Spectra', 
  definition=function(.Object, wl=0, spec=list(new(".Spectra"))){
    .Object@wl <- wl
    .Object@spec <- spec
    # call inspector to check for horizon level errors
    validObject(.Object)
    return(.Object)
  }
)

## basic printing methods for class Spectra
setMethod(
  f='show', 
  signature='Spectra',
  definition=function(object){
    cat("Collection of ", length(object@spec)," spectra\n", sep='')
    cat("Wavelength range: ", min(object@wl),"-",max(object@wl),'\n', sep="")
    cat("Properties available: ", unique(unlist(strsplit(sapply(object@spec, function(x){names(x@data)}),''))),'\n', sep="")
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
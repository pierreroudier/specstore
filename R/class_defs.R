## 
## S4 class defs for storage of soil spectra
##

setClass(
  Class='Spectra',
  representation=representation(
    wl='numeric',
    ref='list',
    id='character',
    data='data.frame'
  ),
  prototype=prototype(
    wl=0,
    ref=list(),
    id="",
    data=data.frame()
  )
)

## 
## Class initializer
##

setMethod(
  f='initialize', 
  signature='Spectra', 
  definition=function(.Object, wl=0, ref=list(), id="", data=data.frame()){
    .Object@wl <- wl
    .Object@ref <- ref
    .Object@id <- id
    .Object@data <- data
    # call inspector to check for horizon level errors
    validObject(.Object)
    return(.Object)
  }
)


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
    # if the wl are given as an integer vector they are translated into a numeric vector
    # for clarity (only one type to manage)
    if (inherits(object@wl, "integer"))
      object@wl <- as.numeric(object@wl)
    if (!inherits(object@wl, "numeric"))
      stop("wl should be of class integer or numeric")
    if (is.character(getSpectralResolution(object@wl)))
      warning(getSpectralResolution(object@wl))
    # If no id is given, id is using the N first integers
    if (is.na(object@id)) {
      object@id <- as.character(seq(1, nrow(object@nir)))
    } else {
      # Test of inconsistent ids when id is specified by the user
      if (nrow(object@nir) != length(object@id))
	stop("number of individuals and number of rows in the spectra matrix don't match")
    }
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
#     if (!inherits(object@wl, "numeric") & !inherits(object@wl, "integer"))
#       stop("wl should be of class integer or numeric")
#     if (is.character(getSpectralResolution(object@wl)))
#       warning(getSpectralResolution(object@wl))
#     # If no id is given, id is using the N first integers
#     if (is.na(object@id)) {
#       object@id <- as.character(seq(1, nrow(object@nir)))
#     } else {
#       # Test of inconsistent ids when id is specified by the user
#       if (nrow(object@nir) != length(object@id))
# 	stop("number of individuals and number of rows in the spectra matrix don't match")
#     }
#     if (ncol(object@nir) != length(object@wl))
#       stop("number of columns in the spectra matrix and number of observed wavelengths don't match")
    if (ncol(object@data) == 0)
      stop("data.frame is empty: use Spectra() to create spectra-only object")
    if (nrow(object@data) != nrow(object@nir))
      stop("number of rows in data.frame and spectra don't match")
    return(TRUE)
  }
)

## basic printing methods for class Spectra

## Returns spectral resolution of the wavelengths

setGeneric("getSpectralResolution", function(object, ...){
  standardGeneric("getSpectralResolution")
}
)

# In getSpectralResolution, had to put a round - otherwise diff() picks some unsignificant values
getSpectralResolution.numeric <- function(object, digits=10, ...){
  unique(round(diff(object), digits=digits))
}

getSpectralResolution.Spectra <- function(object, digits=10, ...){
  x <- object@wl
  unique( round( diff(x), digits=digits) )
#   if (length(res) > 1) res <- "irregular wavelength spacing"
}

setMethod("getSpectralResolution", "numeric", getSpectralResolution.numeric)
setMethod("getSpectralResolution", "integer", getSpectralResolution.numeric)
setMethod("getSpectralResolution", "Spectra", getSpectralResolution.Spectra)
setMethod("getSpectralResolution", "SpectraDataFrame", getSpectralResolution.Spectra)

## summary of the Spectra* objects

setGeneric("summary", function(object, ...){
  standardGeneric("summary")
}
)

summary.Spectra <- function (object, ...){
    obj = list()
    obj[["class"]] = class(object)
    obj[["wl"]] = object@wl
    obj[["id"]] = object@id
    obj[["nir"]] = object@nir
    if ("data" %in% slotNames(object)) {
        if (ncol(object@data) > 1) 
            obj[["data"]] = summary(object@data)
        else obj[["data"]] = summary(object@data[[1]])
    }
    else obj[["data"]] = NULL
    class(obj) = "summary.Spectra"
    obj
}

setMethod("summary", "summary.Spectra", summary.Spectra)

print.summary.Spectra = function(x, ...) {
    cat(paste("Object of class ", x[["class"]], "\n", sep = ""))
    cat("Wavelength range: ")
    cat(min(x[["wl"]], na.rm=TRUE), " to ", max(x[["wl"]], na.rm=TRUE)," nm \n", sep="")
    SpectralResolution <- getSpectralResolution(x[["wl"]])
    if (length(SpectralResolution) > 1) {
      cat("Spectral resolution: irregular wavelength spacing\n")
    } else {
      cat("Spectral resolution: ", SpectralResolution , " nm\n", sep="")
    }
    cat(paste("Number of samples:", length(x[["id"]]), "\n"))
    if (!is.null(x$data)) {
        cat("Data attributes:\n")
        print(x$data)
    }
    invisible(x)
}

## Print methods

setMethod(
  f='show', 
  signature='Spectra',
  definition=function(object){
    cat("Collection of ", length(object@id)," spectra\n", sep='')
    cat("Wavelength range: ", min(object@wl, na.rm=TRUE),"-",max(object@wl, na.rm=TRUE),' nm \n', sep="")
    SpectralResolution <- getSpectralResolution(object)
    if (length(SpectralResolution) > 1) {
      cat("Spectral resolution: irregular wavelength spacing\n")
    } else {
      cat("Spectral resolution: ", SpectralResolution , " nm\n", sep="")
    }
  }
)

setMethod(
  f='show', 
  signature='SpectraDataFrame',
  definition=function(object){
    cat("Collection of ", length(object@id)," spectra\n", sep='')
    cat("Wavelength range: ", min(object@wl, na.rm=TRUE),"-",max(object@wl, na.rm=TRUE),' nm \n', sep="")
    SpectralResolution <- getSpectralResolution(object)
    if (length(SpectralResolution) > 1) {
      cat("Spectral resolution: irregular wavelength spacing\n")
    } else {
      cat("Spectral resolution: ", SpectralResolution , " nm\n", sep="")
    }
    cat("Data attributes:\n")
    print((object@data))
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
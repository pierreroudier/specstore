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
    if (is(object@wl, "integer"))
      object@wl <- as.numeric(object@wl)
    if (!inherits(object@wl, "numeric"))
      stop("wl should be of class integer or numeric")
    if ((length(object@id) > 0) & (nrow(object@nir) != length(object@id)))
      stop("number of individuals and number of rows in the spectra matrix don't match")
    if ((length(object@wl > 1) & (ncol(object@nir) != length(object@wl))))
      stop("number of columns in the spectra matrix and number of observed wavelengths don't match")
    if (length(unique(object@id)) != length(object@id))
      stop("The ids of the samples need to be unique")
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
    if (ncol(object@data) == 0)
      stop("data.frame is empty: use Spectra() to create spectra-only object")
    if (nrow(object@data) != nrow(object@nir))
      stop("number of rows in data.frame and spectra don't match")
    return(TRUE)
  }
)

## 
## METHODS
##

## Initializers
##

"Spectra" <- function(wl=numeric(), nir=matrix(), id=as.character(NA)) {
  # if the wl are given as an integer vector they are translated into a numeric vector
  # for clarity (only one type to manage)
  if (is(wl, "integer"))
    wl <- as.numeric(wl)
  if (is(nir, 'data.frame'))
    nir <- as.matrix(nir)
  if (!is(id, "character"))
    id <- as.character(id)
  # If no id is given
  if (all(is.na(id))) {
    # If the object is void
    if (length(nir) == 1)
      id <- as.character(NULL)
    # if a matrix is here
    else
      id <- as.character(seq(1, nrow(nir)))
  } else {
    # Test of inconsistent ids when id is specified by the user
    if (nrow(nir) != length(id))
      stop("number of individuals and number of rows in the spectra matrix don't match")
  }
  if ((length(wl) > 1) & (ncol(nir) != length(wl)))
    stop("number of columns in the spectra matrix and number of observed wavelengths don't match")
  rownames(nir) <- id
  colnames(nir) <- wl
  new("Spectra", wl=wl, nir=nir, id=id)
}

"SpectraDataFrame" <- function(wl=numeric(), nir=matrix(), id=as.character(NA), data=data.frame()) {
  # Initialization from an existing Spectra object
  if (is(wl, 'Spectra')){
    wl <- wl@wl
    nir <- wl@nir
    id <- wl@id
  }
  else {
    # if the wl are given as an integer vector they are translated into a numeric vector
    # for clarity (only one type to manage)
    if (is(wl, "integer"))
      wl <- as.numeric(wl)
    if (is(nir, 'data.frame'))
      nir <- as.matrix(nir)
    if (!is(id, "character"))
      id <- as.character(id)
    # If no id is given
    if (all(is.na(id))) {
      # If the object is void
      if (length(nir) == 1) 
	id <- as.character(NULL)
      # if a matrix is here
      else 
	id <- as.character(seq(1, nrow(nir)))
    } else {
      # Test of inconsistent ids when id is specified by the user
      if (nrow(nir) != length(id))
	stop("number of individuals and number of rows in the spectra matrix don't match")
    }
    if ((length(wl) > 1) & (ncol(nir) != length(wl)))
      stop("number of columns in the spectra matrix and number of observed wavelengths don't match")
  }
  if (is(data, "numeric") | is(data, "integer"))
      data <- as.data.frame(data)
  colnames(nir) <- wl
  rownames(nir) <- id
  rownames(data) <- id
  new("SpectraDataFrame", wl=wl, nir=nir, id=id, data=data)
}

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
    cat("Set of ", length(x[['id']])," spectra\n", sep = "")
    if (length(x[['id']]) > 0){
      cat("Wavelength range: ")
      cat(min(x[["wl"]], na.rm=TRUE), " to ", max(x[["wl"]], na.rm=TRUE)," nm \n", sep="")
      SpectralResolution <- getSpectralResolution(x[["wl"]])
      if (length(SpectralResolution) > 1) 
	cat("Spectral resolution: irregular wavelength spacing\n")
      else {
	if (length(SpectralResolution) == 0)
	  cat("Spectral resolution: NA\n")
	else 
	  cat("Spectral resolution: ", SpectralResolution , " nm\n", sep="")
      }
      if (!is.null(x$data)) {
	  cat("Data attributes:\n")
	  print(x$data)
      }
    }
    invisible(x)
}

setMethod("print", "summary.Spectra", print.summary.Spectra)

## Print methods

setMethod(
  f='show', 
  signature='Spectra',
  definition=function(object){
    cat(paste("Object of class ", class(object), "\n", sep = ""))
    cat("Set of ", length(object@id)," spectra\n", sep='')
    if (length(object@id) > 0){
      cat("Wavelength range: ", min(object@wl, na.rm=TRUE),"-",max(object@wl, na.rm=TRUE),' nm \n', sep="")
      SpectralResolution <- getSpectralResolution(object)
      if (length(SpectralResolution) > 1) 
	cat("Spectral resolution: irregular wavelength spacing\n")
      else {
	if (length(SpectralResolution) == 0)
	  cat("Spectral resolution: NA\n")
	else 
	  cat("Spectral resolution: ", SpectralResolution , " nm\n", sep="")
      }
    }
  }
)

setMethod(
  f='show', 
  signature='SpectraDataFrame',
  definition=function(object){
    cat(paste("Object of class ", class(object), "\n", sep = ""))
    cat("Set of ", length(object@id)," spectra\n", sep='')
    if (length(object@id) > 0){
      cat("Wavelength range: ", min(object@wl, na.rm=TRUE),"-",max(object@wl, na.rm=TRUE),' nm \n', sep="")
      SpectralResolution <- getSpectralResolution(object)
      if (length(SpectralResolution) > 1) 
	cat("Spectral resolution: irregular wavelength spacing\n")
      else {
	if (length(SpectralResolution) == 0)
	  cat("Spectral resolution: NA\n")
	else 
	  cat("Spectral resolution: ", SpectralResolution , " nm\n", sep="")
      }
    }
    cat("Data attributes:\n")
    print((object@data))
  }
)

## Accessors

setGeneric("get_data", function(object, ...){
  standardGeneric("get_data")
  }
)

get_data.SpectraDataFrame <- function(object){
    object@data
}

setMethod("get_data", "SpectraDataFrame", get_data.SpectraDataFrame)

setGeneric("get_spectra", function(object, ...){
  standardGeneric("get_spectra")
  }
)

get_spectra.Spectra<- function(object){
    object@nir
}

setMethod("get_spectra", "Spectra", get_spectra.Spectra)
setMethod("get_spectra", "SpectraDataFrame", get_spectra.Spectra)
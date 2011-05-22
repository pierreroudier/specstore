#' Constructor for the Spectra class
#'
#' @param wl a numeric vector giving the wavelengths at with the spectra have been measured
#' @param nir a \code{matrix} or a \code{data.frame} object giving the spectra values for each sample
#' @param id a vector giving the unique id of each sample in the collection
#' @param units a character giving the unit in which the wavelengths values are expressed
#' @return a new Spectra object
#' @export
#' @author Pierre Roudier \url{pierre.roudier@@gmail.com}
"Spectra" <- function(wl=numeric(), nir=matrix(), id=as.character(NA), units="nm") {
  # if the wl are given as an integer vector they are translated into a numeric vector
  # for clarity (only one type to manage)
  if (is(wl, "integer"))
    wl <- as.numeric(wl)

  if (is(nir, 'data.frame'))
    nir <- as.matrix(nir)

  if (!is(id, "data.frame"))
    id <- data.frame(id = id)

  # If no id is given
  if (all(is.na(id))) {
    # If the object is void
    if (length(nir) == 1)
      id <- as.character(NULL)
    # if a matrix is here
    else
      id <- as.character(seq(1, nrow(nir)))
  } 

  # if ids are actually given by the user
  else {
    # Test of inconsistent ids when id is specified by the user

    # if theres only one spectra
    if (is.null(nrow(nir))) { 
      if (nrow(id) != 1)
	stop("number of individuals and number of rows in the spectra matrix don't match")
      if ((length(wl) > 1) & (length(nir) != length(wl)))
        stop("number of columns in the spectra matrix and number of observed wavelengths don't match")
      nir <- matrix(nir, nrow=1)
    }

    # if theres more than one specta
    else {
      if (nrow(nir) != nrow(id))
        stop("number of individuals and number of rows in the spectra matrix don't match")
      if ((length(wl) > 1) & (ncol(nir) != length(wl)))
        stop("number of columns in the spectra matrix and number of observed wavelengths don't match")
      colnames(nir) <- wl
      rownames(nir) <- as.vector(do.call('rbind', id))
    }
  }

  # consistency nimber of wl/number of cols in the NIR matrix
  if ((length(wl) > 1) & (ncol(nir) != length(wl)))
    stop("number of columns in the spectra matrix and number of observed wavelengths don't match")
  rownames(nir) <- as.vector(do.call('rbind', id))
  colnames(nir) <- wl
  new("Spectra", wl = wl, nir = nir, id = id, units = units)
}

## SUMMARY

if (!isGeneric("summary"))
  setGeneric("summary", function(object, ...)
    standardGeneric("summary"))

#' @param object an object inheriting from \code{Spectra} 
#' @param ... Ignored
#' @method summary Spectra
#' @rdname Spectra
#' @author Pierre Roudier \url{pierre.roudier@@gmail.com}
summary.Spectra <- function (object, ...){
    obj = list()
    obj[["class"]] = class(object)
    obj[["wl"]] = object@wl
    obj[["id"]] = object@id
    obj[["nir"]] = object@nir
    obj[["units"]] = object@units
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

#' @param object a summary for an object inheriting from \code{Spectra}
#' @param ... Ignored
#' @method print summary.Spectra
#' @author Pierre Roudier \url{pierre.roudier@@gmail.com}
print.summary.Spectra = function(x, ...) {
    cat(paste("Object of class ", x[["class"]], "\n", sep = ""))
    cat("Set of ", nrow(x[['id']])," spectra\n", sep = "")
    if (nrow(x[['id']]) > 0){
      cat("Wavelength range: ")
      cat(min(x[["wl"]], na.rm=TRUE), " to ", max(x[["wl"]], na.rm=TRUE)," ", x[["units"]], "\n", sep="")
      SpectralResolution <- get_resolution(x[["wl"]])
      if (length(SpectralResolution) > 1) 
	cat("Spectral resolution: irregular wavelength spacing\n")
      else {
	if (length(SpectralResolution) == 0)
	  cat("Spectral resolution: NA\n")
	else 
	  cat("Spectral resolution: ", SpectralResolution , " ",  x[["units"]], "\n", sep="")
      }
      if (!is.null(x$data)) {
	cat("Data attributes:\n")
	print(x$data)
      }
    }
    invisible(x)
}

setMethod("print", "summary.Spectra", print.summary.Spectra)

## PRINT

#' @param object an object inheriting from \code{Spectra} 
#' @method show Spectra
#' @rdname Spectra-methods
#' @author Pierre Roudier \url{pierre.roudier@@gmail.com}
setMethod(
  f='show', 
  signature='Spectra',
  definition=function(object){
    cat(paste("Object of class ", class(object), "\n", sep = ""))
    cat("Set of ", nrow(object@id)," spectra\n", sep='')
    if (nrow(object@id) > 0){
      cat("Wavelength range: ", min(object@wl, na.rm=TRUE),"-",max(object@wl, na.rm=TRUE)," ", object@units, "\n", sep="")
      SpectralResolution <- get_resolution(object)
      if (length(SpectralResolution) > 1) 
        cat("Spectral resolution: irregular wavelength spacing\n")
      else {
        if (length(SpectralResolution) == 0)
          cat("Spectral resolution: NA\n")
        else 
          cat("Spectral resolution: ", SpectralResolution , " ", object@units, "\n", sep="")
      }
    }
    if ("data" %in% slotNames(object)) {
      cat("Data attributes:\n")
      print((object@data))
    }
  }
)

## coercition methods

#' @param x an object inheriting from \code{Spectra} 
#' @param ... Ignored
#' @return a \code{data.frame} object
#' @author Pierre Roudier \url{pierre.roudier@@gmail.com}
as.data.frame.Spectra <- function(x, ...)  {
  df <- as.data.frame(spectra(x))
  names(df) <- wl(x)
  df
}

setAs("Spectra", "data.frame", function(from)
	as.data.frame.Spectra(from))

## Accessing data

# Getting the spectra matrix
if (!isGeneric("spectra"))
  setGeneric("spectra", function(object, ...)
    standardGeneric("spectra"))

#' Returns the matrix of the spectra in the collection
#'
#' @param object an object inheriting from \code{Spectra} 
#' @return a \code{matrix} object
#'
#' @export
#' @author Pierre Roudier \url{pierre.roudier@@gmail.com}
setMethod("spectra", "Spectra", 
  function(object)
    object@nir
)

# Getting the wavelengths
if (!isGeneric("wl"))
  setGeneric("wl", function(object, ...)
    standardGeneric("wl"))

#' Returns the wavelengths at which the spectra have been recorded
#'
#' @param object an object inheriting from \code{Spectra} 
#' @return a \code{numeric} object
#'
#' @export
#' @author Pierre Roudier \url{pierre.roudier@@gmail.com}
setMethod("wl", "Spectra", 
  function(object)
    object@wl
)

# Getting the ids
if (!isGeneric("id"))
  setGeneric("id", function(object, ...)
    standardGeneric("id"))

#' Returns the ids of each spectra in the collection
#'
#' @param object an object inheriting from \code{Spectra} 
#' @return a \code{character} object
#'
#' @export
#' @author Pierre Roudier \url{pierre.roudier@@gmail.com}
setMethod("id", "Spectra", 
  function(object)
    object@id
)

# Getting the units
if (!isGeneric("get_units"))
  setGeneric("get_units", function(object, ...)
    standardGeneric("get_units"))

#' Returns the unit in which the wavelengths values are expressed
#'
#' @param object an object inheriting from \code{Spectra} 
#' @return a \code{character} 
#'
#' @export 
#' @author Pierre Roudier \url{pierre.roudier@@gmail.com}
setMethod("get_units", "Spectra", 
  function(object)
    object@units
)
    
#' Returns the number of wavelengths in the object
#'
#' @param object an object inheriting from \code{Spectra} 
#' @return a vector
#'
#' @export 
#' @author Pierre Roudier \url{pierre.roudier@@gmail.com}
setMethod(f='length', signature='Spectra',
  definition=function(x)
    ncol(x@nir)
)

#' Returns the number of samples in the object
#'
#' @param object an object inheriting from \code{Spectra} 
#' @return a vector
#'
#' @export 
#' @author Pierre Roudier \url{pierre.roudier@@gmail.com}
setMethod(f='nrow', signature='Spectra',
definition=function(x)
    nrow(id(x))
)

## Returns spectral resolution of the wavelengths

if (!isGeneric("get_resolution"))
  setGeneric("get_resolution", function(object, ...)
    standardGeneric("get_resolution"))

#' Returns the spectral resolution of an object
#'
#' @param object a vector
#' @param digits the number of significant digits 
#' @return a vector
#'
#' @method get_resolution numeric 
#' @author Pierre Roudier \url{pierre.roudier@@gmail.com}
get_resolution.numeric <- function(object, digits = 10, ...){
  unique(round(diff(object), digits = digits)) # round - otherwise diff() picks some unsignificant values
}

#' Returns the spectral resolution of an object
#'
#' @param object an object inheriting from \code{Spectra} 
#' @param digits the number of significant digits 
#' @return a vector
#'
#' @method get_resolution Spectra
#' @author Pierre Roudier \url{pierre.roudier@@gmail.com}
get_resolution.Spectra <- function(object, digits=10, ...){
  x <- wl(object)
  unique( round( diff(x), digits=digits) )
}

setMethod("get_resolution", "numeric", get_resolution.numeric)
setMethod("get_resolution", "integer", get_resolution.numeric)
setMethod("get_resolution", "Spectra", get_resolution.Spectra)

## overloads

#' extract parts of Spectra objects
#'
#' @name [
#' @aliases [, Spectra-method
#' @docType methods
#' @rdname extract-methods
#'
#' @author Pierre Roudier \url{pierre.roudier@@gmail.com}
setMethod("[", c("Spectra", "ANY", "missing"), 
  function(x, i, j, ... ) {
    missing.i = missing(i)
    missing.j = missing(j)
    nargs = nargs() # e.g., a[3,] gives 2 for nargs, a[3] gives 1.
    if (missing.i && missing.j) {
      i = TRUE
      j = TRUE
    } else if (missing.j && !missing.i) { 
      if (nargs == 2) {
        j = i
        i = TRUE
      } else {
        j = TRUE
      }
    } else if (missing.i && !missing.j)
      i = TRUE
    if (any(is.na(i))) 
      stop("NAs not permitted in row index")
    if (is.character(i)) {
      i <- which(x@id %in% i) 
    }
  #   if (!isTRUE(j)) # i.e., we do some sort of column selection => not sure on what we select (nir? data?)
    if ("data" %in% slotNames(x)) {
      if (length(names(x@data)) == 1) {
        df <- data.frame(x@data[i,])
        names(df) <- names(x@data)
      } 
      else
        df <- x@data[i,]
      res <- SpectraDataFrame(wl=x@wl, nir=x@nir[i,], id=x@id[i, 1, drop = FALSE], data=df)
    }
    else 
      res <- Spectra(wl=x@wl, nir=x@nir[i,], id=x@id[i, 1, drop = FALSE])
    res  
  }
)

## Upgrade a Spectra object to a SpectraDataFrame

if (!isGeneric('data<-'))
  setGeneric('data<-', function(object, value) 
    standardGeneric('data<-'))

#' 
setReplaceMethod("data", "Spectra",
  function(object, value) {
    if (!inherits(value, "data.frame")) 
      stop('invalid initialization for SpectraDataFrame object')
    SpectraDataFrame(object, data=value)
  }
)

## Modifying the spectra matrix
## WORK NEEDED!!

# "set_spectra<-.Spectra" <- function(obj, id=obj@id, value){
#   if (all(id %in% obj@id)){
#     id.lines <- which(obj@id %in% id)
#     
#     if (nrow(value) != nrow(id))
#       stop("the matrix you try to substitute does not have suitable dimensions")
#     if (ncol(value) != length(obj@wl))
#       stop("inconsistent wavelengths")
#     
#     obj@nir[id.lines,] <- value
#   }
#   else 
#     stop('the proposed ids are not matching the object ids')
#   obj
# }

## Adding objects together
# Maybe to be moved into the Spectra() and SpectraDataFrame() method.

if (!isGeneric("add"))
  setGeneric("add", function(x, y, ...)
    standardGeneric("add"))

#' Adds two Spectra objects together
#'
#' @author Pierre Roudier \url{pierre.roudier@@gmail.com}
.add.Spectra <- function(x, y){
  tmp <- list()

  if (identical(x@wl, y@wl)) 
    tmp$wl <- x@wl
  else 
    stop('You can not add objects with different wavelength ranges')

  if (identical(ncol(x@wl), ncol(y@wl)))
    tmp$nir <- rbind(x@nir, y@nir)
  else 
    stop('You can not add objects with different wavelength ranges')

  if (!any(x@id %in% y@id)) 
    tmp$id <- rbind(x@id, y@id)
  else 
    stop('You can not add objects with overlapping IDs')

  if (x@units %in% y@units) 
    tmp$units <- x@units
  else 
    stop('You can not add objects with different wavelength units')

  if (("data" %in% slotNames(x)) & ("data" %in% slotNames(y))) {
    tmp$data <- join(x@data, y@data, type="full")
    res <- SpectraDataFrame(wl=tmp$wl, nir=tmp$nir, id=tmp$id, units=tmp$units, data=tmp$data)
  }
  else 
    res <- Spectra(wl=tmp$wl, nir=tmp$nir, id=tmp$id, units=tmp$units)

  res
}

add.Spectra <- function(...){
  dotargs <- list(...)
  if ( !all(sapply(dotargs, function(x) is(x,"Spectra") )) )
    stop('the arguments must be Spectra objects')

  res <- dotargs[[1]]
  if (nargs() >= 2) {
    for (i in 2:length(dotargs))
      res <- .add.Spectra(res, dotargs[[i]])
  }
  res
}

setMethod("add", signature=c("Spectra", "Spectra"), 
  function(x,y,...) add.Spectra(x, y, ...))

setMethod("add", signature=c("SpectraDataFrame", "SpectraDataFrame"), 
  function(x,y,...) add.Spectra(x, y, ...))


# ## Transform the Spectra object
# 
# transform.Spectra <- function (obj, condition, ...){
#   # for that class the transform is focusing exclusively on the NIR spectra
#   require(reshape2)
#   require(stringr)
#   condition_call <- substitute(condition)
#   if (!str_detect(deparse(condition_call), "nir"))
#     stop('use the nir variable in the condition call')
#   nir <- melt(spectra(obj), value.name="nir", varnames=c('id','wl'))
#   nir <- transform(nir, ...)
#   nir <- matrix(nir$nir, nrow=nrow(obj), ncol=length(wl(obj)))
#   rownames(nir) <- id(obj)
#   colnames(nir) <- wl(obj)  
#   Spectra(wl=wl(obj), nir=nir, id=id(obj), units=get_units(obj))
# }

# setMethod("transform", "Spectra", transform.Spectra)

#`  Mutate a Spectra object by adding new or replacing existing columns.
#`
#` This function is a simple port of the \code{\link{mutate}} function in the
#' plyr package to the Spectra objects, which it wraps.
#'
#' This function is very similar to \code{\link{transform}} but it executes
#' the transformations iteratively so that later transformations can use the
#' columns created by earlier transformations. Like transform, unnamed
#' components are silently dropped.
#'
#' Mutate seems to be considerably faster than transform for large data
#' frames.
#'
#' @param obj an object inheriting from the \code{Spectra} class
#' @param ... named parameters giving definitions of new columns
#' @seealso \code{\link{mutate}}
#' @author Pierre Roudier \url{pierre.roudier@@gmail.com}
#' @export
mutate.Spectra <- function (.data, ...){
  
  condition_call <- substitute(list(...))

  # you want to affect the spectra
  if (!str_detect(deparse(condition_call), "nir")) 
    stop('You must use the nir variable in the condition call')

  nir <- melt(spectra(.data), varnames=c('id','wl'))
  names(nir)[which(names(nir) == 'value')] <- 'nir'
  nir <- mutate(nir, ...)
  nir <- matrix(nir$nir, nrow=nrow(.data), ncol=length(wl(.data)))

  rownames(nir) <- id(.data)
  colnames(nir) <- wl(.data)  

  Spectra(wl=wl(.data), nir=nir, id=id(.data), units=get_units(.data))
}

setMethod("mutate", "Spectra", mutate.Spectra)

#` Melting the spectra matrix
#'
#' @param obj an object inheriting from the \code{Spectra} class
#' @export
#' @author Pierre Roudier \url{pierre.roudier@@gmail.com}
#` @import reshape2
melt_spectra <- function(obj, ...){

  # if obj is Spectra* class
  if (inherits(obj, 'Spectra')){
    x <- spectra(obj)
  }
  # if obj is a data.frame or a matrix (ass returned by spectra)
  else {
    if ((inherits(obj, 'data.frame')) | (inherits(obj, 'matrix'))){
      x <- obj
    } 
    else 
      stop('The object you try to melt either be a matrix or data.frame, or a Spectra* object')
  }
  res <- reshape2:::melt.array(x, varnames=c('id', 'wl'), value.name="nir")
  names(res)[3] <- "nir" # tmp fix - waiting for fix upstream in reshape2
  res    
}
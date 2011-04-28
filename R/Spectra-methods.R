## INITIALIZER

"Spectra" <- function(wl=numeric(), nir=matrix(), id=as.character(NA), units="nm") {
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
  } 
  else {
    # Test of inconsistent ids when id is specified by the user
    if (is.null(nrow(nir))) { # if theres only one spectra
      if (length(id) != 1)
        stop("number of individuals and number of rows in the spectra matrix don't match")
      if ((length(wl) > 1) & (length(nir) != length(wl)))
        stop("number of columns in the spectra matrix and number of observed wavelengths don't match")
      nir <- matrix(nir, nrow=1)
    } 
    else {
      if (nrow(nir) != length(id))
        stop("number of individuals and number of rows in the spectra matrix don't match")
      if ((length(wl) > 1) & (ncol(nir) != length(wl)))
        stop("number of columns in the spectra matrix and number of observed wavelengths don't match")
      colnames(nir) <- wl
      rownames(nir) <- id
    }
  }
  if ((length(wl) > 1) & (ncol(nir) != length(wl)))
    stop("number of columns in the spectra matrix and number of observed wavelengths don't match")
  rownames(nir) <- id
  colnames(nir) <- wl
  new("Spectra", wl=wl, nir=nir, id=id, units=units)
}

## SUMMARY

if (!isGeneric("summary"))
  setGeneric("summary", function(object, ...)
    standardGeneric("summary"))

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

print.summary.Spectra = function(x, ...) {
    cat(paste("Object of class ", x[["class"]], "\n", sep = ""))
    cat("Set of ", length(x[['id']])," spectra\n", sep = "")
    if (length(x[['id']]) > 0){
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

setMethod(
  f='show', 
  signature='Spectra',
  definition=function(object){
    cat(paste("Object of class ", class(object), "\n", sep = ""))
    cat("Set of ", length(object@id)," spectra\n", sep='')
    if (length(object@id) > 0){
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

as.data.frame.Spectra = function(x, ...)  {
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

setMethod("spectra", "Spectra", 
  function(object)
    object@nir
)

# Getting the wavelengths
if (!isGeneric("wl"))
  setGeneric("wl", function(object, ...)
    standardGeneric("wl"))

setMethod("wl", "Spectra", 
  function(object)
    object@wl
)

# Getting the ids
if (!isGeneric("id"))
  setGeneric("id", function(object, ...)
    standardGeneric("id"))

setMethod("id", "Spectra", 
  function(object)
    object@id
)

# Getting the units
if (!isGeneric("get_units"))
  setGeneric("get_units", function(object, ...)
    standardGeneric("get_units"))

setMethod("get_units", "Spectra", 
  function(object)
    object@units
)
    
# overload length() to give us the number of wl
setMethod(f='length', signature='Spectra',
  definition=function(x)
    ncol(x@nir)
)

# overload nrow() to give us the number of samples
setMethod(f='nrow', signature='Spectra',
definition=function(x)
    length(id(x))
)

## Returns spectral resolution of the wavelengths

if (!isGeneric("get_resolution"))
  setGeneric("get_resolution", function(object, ...)
    standardGeneric("get_resolution"))

get_resolution.numeric <- function(object, digits=10, ...){
  unique(round(diff(object), digits=digits)) # round - otherwise diff() picks some unsignificant values
}

get_resolution.Spectra <- function(object, digits=10, ...){
  x <- wl(object)
  unique( round( diff(x), digits=digits) )
}

setMethod("get_resolution", "numeric", get_resolution.numeric)
setMethod("get_resolution", "integer", get_resolution.numeric)
setMethod("get_resolution", "Spectra", get_resolution.Spectra)

## overloads

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
      res <- SpectraDataFrame(wl=x@wl, nir=x@nir[i,], id=x@id[i], data=df)
    }
    else 
      res <- Spectra(wl=x@wl, nir=x@nir[i,], id=x@id[i])
    res  
  }
)

## Upgrade a Spectra object to a SpectraDataFrame

if (!isGeneric('data<-'))
  setGeneric('data<-', function(object, value) 
    standardGeneric('data<-'))

setReplaceMethod("data", "Spectra",
  function(object, value) {
    if (!inherits(value, "data.frame")) 
      stop('invalid initialization for SoilProfile object')
    SpectraDataFrame(object, data=value)
  }
)

## Modifying the spectra matrix
## WORK NEEDED!!

"set_spectra<-.Spectra" <- function(obj, id=obj@id, value){
  if (all(id %in% obj@id)){
    id.lines <- which(obj@id %in% id)
    
    if (nrow(value) != length(id))
      stop("the matrix you try to substitute does not have suitable dimensions")
    if (ncol(value) != length(obj@wl))
      stop("inconsistent wavelengths")
    
    obj@nir[id.lines,] <- value
  }
  else 
    stop('the proposed ids are not matching the object ids')
  obj
}

## Adding objects together
# Maybe to be moved into the Spectra() and SpectraDataFrame() method.

if (!isGeneric("add"))
  setGeneric("add", function(x, y, ...)
    standardGeneric("add"))

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
    tmp$id <- c(x@id, y@id)
  else 
    stop('You can not add objects with overlapping IDs')

  if (x@units %in% y@units) 
    tmp$units <- x@units
  else 
    stop('You can not add objects with different wavelength units')

  if (("data" %in% slotNames(x)) & ("data" %in% slotNames(y))) {
    require(plyr)
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



## Transform the Spectra object

transform.Spectra <- function (obj, condition, ...){
  # for that class the transform is focusing exclusively on the NIR spectra
  require(reshape2)
  require(stringr)
  condition_call <- substitute(condition)
  if (!str_detect(deparse(condition_call), "nir"))
    stop('use the nir variable in the condition call')
  nir <- melt(spectra(obj), value.name="nir", varnames=c('id','wl'))
  nir <- transform(nir, ...)
  nir <- matrix(nir$nir, nrow=nrow(obj), ncol=length(wl(obj)))
  rownames(nir) <- id(obj)
  colnames(nir) <- wl(obj)  
  Spectra(wl=wl(obj), nir=nir, id=id(obj), units=get_units(obj))
}

## Mutate the SpectraDataFrame object	

mutate.Spectra <- function (obj, ...){
  require(reshape2)
  require(stringr)
  require(plyr)

  condition_call <- substitute(list(...))

  # you want to affect the spectra
  if (!str_detect(deparse(condition_call), "nir")) 
    stop('You must use the nir variable in the condition call')

  nir <- melt(spectra(obj), varnames=c('id','wl'))
  names(nir)[which(names(nir) == 'value')] <- 'nir'
  nir <- mutate(nir, ...)
  nir <- matrix(nir$nir, nrow=nrow(obj), ncol=length(wl(obj)))

  rownames(nir) <- id(obj)
  colnames(nir) <- wl(obj)  

  Spectra(wl=wl(obj), nir=nir, id=id(obj), units=get_units(obj))
}

## Melting the spectra matrix

melt_spectra <- function(obj, ...){
  require(reshape2)
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
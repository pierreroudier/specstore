.findSpectraCols <- function(data, wl, ...){
  
  # for each colname, we check if a part of it is in the spectral range
  # first implementation - simple test
  ind_col_spectra <- which(names(data) %in% as.character(wl))
  if (length(ind_col_spectra) == 0) {
    # more tricky - looking for X350, etc:
    require(plyr)
    require(stringr)
    ind_col_spectra <- aaply(names(data), 1, .fun=function(x) any(str_detect(x, as.character(wl))), ...)
  }
  if (length(ind_col_spectra) == 0)
    stop('No columns found.')
  which(ind_col_spectra)
}

## setter for Spectra objects

# the idea here is to have a quick setter - like coordinates(...) in sp.
#
# e.g. wl(mydf) <- id ~ 350:2500
#
if (!isGeneric('wl<-'))
  setGeneric('wl<-', function(object, value) 
    standardGeneric('wl<-'))

setReplaceMethod("wl", "data.frame",
  function(object, value) {
    # finding which cols contrain the spectra
    ind_nir <- .findSpectraCols(data=object, wl=value, .progress='text')
    nir <- object[, ind_nir]

    res <- Spectra(wl=as.numeric(value), nir=as.matrix(nir))
    
    # If there are some columns left, we use them to initiate a SpectraDataFrame object
    if (ncol(nir) < ncol(object))
      data(res) <- object[, -ind_nir]
      
  res
  }
)

if (!isGeneric('id<-'))
  setGeneric('id<-', function(object, value) 
    standardGeneric('id<-'))

setReplaceMethod("id", "Spectra",
  function(object, value) {
    if (length(value) != nrow(object))
      stop("length of the new ID does not match the length of the object")
    if (!is.character(value))
      value <- as.character(value)
    object@id <- value
    object
  }
)

## id(mySDF) <- ~ myIDcolname
##
setReplaceMethod("id", "SpectraDataFrame",
  function(object, value) {
    if (is(value, 'formula')){
      mf <- model.frame(formula=value, data=object)
      if (ncol(mf) == 1) {
	# removing the id col from the data slot
	object@data <- object@data[, -which(names(object@data) == names(mf))]
	# assigning the id slot
	object@id <- as.character(mf[, 1])
      }
      else
	stop('wrong id initialisation')
    }
    else{
      if (inherits(value, 'numeric')) {
	if (length(value) != length(object))
	  stop("length of the new ID does not match the length of the object")
	if (!is.character(value))
	  value <- as.character(value)
	object@id <- value
      }
      else {
	object@id <- as.character(value)
      }
    }
    object
  }
)

##

# df <-read.csv('path/to/my/data.csv')
# wl(df) <- 350:2500
# id(df) <- ~ id
# data(df) <- someDataFrame (for a Spectra object)
#
# ...
#
# coordinates(df) <- ~x+y
# proj4string(df) <- ...
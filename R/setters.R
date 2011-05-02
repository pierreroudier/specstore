.guessWl <- function(string){
  # simply returns the wavelengths from a string
  as.numeric(laply(.data=str_extract_all(string, "\\d"), .fun=paste, collapse=""))
}

# Tries to find the columns of the spectra from the spectral range
.findSpectraCols <- function(data, wl, ...){
  
  # for each colname, we check if a part of it is in the spectral range
  # first implementation - simple test
  ind_col_spectra <- which(names(data) %in% as.character(wl))

  # if that did not succeed, we bite the bullet and
  # try to extract wl from the colnames
  if (length(ind_col_spectra) == 0) {
    # more tricky - looking for X350, etc:
    require(plyr)
    require(stringr)
#     ind_col_spectra <- aaply(names(data), 1, .fun=function(x) any(str_detect(x, as.character(wl))), ...)
    nm <- .guessWl(names(data))
    ind_col_spectra <- which(nm %in% wl)
  }
  if (length(ind_col_spectra) == 0)
    stop('No columns found.')
#   which(ind_col_spectra)
  ind_col_spectra
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

    # value as to be a numeric vector
    if (is(value, 'numeric')) {
      # finding which cols contrain the spectra
      ind_nir <- .findSpectraCols(data=object, wl=value, .progress='text')
      nir <- object[, ind_nir]
      
      res <- Spectra(wl=as.numeric(value), nir=as.matrix(nir))
      
      # If there are some columns left, we use them to initiate a SpectraDataFrame object
      if (ncol(nir) < ncol(object)) {
	data <- object[, -ind_nir]
	# if there is only one col left,
	# we have to make sure to pass it as a data.frame
	if (is(data, 'numeric')) {
	  data <- data.frame(data)
	  names(data) <- names(object)[setdiff(1:ncol(object), ind_nir)]
	}
	data(res) <- data
      }
    }
    else
      stop('Bad initialisation, please provide wavelengths as a numeric vector.')
  res
  }
)

## setting the spectra of a Spectra* object
##
## - if applied to a data.frame --> we create a Spectra* object
## - if applied to a Spectra* --> we change its @nir slot
if (!isGeneric('spectra<-'))
  setGeneric('spectra<-', function(object, value) 
    standardGeneric('spectra<-'))

## for a data.frame
setReplaceMethod("spectra", "data.frame",
  function(object, value) {
  
    # if given a formula
    # eg spectra(df) <- ~ .  | (Spectra object)
    #    spectra(df) <- organic_carbon + ph ~ . | (SpectraDataFrame object)
    # if there's only an id : spectra(df) <- id ~ . ; id(df) <- id
    if (is(value, 'formula')) {
      mf <- model.frame(formula=value, data=object)

      # there are variable(s) on the left side of the formula
      vars.left <- all.vars(update(value, .~0))
      if (all(vars.left != '.')) {
	ind.vars <- which(colnames(mf) %in% vars.left)
	if (length(ind.vars) == 1) {
	  data <- data.frame(mf[, ind.vars])
	  names(data) <- vars.left
	}
	else
	  data <- mf[, ind.vars]
	mf <- mf[, -ind.vars]	
      }

      # trying to find the WL
      wl <- .guessWl(names(mf))
      res <- Spectra(wl=wl, nir=mf)

      cat("Wavelength range: ")
      cat(min(wl(res), na.rm=TRUE), " to ", max(wl(res), na.rm=TRUE)," ", get_units(res), "\n", sep="")
      cat("Spectral resolution: ", get_resolution(wl(res)) , " ",  get_units(res), "\n", sep="")

      # if some data is present
      if (vars.left != '.')
	res <- SpectraDataFrame(res, data=data)
    }
    
    # if given a numeric vector (interpreted as the index of the cols)
    # eg spectra(df) <- 11:2161
    else if (is(value, 'numeric')) {
    }
    # if given a character vector (interpreted as the names of the cols)
    # eg spectra(df) <- c('X450', 'X451', 'X452')
    else if (is(value, 'character')) {
    }
    else
      stop('wrong initialisation')
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
      # the id needs to be unique!
      if (length(all.vars(value)) == 1) {
	mf <- model.frame(formula=value, data=object)
	# assigning the id slot
	object@id <- as.character(mf[, 1])
	# removing the id col from the data slot
	object@data <- object@data[, -which(names(object@data) == names(mf))]
	# if nothing left in the data slot, back to a Spectra object!
	if (ncol(object@data) == 0)
	  object <- Spectra(wl=object@wl, nir=object@nir, id=object@id, units=object@units)
      }
      else
	stop('wrong id initialisation: id must be unique.')
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
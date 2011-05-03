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

## Parsing of the formula interface to spectra<-
##
## spectra(df) <- id ~ attr1 + attr2 ~ ...
## spectra(df) <- id ~ ... What if id col do not exist (?)
## spectra(df) <- ~ ...
##
## Inspired from Hadley Wickham's parse_formula
## https://github.com/hadley/reshape/blob/master/R/formula.r
##
## @param formula a formula fot the spectra()<- setter
## @param object a data.frame
## @value returns a list of column names for the id slot, 
## the data slot and the nir slot of teh Spectra* object
##
.parse_formula <- function(formula, object){
  require(stringr)

  formula <- str_c(deparse(formula, 500), collapse="")

  elements <- str_split(formula, fixed("~"))[[1]]
  length_elements <- aaply(elements, 1, str_length)
  elements <- elements[which(length_elements > 0)]

  formula <- lapply(str_split(elements, "[+*]"), str_trim)
  n_elements <- length(formula)
  all_vars <- unlist(formula)

  replace.remainder <- function(x) {
    if (any(x == "...")) 
      c(x[x != "..."], remainder) 
    else x
  }
  
  ## PLACEHOLDERS
  ##
  ## ... : all the columns that havent been used in the formula
  ## :: : sequence of integers, like 350::2500
  ## ::: : sequence of numbers, emulates seq(), like 350:::0.1:::2500
  # if used the "." placeholder
  if (any(all_vars == "...")) {
    remainder <- setdiff(names(object), c(all_vars, 'id')) # setting id as a reserved name for id columns
    formula <- lapply(formula, replace.remainder)
  } 
  
  res <- list(id=NULL, data=NULL, nir=NULL)
  if (n_elements == 1) { # case spectra(df) <- ~ .
    res[['nir']] <- formula[[1]]
    
  }
  else if (n_elements == 2) {# case spectra(df) <- id ~ .      
    res[['id']] <- formula[[1]]
    res[['nir']] <- formula[[2]]
  }
  else if (n_elements == 3) {# spectra(df) <- id ~ attr1 + attr2 ~ .
#     # if id is not present in the colnames
#     if ((formula[[1]] == 'id') & !(formula[[1]] %in% names(object)))
    res[['id']] <- formula[[1]]
    res[['data']] <- formula[[2]]
    res[['nir']] <- formula[[3]]
  }
  else
    stop('wrong formula.')
  res
}

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
    if (is(value, 'formula')) {
      # parsing the formula to retrieve the different slots (id, data, nir)
      ind.vars <- lapply(.parse_formula(value, object), function(x) which(names(object) %in% x))
      
      if (length(ind.vars$nir) == 0) 
	ind.vars$nir <- .findSpectraCols(object, .parse_formula(value, object)$nir)
      
      nir <- object[, ind.vars$nir]
      if (length(ind.vars$id) == 0)
	ids <- as.character(NA)
      else
	ids <- object[, ind.vars$id]
      
      wl <- .guessWl(names(nir))
      res <- Spectra(id=ids, wl=wl, nir=nir)
      
      cat("Wavelength range: ")
      cat(min(wl(res), na.rm=TRUE), " to ", max(wl(res), na.rm=TRUE)," ", get_units(res), "\n", sep="")
      cat("Spectral resolution: ", get_resolution(wl(res)) , " ",  get_units(res), "\n", sep="")
      
      if (length(ind.vars$data != 0)) 
	res <- SpectraDataFrame(res, data=object[, ind.vars$data])
    }
    
    # if given a numeric vector (interpreted as the index of the cols)
    # eg spectra(df) <- 11:2161
    else if (is(value, 'numeric')) {
      nir <- object[, value]
      wl <- .guessWl(names(nir))
      res <- Spectra(wl=wl, nir=nir)

      # if there's some cols left, we create a SpectraDataFrame
      if (length(value) < ncol(object)) {
	data <- object[, setdiff(1:ncol(object), value)]
	res <- SpectraDataFrame(res, data=data)
      }
    }

    # if given a character vector (interpreted as the names of the cols)
    # eg spectra(df) <- c('X450', 'X451', 'X452')
    else if (is(value, 'character')) {
      ind.nir <- which(names(object) %in% value)
      nir <- object[, ind.nir]
      
      wl <- .guessWl(names(nir))
      res <- Spectra(wl=wl, nir=nir)

      # if there's some cols left, we create a SpectraDataFrame
      if (length(value) < ncol(object)) {
	data <- object[, setdiff(1:ncol(object), ind.nir)]
	res <- SpectraDataFrame(res, data=data)
      }
    }

    else
      stop('Wrong Spectra initialisation.')

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
"SpectraDataFrame" <- function(..., wl=numeric(), nir=matrix(), id=as.character(NA), units="nm", data=data.frame()) {
#   # Initialisation from SpectradataFrame object(s)
#   dotargs <- list(...)
#   if (any(sapply(dotargs, class) == "SpectraDataFrame")) {
#     id.SDFs <- which(sapply(dotargs, class) == "SpectraDataFrame")
#     
#   }

  # Initialization from an existing Spectra object
  if (is(wl, 'Spectra')){
    wl <- wl@wl
    nir <- wl@nir
    id <- wl@id
    units <- wl@units
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
  }
  if (is(data, "numeric") | is(data, "integer"))
      data <- as.data.frame(data)
  rownames(data) <- id
  new("SpectraDataFrame", wl=wl, nir=nir, id=id, units=units, data=data)
}

## coercition methods

as.data.frame.SpectraDataFrame = function(x, ...)  {
  df <- as.data.frame(get_spectra(x))
  data <- get_data(x)
  df <- data.frame(data, df)
  names(df) <- c(names(data), get_wl(x))
  df
}

setAs("SpectraDataFrame", "data.frame", function(from)
	as.data.frame.SpectraDataFrame(from))

## Getting the data

if (!isGeneric("get_data"))
  setGeneric("get_data", function(object, ...)
    standardGeneric("get_data"))

setMethod("get_data", "SpectraDataFrame", 
  function(object)
    object@data
)

setMethod("$", "SpectraDataFrame",
  definition=function(x, name) x@data[[name]]
)

setReplaceMethod("$", "SpectraDataFrame",
  definition=function(x, name, value) {
    x@data[[name]] <- value
    x
  }
)

setMethod("[[", c("SpectraDataFrame", "ANY", "missing"), 
  function(x, i, j, ...) {
    if (!("data" %in% slotNames(x)))
      stop("no [[ method for object without attributes")
    x@data[[i]]
  }
)

setReplaceMethod("[[", c("SpectraDataFrame", "ANY", "missing", "ANY"), 
  function(x, i, j, value) {
    if (!("data" %in% slotNames(x)))
      stop("no [[ method for object without attributes")
    x@data[[i]] <- value
    x
  }
)

names.SpectraDataFrame <- function(x) names(x@data)

"names<-.SpectraDataFrame" <- function(x, value) { 
  names(x@data) <- value
  x 
}

## Subset SDF with a subset/select query

subset.SpectraDataFrame <- function(x, subset, select, drop = FALSE, ...) {
  # adapted from subset.data.frame
  df <- get_data(x)
  if (missing(subset)) 
        r <- TRUE
  else {
    e <- substitute(subset)
    r <- eval(e, df, parent.frame())
    if (!is.logical(r)) 
	stop("'subset' must evaluate to logical")
    r <- r & !is.na(r)
  }
  if (missing(select)) 
    vars <- TRUE
  else {
    nl <- as.list(seq_along(df))
    names(nl) <- names(df)
    vars <- eval(substitute(select), nl, parent.frame())
  }  
  df_sub <- df[r, vars, drop = drop]
  # remove unused factors
  df_sub <- droplevels(df_sub)
  id_selected <- which(rownames(df) %in% rownames(df_sub))
  x <- SpectraDataFrame(wl=get_wl(x), nir=get_spectra(x)[id_selected,], id=get_id(x)[id_selected], units=get_units(x), data=df_sub)
  x
}

## Split

setMethod("split", "SpectraDataFrame", split.data.frame)

## Separate calibration set vs validation set

if (!isGeneric("separate"))
  setGeneric("separate", function(obj, calibration, ...)
    standardGeneric("separate"))

separate.SpectraDataFrame <- function(obj, calibration){
  if (calibration < 1)
    calibration <- floor(calibration*length(obj))
  calib <- sample(x=seq_len(length(obj)), size=calibration, replace=FALSE)
  valid <- setdiff(seq_len(length(obj)), calib)
  list(calibration=obj[calib,], validation=obj[valid,])
}

setMethod("separate", "SpectraDataFrame", separate.SpectraDataFrame)

if (!isGeneric("unseparate"))
  setGeneric("unseparate", function(obj, calibration, ...)
    standardGeneric("unseparate"))

unseparate.SpectraDataFrame <- function(obj){
  # Warning: does not recover the order of the samples
  #
  add(obj$calibration, obj$validation)
}

setMethod("unseparate", "list", unseparate.SpectraDataFrame)

## Transform the SpectraDataFrame object

transform.SpectraDataFrame <- function (obj, condition, ...){
  require(reshape2)
  require(stringr)
  data <- get_data(obj)
  condition_call <- substitute(condition)
  # you want to affect the spectra
  if (str_detect(deparse(condition_call), "nir")) {
    nir <- melt(get_spectra(obj), value.name="nir", varnames=c('id','wl'))
    nir <- transform(nir, ...)
    nir <- matrix(nir$nir, nrow=length(obj), ncol=length(get_wl(obj)))
    rownames(nir) <- get_id(obj)
    colnames(nir) <- get_wl(obj)  
    res <- SpectraDataFrame(wl=get_wl(obj), nir=nir, id=get_id(obj), units=get_units(obj), data=data)
  }
  # you want to affect the data
  else {
    data <- transform(data, condition)
    res <- SpectraDataFrame(wl=get_wl(obj), nir=get_spectra(nir), id=get_id(obj), units=get_units(obj), data=data)
  }
  res
}
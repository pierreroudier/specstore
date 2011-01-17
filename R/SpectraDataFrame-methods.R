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
  new("SpectraDataFrame", wl=wl, nir=nir, id=id, units=units, data=data)
}

## Getting the data
setGeneric("get_data", function(object, ...)
  standardGeneric("get_data")
)

setMethod("get_data", "SpectraDataFrame", 
  function(object)
    object@data
)

names.SpectraDataFrame <- function(x) names(x@data)

"names<-.SpectraDataFrame" <- function(x, value) { 
  names(x@data) <- value
  x 
}

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

#' @include Class-SpectraDataFrame.R
roxygen()

#' Class for spectra collections that have
#' associated attributes and spatial point locations
#'
#' @slot wl object of class "\code{numeric}"; the wavelengths at which the spectra has been measured
#' @slot nir object of class "\code{matrix}"; the spectra, with as many columns as wavelengths, and as many rows as samples
#' @slot id object of class "\code{data.frame}" with one attribute; the identification strings for each sample in the collection
#' @slot units object of class "\code{character}"; units in which the wavelengths are expressed
#' @slot data object of class data.frame containing the attribute data (may or may not contain the coordinates in its columns)
#' @slot bbox object of class "matrix"; bounding box
#' @slot proj4string object of class "CRS"; projection string
#' @slot coords object of class "matrix"; the coordinates matrix (points are rows in the matrix)
#' @slot coords.nrs object of class logical; if TRUE, when the object was created the coordinates were retrieved from the data.frame, and hence stripped from it; after coercion to data.frame, e.g. by as.data.frame(x), coordinates will again be added (as ﬁrst few columns) to the data.frame
#' @seealso \code{\link{spectra}}, \code{\link{wl}}, \code{\link{coordinates}}, \code{\link{SpectraDataFrame-class}}
#' @author Pierre Roudier \email{pierre.roudier@@gmail.com}
#' @rdname SpatialSpectraDataFrame-class
#' @exportClass SpatialSpectraDataFrame
#' @import sp
setClass(
  Class='SpatialSpectraDataFrame',
  representation=representation(
    'SpatialPointsDataFrame',
    'SpectraDataFrame'
  ),
  prototype=prototype(
    bbox = matrix(NA), 
    proj4string = CRS(as.character(NA)),
    coords = matrix(NA), 
    coords.nrs = numeric(0),
    wl=numeric(),
    nir=matrix(),
    id=data.frame(NA),
    units=as.character(NA),
    data = data.frame()
  )
)
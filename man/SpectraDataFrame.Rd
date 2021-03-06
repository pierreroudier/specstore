\name{SpectraDataFrame}
\alias{SpectraDataFrame}

\title{Constructor for the SpectraDataFrame class.}

\usage{SpectraDataFrame(..., wl=numeric(), nir=matrix(), id=as.character(NA), units="nm", data = data.frame())}

\description{Constructor for the SpectraDataFrame class. Creates a SpectraDataFrame object, either from scratch, or from an existing Spectra object.}

\value{a new \code{"Spectra"} object}

\author{Pierre Roudier \url{pierre.roudier@gmail.com}}

\arguments{
  \item{...}{an object inheriting from \code{"Spectra"}}
  \item{wl}{a numeric vector giving the wavelengths at with the spectra have been measured}
  \item{nir}{a \code{"matrix"} or a \code{"data.frame"} object giving the spectra values for each sample}
  \item{id}{a vector giving the unique id of each sample in the collection}
  \item{units}{a character giving the unit in which the wavelengths values are expressed}
  \item{data}{object of class \code{"data.frame"} containing the attribute data}
}

\seealso{\code{\link{spectra}}, \code{\link{wl}}, \code{\link{Spectra-class}}}

\examples{
  # Creating a SpectraDataFrame object from scratch
  my.wl <- 350:2500
  my.id <- c("A", "B")
  my.nir <- matrix(runif(2*length(my.wl)), nrow=2)
  my.data <- data.frame(foo = runif(2), bar = LETTERS[1:2])
  my.sdf <- SpectraDataFrame(wl = my.wl, nir = my.nir, id = my.id, data = my.data)

  # Creating a SpectraDataFrame object from an existing Spectra object
  my.s <- Spectra(wl = my.wl, nir = my.nir, id = my.id)
  my.sdf <- SpectraDataFrame(my.s, data = my.data)
}
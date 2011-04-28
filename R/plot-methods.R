## Spectra

plot.Spectra <- function(s, gg=FALSE, ...){
  s.melt <- melt_spectra(s)
  if (gg) {
    require(ggplot2)
    p <- ggplot(s.melt) + geom_line(aes(x=wl, y=nir, group=id)) # + ylim(c(0,1))
  }
  else {
    require(lattice)
    p <- xyplot(nir ~ wl, groups=id, data=s.melt, type='l', col.line='black', ...)
  }
  p
}

## SpectraDF
plot.SpectraDataFrame <- plot.Spectra
##
## Since the varying spectral regions appear somewhat parabolic, it seems that a
## pair of partial parabolas inserted into what otherwise is an identity matrix can be
## used to correct the temperature-sensitive channels. These parabolas may be ex-
## pressed in the form:
## 
## (x-h)^2 = 4p(y-k)
## 
## where the vertex is at (h, k); focus is at (h, k+p). In our case, p is the distance
## from the vertex to the focus point and h will always be one, so that the parabolas
## merge with the identity matrix (I) at the vertices. The focus will be the dynamic
## feature of the parabola, determined directly from the difference between the y-
## values on either side of the spectral splice of interest. In other words, as a good
## approximation to correcting the far red endpoint of the VNIR region, we will
## build a transformation parabola that will set the value of that endpoint equal to
## the value of the adjacent SWIR1 endpoint; which will determine how “open” the
## parabola will be, given the same vertex for any correction matrix. A similar
## situation is true of the SWIR2 correction.
## 
## After experimenting with several different vertex choices on several different
## Full Range instruments, we have found the best general choices for all units were
## at about the points (700,1) and (1975,1). You may wish to use these as your in-
## strument’s “defaults”, until a full characterization can be performed. When char-
## acterizing your unit’s vertices, be sure to view a stable source, and collect a new
## dark current before each spectrum, so that dark current drift will not be a factor.
## We used 725 and 1950 as vertices for instrument 632 (Figure 3), with excellent
## results – a maximum deviation of less than 1% from unity for both trouble re-
## gions in the spectrum. The actual ideal vertices for this instrument are at 675 and
## 1980, so this plot introduces some additional unnecessary error – a worst-case
## scenario, let’s say. The full equation reads:
## 
## IF 724 < x < 1021,
##   y = (x-725)2(y1021-y1020)/y1020(1020-725)2 + 1,
## IF 1800 < x < 1951,
##   y = (x-1950)2(y1800-y1801)/y1801(1800-1950)2 + 1,
## ELSE y = 1
##

# spl1 <- which((wl(obj) > 724) & (wl(obj) < 1021))
# spl2 <- which((wl(obj) > 1800) & (wl(obj) < 1951))
# others <- setdiff(wl(obj), c(spl1, spl2))
# 
# y <- vector(mode='numeric', length=length(obj)) + 1
# 
# y1020 <- spectra(obj)[, wl(obj) == 1020]
# y1021 <- spectra(obj)[, wl(obj) == 1021]
# y1800 <- spectra(obj)[, wl(obj) == 1800]
# y1801 <- spectra(obj)[, wl(obj) == 1801]
# 
# y[spl1] <- ((spectra(obj)[, spl1] - 725)^2*(y1021 - y1020))/(y1020*(1020 - 725)^2) + 1
# y[spl2] <- ((spectra(obj)[, spl2] - 1950)^2*(y1800 - y1801))/(y1801*(1800 - 1950)^2) + 1

splice <- function(obj){
  nir <- spectra(obj)
  
  .splice <- function(x, wl) {
    spl1 <- which((wl > 724) & (wl < 1021))
    spl2 <- which((wl > 1800) & (wl < 1951))
    others <- setdiff(wl, c(spl1, spl2))

    y <- vector(mode='numeric', length=length(x)) + 1
    
    y1020 <- x[wl == 1020]
    y1021 <- x[wl == 1021]
    y1800 <- x[wl == 1800]
    y1801 <- x[wl == 1801]

    y[spl1] <- ((x[spl1] - 725)^2*(y1021 - y1020))/(y1020*(1020 - 725)^2) + 1
    y[spl2] <- ((x[spl2] - 1950)^2*(y1800 - y1801))/(y1801*(1800 - 1950)^2) + 1
    y
  }

  y <- aaply(nir, 1, .fun=.splice, wl(obj))
  y
}

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
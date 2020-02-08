#' The difference between two maps , i.e. the average distance between the closest pairs of neurons .
#'
#' @param map1 TODO
#' @param map2 TODO
#' @return TODO:
#' @export
get.map.diff <-
function(map1, map2) {
  # Makesurethemapshavethesamedimensions
  if (map1$xdim != map2$xdim || map1$ydim != map2$ydim) {
    stop("mapshavedifferentdimesions")
  }
  # Gettheneuronsfromthemaps
  neurons.1.df <- data.frame(map1$neurons)
  neurons.2.df <- data.frame(map2$neurons)
  # Makesuretheneuronshavethesamenumberofdims
  if (dim(neurons.1.df)[2] != dim(neurons.2.df)[2]) {
    stop("mapneuronshavedifferentdimesions")
  }
  # Getthenumberofneurons(forextractingdist)
  n <- dim(neurons.1.df)[1]
  # Mergethedata
  colnames(neurons.1.df) <- colnames(neurons.2.df)
  all <- rbind(neurons.1.df, neurons.2.df)
  # Calculatethedistances
  d <- as.matrix(dist(all))
  # Pulloutthedistancesbetweenneuronsintwomaps
  dist.cross <- d[1:n, (n + 1):(n + n)]
  # Usequantizationerrortofindthedistance
  # betweenthenodesinthetwomaps
  m.diff <- get.quant.err(dist.cross)$val
  # Returnlist
  list(val = m.diff)
}

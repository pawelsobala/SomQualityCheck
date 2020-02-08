#' The ratio of neurons that are best matching units for a data point
#'
#' @param dist.cross the distance matrix between the data points and the map neurons
#' @return List containing a value and a vector:
#' ratio - the ratio of neurons that are a BMU for a
#' data point .
#' neurons - number of data points mapped to each neuron
#' @export
get.bmu.ratio <-
function(dist.cross) {
  # Initialize
  n <- dim(dist.cross)[1]
  m <- dim(dist.cross)[2]
  neurons <- rep(0, m)
  # Checkeachdatapoint
  for (i in 1:n) {
    between <- dist.cross[i, ]
    bmu.idx <- which.min(between)
    neurons[bmu.idx] <- neurons[bmu.idx] + 1
  } # endfor
  # Calculatetheerror
  bmus <- 0
  for (i in 1:m) {
    if (neurons[i] > 0) {
      bmus <- bmus + 1
    }
  }
  ratio <- bmus / m
  # Returnlist
  list(ratio = ratio, neurons = neurons)
}

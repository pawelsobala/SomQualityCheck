#' The distances between the data, neurons, and across both .
#'
#' @param map map; object returned by map.build
#' @return List containing distances between 3 matrices:
#' dist.data - distance between all data points
#' dist.neurons - distance between all map neurons
#' dist.cross - distance between the data points and the map neurons
#' dist.proj - distance between the projected data points 
#'
#' @export
get.distances <-
function(map) {
  if (class(map) != "map") {
    stop("get.distances:notamapobject")
  }
  # Getthedataset
  data.df <- data.frame(map$data)
  # Gettheneurons
  neurons.df <- data.frame(map$neurons)
  # Mergethedata
  colnames(neurons.df) <- colnames(data.df)
  all <- rbind(data.df, neurons.df)
  # Calculatethedistances
  d <- as.matrix(dist(all))
  # Pulloutthedistancesbetweendataandneurons
  n <- dim(data.df)[1]
  m <- dim(neurons.df)[1]
  dist.data <- d[1:n, 1:n]
  dist.neurons <- d[(n + 1):(n + m), (n + 1):(n + m)]
  dist.cross <- d[1:n, (n + 1):(n + m)]
  # Gettheprojectedpointsanddistances
  projection <- map$neurons[map$visual, ]
  dist.proj <- as.matrix(dist(projection))
  # Returndistances
  list(
    dist.data = dist.data,
    dist.neurons = dist.neurons,
    dist.cross = dist.cross,
    dist.proj = dist.proj
  )
}

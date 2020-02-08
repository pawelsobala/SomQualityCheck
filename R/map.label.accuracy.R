#' The labeling of the neurons and other measures
#' 
#' @param map map;an object returned by map. build
#' @return list containing :
#' neurons - majority label mapped to each neuron
#' acc - labeling accuracy of the map
#' bmu - ratio of BMUs
#' fit - combined accuracy and ratio value
#' @export

map.label.accuracy <-
function(map) {
  # gettheneuronlabels
  neuron.l <- rep(NA, dim(map$neurons)[1])
  proj <- map$visual
  labels <- as.vector(as.data.frame(map$labels)[, 1])
  # assignmajoritylabelstoneurons
  for (i in 1:length(neuron.l)) {
    if (length(which(proj == i)) > 0) {
      labels.n <- labels[which(proj == i)]
      neuron.l[i] <- names(which.max(table(labels.n)))
    } # endif
  } # endforlength(unique(proj))/length(neuron.l)
  # checkhowmanyofthelabelsoftheneuronsmatch
  # theinputandhowmanyneuronsaremappedto
  acc <- (length(which(neuron.l[proj] == labels))
  / length(proj))
  bmu <- length(unique(proj)) / length(neuron.l)
  list(
    neurons = neuron.l, acc = acc, bmu = bmu,
    fit = (acc * bmu)
  )
}

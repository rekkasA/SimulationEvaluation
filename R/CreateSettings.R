#' @export

createPredictionSettings <- function(
  args,
  fun
) {
  analysis <- list()
  for (name in names(formals(createPredictionSettings))) {
    analysis[[name]] <- get(name)
  }
  class(analysis) <- "args"

  return(analysis)
}



#'@export

createSmoothSettings <- function(
  type,
  label,
  settings
) {
  analysis <- list()
  for (name in names(formals(createSmoothSettings))) {
    analysis[[name]] <- get(name)
  }
  class(analysis) <- "args"

  return(analysis)
}

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



#' @export
createAnalysisSettings <- function(
  threads        = 1,
  replications   = 100,
  validationSize = 1e6,
  seeds          = sample(1e5, replications),
  label          = "analysis",
  description    = "description"
) {
  analysis <- list()
  for (name in names(formals(createAnalysisSettings))) {
    analysis[[name]] <- get(name)
  }
  class(analysis) <- "args"

  return(analysis)
}

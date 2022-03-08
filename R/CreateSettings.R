#' Create the prediction settings of the simulation
#'
#' @description
#' Create the settings for the development of the internal prediction model.
#'
#' @param fun                The name of the function to be called for developing the
#'                           Prediction model.
#' @param args               A list containing the arguments to be passed on to the
#'                           Function.
#' @param .before            A function of the data for runnning preprocessing
#'                           Operations.
#' @param .beforeSettings    A list of settings for `.before` function
#' @param .after             A function of the data for runnning post-processing
#'                           operations.
#' @param .beforeSettings    A list of settings for `.after` function.
#'
#' @export

createPredictionSettings <- function(
  args,
  fun,
  .before = NULL,
  .beforeSettings = NULL,
  .after = NULL,
  .afterSettings = NULL
) {
  analysis <- list()
  for (name in names(formals(createPredictionSettings))) {
    analysis[[name]] <- get(name)
  }
  class(analysis) <- "args"

  return(analysis)
}



#' Create the smoothing settings
#'
#' @description
#' Creates the settings for performing a smooth estimation of benefit
#'
#' @param type        The type of smoothing. Can be one of "loess", "rcs" or
#'                    "locfit", "stratified", "modelBased" or "adaptive".
#' @param label       The label of the smoothing approach
#' @param settings    Depending on the type of smoothing can be generated from
#'                    [createLoessSettings()], [createRcsSettings()] or
#'                    [createLocfitSettings()]
#'
#'@export

createSmoothSettings <- function(
  type,
  label,
  settings
) {
  validTypes <- c(
    "loess",
    "rcs",
    "locfit",
    "stratified",
    "modelBased",
    "adaptive"
  )
  if (!type %in% validTypes) stop("Not a valid type!")

  analysis <- list()
  for (name in names(formals(createSmoothSettings))) {
    analysis[[name]] <- get(name)
  }
  class(analysis) <- "args"

  return(analysis)
}




#' Create the analysis settings
#'
#' @description
#' Creates the more general settings for running the simulation.
#'
#' @param analysisId         The id of the analysis.
#' @param threads            The number of parallel threads to be considered
#'                           for running the simulations
#' @param replications       The number of replications for runnning the
#'                           simulation
#' @param validationSize     The size of the "true" population size. Used to
#'                           approximate the "true" performance of the method
#'                           under study
#' @param seed               The seed used to generate the `validationDataset`
#'                           and the simulated datasets
#' @param description        The description of the simulation
#' @param saveDirectory      The directory where the results will be stored
#'
#' @export
createAnalysisSettings <- function(
  analysisId     = "analysis",
  threads        = 1,
  replications   = 100,
  validationSize = 1e6,
  seed           = 1,
  description    = "description",
  saveDirectory  = getwd()
) {
  analysis <- list()
  for (name in names(formals(createAnalysisSettings))) {
    analysis[[name]] <- get(name)
  }
  class(analysis) <- "args"

  return(analysis)
}

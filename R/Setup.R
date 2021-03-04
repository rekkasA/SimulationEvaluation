#' Evaluate target settings
#'
#' @description
#'   Evaluates the target settings for the simulation. Currently, it can be used
#'   to derive the C-statistic of the prediction model and the event rates per
#'   treatment arm
#'
#' @param simulationSettings      A list containing the `databaseSettings`,
#'                                `propensitySettings`, `baselineRiskSettings`
#'                                and the `treatmentEffectSettings`
#' @param analysisSettings        ...
#' @param predictionSettings      ...
#'
#' @export

evaluateTargets <- function(
  simulationSettings,
  analysisSettings,
  predictionSettings
) {

  validationDatabaseSettings <- simulationSettings$databaseSettings
  validationDatabaseSettings$numberOfObservations <- analysisSettings$validationSize
  validationDataset <- SimulateHte::runDataGeneration(
    databaseSettings        = validationDatabaseSettings,
    propensitySettings      = simulationSettings$propensitySettings,
    baselineRiskSettings    = simulationSettings$baselineRiskSettings,
    treatmentEffectSettings = simulationSettings$treatmentEffectSettings
  )

  simulatedDataset <- SimulateHte::runDataGeneration(
    databaseSettings        = simulationSettings$databaseSettings,
    propensitySettings      = simulationSettings$propensitySettings,
    baselineRiskSettings    = simulationSettings$baselineRiskSettings,
    treatmentEffectSettings = simulationSettings$treatmentEffectSettings
  )

  predictionSettings$args$data <- simulatedDataset
  predictionModel <- do.call(
    eval(parse(text = predictionSettings$fun)),
    predictionSettings$args
  )
  validationDataset <- validationDataset %>%
    dplyr::mutate(
      riskLinearPredictor = predict(
        predictionModel,
        newdata = validationDataset %>%
          dplyr::mutate(treatment = 0)
      )
    )

  rate <- validationDataset %>%
    group_by(treatment) %>%
    summarise(rate = mean(outcome))

  list(
    auc = AUC(validationDataset$riskLinearPredictor, validationDataset$outcome)$AUC,
    rate = rate
  )

}

#' Calculate AUC
#'
#' @author
#'   David van Klaveren
#' @description
#'   Calculates AUC for binary outcomes
#'
#' @param xb.hat     A vector with the predictions
#' @param y          A vector with the observed outcomes

AUC <- function(xb.hat,y){
  max.y <- max(y)
  n <- as.numeric(length(xb.hat))
  n1 <- as.numeric(sum(y == max.y))
  mean.rank <- mean(rank(xb.hat)[y == max.y])
  AUC <- (mean.rank - (n1 + 1) / 2) / (n - n1)
  comparable <- as.numeric(n1 * (n - n1) * 2)
  concordant <- AUC * n1
  return(
    list(
      AUC        = AUC,
      comparable = comparable,
      concordant = concordant
    )
  )
}

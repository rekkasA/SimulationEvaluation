#' @export

runSimulation <- function(
  simulationSettings,
  predictionSettings,
  smoothSettings,
  runSettings
) {

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
  simulatedDataset <- simulatedDataset %>%
    dplyr::mutate(
      riskLinearPredictor = predict(
        predictionModel,
        newdata = simulatedDataset %>%
          dplyr::mutate(treatment = 0)
      )
    ) %>%
    dplyr::select(-propensityLinearPredictor)

  simulatedDataset0 <- simulatedDataset %>%
    dplyr::filter(treatment == 0)
  simulatedDataset1 <- simulatedDataset %>%
    dplyr::filter(treatment == 1)

  smoothLabels <- unlist(
    rlist::list.map(                     # extract the second element of a
      smoothSettings,                    # list of lists (here the label)
      .[2]
    )
  )
  names(smoothLabels) <- NULL

  pehe <- list()
  for (i in seq_along(smoothLabels)) {
    smoothSettingsTmp <- smoothSettings[[i]]
    if (smoothSettingsTmp$type == "loess") {
      f <- "fitLoessHte"
    } else if (smoothSettingsTmp$type == "rcs") {
      f <- "fitRcsHte"
    } else {
      f <- "fitLocfitHte"
    }
    args <- list(
      data = simulatedDataset0,
      settings = smoothSettingsTmp$settings
    )
    s0 <- do.call(
      eval(parse(text = f)),
      args = args
    )
    args <- list(
      data = simulatedDataset1,
      settings = smoothSettingsTmp$settings
    )
    s1 <- do.call(
      eval(parse(text = f)),
      args = args
    )

    pehe[[i]] <- SimulateHte::calculatePEHE(
      data = simulatedDataset,
      predictedBenefit = SmoothHte::predictBenefit(
        p = plogis(simulatedDataset$riskLinearPredictor),
        smoothControl = s0,
        smoothTreatment = s1
      )
    )
  }
  names(pehe) <- smoothLabels

  return(pehe)

}

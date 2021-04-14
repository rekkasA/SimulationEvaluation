#' @export
fitAdaptiveApproach <- function(
  trainingDataset,
  validationDataset,
  nonLinearSettings,
  linearSettings,
  constantSettings
) {

  nonLinearFit <- rms::lrm(
    outcome ~ treatment + rms::rcs(riskLinearPredictor, 3) + treatment * rms::rcs(riskLinearPredictor, 3),
    data = trainingDataset
  )
  linearFit <- rms::lrm(
    outcome ~ treatment + riskLinearPredictor + treatment * riskLinearPredictor,
    data = trainingDataset
  )
  constantFit <- rms::lrm(
    outcome ~ treatment + riskLinearPredictor,
    data = trainingDataset
  )
  testResult      <- rms::lrtest(nonLinearFit, linearFit)
  pValueNonLinear <- testResult$stats[["P"]]
  testResult      <- rms::lrtest(linearFit, constantFit)
  pValueConstant  <- testResult$stats[["P"]]

  adaptiveMethod <- case_when(
    pValueNonLinear < .05                          ~ "nonLinear",
    pValueNonLinear >= .05 & pValueConstant < .05  ~ "linear",
    pValueNonLinear >= .05 & pValueConstant >= .05 ~ "constant"
  )

  if (adaptiveMethod == "nonLinear") {
    dataUntreated <- trainingDataset %>%
      dplyr::filter(treatment == 0)
    dataTreated <- trainingDataset %>%
      dplyr::filter(treatment == 1)

    smoothControl <- SmoothHte::fitRcsHte(
      data     = dataUntreated,
      settings = nonLinearSettings
    )
    smoothTreatment <- SmoothHte::fitRcsHte(
      data     = dataTreated,
      settings = nonLinearSettings
    )

    predictedBenefit <- SmoothHte::predictSmoothBenefit(
      p               = plogis(validationDataset$riskLinearPredictor),
      smoothControl   = smoothControl,
      smoothTreatment = smoothTreatment
    )
  } else if (adaptiveMethod == "linear") {
    modelBasedHte <- SmoothHte::fitModelBasedHte(
      data     = trainingDataset,
      settings = linearSettings
    )

    predictedBenefit <- SmoothHte::predictBenefitModelBasedHte(
      p             = plogis(validationDataset$riskLinearPredictor),
      modelBasedFit = modelBasedHte
    )
  } else if (adaptiveMethod == "constant") {
    modelBasedHte <- SmoothHte::fitModelBasedHte(
      data     = trainingDataset,
      settings = constantSettings
    )

    predictedBenefit <- SmoothHte::predictBenefitModelBasedHte(
      p             = plogis(validationDataset$riskLinearPredictor),
      modelBasedFit = modelBasedHte
    )
  }

  return(predictedBenefit)
}

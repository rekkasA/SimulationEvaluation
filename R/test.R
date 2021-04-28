#' @export
fitAdaptiveApproach <- function(
  trainingDataset,
  validationDataset,
  criterion = "aic"
) {

  untreatedData     <- trainingDataset %>% dplyr::filter(treatment == 0)
  treatedData       <- trainingDataset %>% dplyr::filter(treatment == 1)

  modelsUntreated   <- createFittedModels(untreatedData)
  modelUntreated    <- selectModel(modelsUntreated, criterion)

  modelsTreated     <- createFittedModels(treatedData, treatment = 1)
  modelTreated      <- selectModel(modelsTreated, criterion)

  evaluationData <- createEvaluationData(
    data           = validationDataset,
    modelUntreated = modelUntreated,
    modelTreated   = modelTreated
  )

  return(evaluationData)
}


createFittedModels <- function(data, treatment = 0) {

  if (treatment == 0) {
    constant <- glm(
      outcome ~ offset(riskLinearPredictor) - 1,
      data = data,
      family = "binomial"
    )
  } else {
    constant <- glm(
      outcome ~ offset(riskLinearPredictor),
      data = data,
      family = "binomial"
    )
  }

  linear <- glm(
    outcome ~ riskLinearPredictor,
    data = data,
    family = "binomial"
  )

  rcs3 <- glm(
    outcome ~ rms::rcs(riskLinearPredictor, 3),
    data = data,
    family = "binomial"
  )


  rcs4 <- glm(
    outcome ~ rms::rcs(riskLinearPredictor, 4),
    data = data,
    family = "binomial"
  )


  rcs5 <- glm(
    outcome ~ rms::rcs(riskLinearPredictor, 5),
    data = data,
    family = "binomial"
  )

  return(
    list(
      constant = constant,
      linear   = linear,
      rcs3     = rcs3,
      rcs4     = rcs4,
      rcs5     = rcs5
    )
  )
}

selectModel <- function(
  models,
  criterion = "aic"
) {
  crit <- Inf
  for (i in seq_along(models)) {
    tmpCrit <- broom::glance(models[[i]])[[toupper(criterion)]]
    if (tmpCrit < crit) {
      crit <- tmpCrit
      model <- i
    }

  }
  return(models[[model]])
}

createEvaluationData <- function(
  data,
  modelUntreated,
  modelTreated
) {
  data %>%
    dplyr::mutate(
      untreatedLp = predict(
        modelUntreated,
        data
      ),
      treatedLp = predict(
        modelTreated,
        data
      ),
      predictedBenefit = plogis(untreatedLp) - plogis(treatedLp)
    ) %>%
    dplyr::select(-c("untreatedLp", "treatedLp")) %>%
    return()
}


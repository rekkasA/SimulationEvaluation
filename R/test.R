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


#' Calculate concordance
#' @description
#' Calculates the concordance based on the Cai's paper (ref)
#'
#' @param data        The evaluation data, containing columns
#'                    'predictedBenefit', 'treatment' and 'outcome'
#' @param limits      Limit the focused area
#' @param quantiles   The number of quantiles to be considered
#'
#' @export
concordanceCai <- function(
  data,
  limits    = c(0, .9),
  quantiles = 100
) {

  x <- seq(
    from       = limits[1],
    to         = limits[2],
    length.out = quantiles
  )
  quants <- quantile(
    data$predictedBenefit,
    x
  )

  observed <- rep(NA, length(quants))
  for (i in seq_along(quants)) {

    dataSubset <- data %>%
      dplyr::filter(predictedBenefit >= quants[i])

    observed[i] <- dataSubset %>%
      dplyr::group_by(treatment) %>%
      summarise(m = mean(outcome)) %>%
      pull(m) %>%
      diff()
  }

  observed    <- -observed*(1 - x)
  tmpFun      <- approxfun(x, observed)
  concordance <- tryCatch(
    {
      integrate(tmpFun, 0, .9)
    },
    error = function(e) {
      e$message
    }
  )

  return(
    list(
      concordance = concordance,
      data        = dplyr::tibble(x, observed),
      curve       = tmpFun
    )
  )

}

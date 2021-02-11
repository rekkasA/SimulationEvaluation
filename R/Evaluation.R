#' @importFrom data.table data.table
#' @export
calculateCalibrationForBenefit <- function(
  data,
  strata = 4
) {

  trueBenefit <- data$trueBenefit
  predictedBenefit <- data$predictedBenefit
  data <- data.table::data.table(
    trueBenefit      = trueBenefit,
    predictedBenefit = predictedBenefit
  )
  data <- data[
    ,
    quantile := cut(
      x              = predictedBenefit,
      breaks         = quantile(
        predictedBenefit,
        probs = seq(0, 1, by = 1/strata),
        na.rm = TRUE
      ),
      right          = FALSE,
      labels         = 1:strata,
      include.lowest = T
    )
  ][
    ,
    .(
      meanTrueBenefit      = mean(trueBenefit),
      meanPredictedBenefit = mean(predictedBenefit)
    ),
    by = quantile
  ][
    ,
    difference := meanPredictedBenefit - meanTrueBenefit
  ][
    order(quantile)
  ]

  res <- mean(
    data[quantile %in% c(1, strata)]$difference
  )

  return(res)
}




#' @export
calculateDiscriminationForBenefit <- function(
  data,
  strata = 4
){

  data <- data.table::data.table(
    outcome          = data$outcome,
    predictedBenefit = data$predictedBenefit,
    treatment        = data$treatment
  )
  data <- data[
    ,
    quantile := cut(
      x              = predictedBenefit,
      breaks         = quantile(predictedBenefit, probs = seq(0, 1, by = 1/strata)),
      right          = FALSE,
      labels         = 1:strata,
      include.lowest = TRUE
    )
  ][
    ,
    .(
      meanObservedBenefit  = mean(outcome),
      meanPredictedBenefit = mean(predictedBenefit)
    ),
    by = .(quantile, treatment)
  ][
    order(quantile)
  ]

  subData <- data %>%
    dplyr::filter(quantile == 1)
  lowest <- subData[subData$treatment == 0, ]$meanObservedBenefit - subData[subData$treatment == 1, ]$meanObservedBenefit
  subData <- data %>%
    dplyr::filter(quantile == strata)
  highest <- subData[subData$treatment == 0, ]$meanObservedBenefit - subData[subData$treatment == 1, ]$meanObservedBenefit

  res <- mean(c(highest, lowest))
  return(res)

}

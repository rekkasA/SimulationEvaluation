# calculateCalibrationForBenefit <- function(
#   data,
#   strata = 4,
#   type
# ) {
#
#   data <- data.table::data.table(data)
#   if (type == "stratified") {
#     observedBenefitData <- data[
#       ,
#       .(
#         meanObservedOutcome = mean(outcome)
#       ),
#       by = .(
#         predictedBenefit,
#         treatment
#       )
#     ][
#       order(predictedBenefit, treatment)
#     ]
#
#     observedBenefit <- observedBenefitData[seq(1, 2 * strata, 2), ]$meanObservedOutcome -
#       observedBenefitData[seq(2, 2 * strata, 2), ]$meanObservedOutcome
#     predictedBenefit <- observedBenefitData[seq(1, 2 * strata, 2), ]$predictedBenefit
#
#     res <- predictedBenefit - observedBenefit
#     res <- mean(res[c(1, 4)])
#
#   } else {
#     data <- data.table::data.table(data)
#     predictedBenefitData <- data[
#       ,
#       quantile := cut(
#         x              = predictedBenefit,
#         breaks         = quantile(
#           predictedBenefit,
#           probs = seq(0, 1, by = 1/strata),
#           na.rm = TRUE
#         ),
#         right          = FALSE,
#         labels         = 1:strata,
#         include.lowest = T
#       )
#     ][
#       ,
#       .(
#         meanPredictedBenefit = mean(predictedBenefit)
#       ),
#       by = .(
#         quantile
#       )
#     ][
#       order(quantile)
#     ]
#
#     observedBenefitData <- data[
#       ,
#       quantile := cut(
#         x              = predictedBenefit,
#         breaks         = quantile(
#           predictedBenefit,
#           probs = seq(0, 1, by = 1/strata),
#           na.rm = TRUE
#         ),
#         right          = FALSE,
#         labels         = 1:strata,
#         include.lowest = T
#       )
#     ][
#       ,
#       .(
#         meanObservedOutcome = mean(outcome)
#       ),
#       by = .(
#         quantile,
#         treatment
#       )
#     ][
#       order(quantile, treatment)
#     ]
#
#     lowestObserved <- observedBenefitData[quantile == 1 & treatment == 0]$meanObservedOutcome -
#       observedBenefitData[quantile == 1 & treatment == 1]$meanObservedOutcome
#     highestObserved <- observedBenefitData[quantile == strata & treatment == 0]$meanObservedOutcome -
#       observedBenefitData[quantile == strata & treatment == 1]$meanObservedOutcome
#
#     lowest <- predictedBenefitData[predictedBenefitData$quantile == 1, ]$meanPredictedBenefit - lowestObserved
#     highest <- predictedBenefitData[predictedBenefitData$quantile == strata, ]$meanPredictedBenefit - highestObserved
#     res <- mean(c(highest, lowest))
#   }
#
#   return(res)
# }




# calculateDiscriminationForBenefit <- function(
#   data,
#   strata = 4,
#   type
# ) {
#
#   data <- data.table::data.table(
#     outcome          = data$outcome,
#     predictedBenefit = data$predictedBenefit,
#     treatment        = data$treatment
#   )
#
#   if (type == "stratified") {
#     data <- data[
#       ,
#       quantile := data.table::frank(predictedBenefit, ties.method = "dense")
#     ]
#   } else {
#     data <- data[
#       ,
#       quantile := cut(
#         x              = predictedBenefit,
#         breaks         = quantile(predictedBenefit, probs = seq(0, 1, by = 1/strata)),
#         right          = FALSE,
#         labels         = 1:strata,
#         include.lowest = TRUE
#       )
#     ]
#   }
#
#   data <- data[
#     ,
#     .(
#       meanObservedBenefit  = mean(outcome),
#       meanPredictedBenefit = mean(predictedBenefit)
#     ),
#     by = .(quantile, treatment)
#   ][
#     order(quantile, treatment)
#   ]
#
#   subData <- data %>%
#     dplyr::filter(quantile == 1)
#   lowest <- subData[treatment == 0, ]$meanObservedBenefit - subData[treatment == 1, ]$meanObservedBenefit
#   subData <- data %>%
#     dplyr::filter(quantile == strata)
#   highest <- subData[treatment == 0, ]$meanObservedBenefit - subData[treatment == 1, ]$meanObservedBenefit
#
#   res <- highest - lowest
#   return(res)
#
# }



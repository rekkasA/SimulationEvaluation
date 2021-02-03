# library(SimulateHte)
# library(dplyr)
# library(SmoothHte)
# library(rms)
#
# databaseSettings <- createDatabaseSettings(
#   numberOfObservations = 1e4,
#   numberOfCovariates = 4,
#   covariateDistributionSettings = list(
#     createNormalDistributionSettings(),
#     createNormalDistributionSettings(),
#     createNormalDistributionSettings(),
#     createNormalDistributionSettings()
#   )
# )
# baselineRiskSettings <- createBaselineRiskSettings(
#   type = "binary",
#   modelSettings = createModelSettings(
#     constant = -1.5,
#     modelMatrix = diag(4),
#     transformationSettings = list(
#       identity,
#       identity,
#       identity,
#       identity
#     ),
#     coefficients = rep(.8, 4)
#   )
# )
# propensitySettings <- createPropensitySettings(
#   type = "binary",
#   modelSettings = createModelSettings(
#     constant = 0,
#     modelMatrix = diag(0)
#   )
# )
# treatmentEffectSettings <- createTreatmentEffectSettings(
#   type = "lp",
#   modelSettings = createModelSettings(
#     constant = -0.5108256
#   )
# )
# simulationSettings <- list(
#   databaseSettings = databaseSettings,
#   propensitySettings = propensitySettings,
#   baselineRiskSettings = baselineRiskSettings,
#   treatmentEffectSettings = treatmentEffectSettings
# )
#
#
# predictionSettings <- createPredictionSettings(
#   args = list(
#     formula = "outcome ~ x1 + x2 + x3 + x4 + treatment",
#     family = "binomial"
#   ),
#   fun = "glm"
# )
#
#
# smoothSettings <- list(
#   loess = createSmoothSettings(
#     type = "loess",
#     settings = SmoothHte::createLoessSettings(),
#     label = "loess"
#   ),
#   rcs = createSmoothSettings(
#     type = "rcs",
#     settings = SmoothHte::createRcsSettings(),
#     label = "rcs"
#   ),
#   locfit = createSmoothSettings(
#     type = "locfit",
#     settings = SmoothHte::createLocfitSettings(),
#     label = "locfit"
#   )
# )
#
#
# runSimulation(
#   simulationSettings,
#   predictionSettings,
#   smoothSettings, NULL
# )
#
# pehe$loess[[i]] <- calculatePEHE(
#   data = dd,
#   predictedBenefit = predictBenefit(
#     p = plogis(dd$riskLinearPredictor),
#     smoothControl = s0,
#     smoothTreatment = s1
#   )
# )
# pehe$rcs[[i]] <- calculatePEHE(
#   data = dd,
#   predictedBenefit = predictBenefit(
#     p = plogis(dd$riskLinearPredictor),
#     smoothControl = r0,
#     smoothTreatment = r1
#   )
# )
# pehe$locfit[[i]] <- calculatePEHE(
#   data = dd,
#   predictedBenefit = predictBenefit(
#     p = plogis(dd$riskLinearPredictor),
#     smoothControl = l0,
#     smoothTreatment = l1
#   )
# )
#

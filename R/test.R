# library(SimulationEvaluationHte)
# library(SmoothHte)
# library(SimulateHte)
#
# databaseSettings <- createDatabaseSettings(
#   numberOfObservations = 5e3,
#   numberOfCovariates = 4,
#   covariateDistributionSettings = list(
#     createNormalDistributionSettings(),
#     createNormalDistributionSettings(),
#     createNormalDistributionSettings(),
#     createNormalDistributionSettings()
#   )
# )
#
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
#
# propensitySettings <- createPropensitySettings(
#   type = "binary",
#   modelSettings = createModelSettings(
#     constant = 0,
#     modelMatrix = diag(0)
#   )
# )
#
# treatmentEffectSettings <- createTreatmentEffectSettings(
#   type = "lp",
#   modelSettings = createModelSettings(
#     constant = -0.5108256
#   )
# )
#
# simulationSettings <- list(
#   databaseSettings = databaseSettings,
#   propensitySettings = propensitySettings,
#   baselineRiskSettings = baselineRiskSettings,
#   treatmentEffectSettings = treatmentEffectSettings
# )
#
# analysisSettings <- createAnalysisSettings(
#   threads = 8,
#   replications = 1e3,
#   validationSize = 5e5,
#   analysisId  = "analysis",
#   description = "description",
#   saveDirectory = "C:/Users/acrek/Documents/Projects/SmoothingPackages/Results"
# )
#
# predictionSettings <- createPredictionSettings(
#   args = list(
#     formula = "outcome ~ x1 + x2 + x3 + x4 + treatment",
#     family = "binomial"
#   ),
#   fun = "glm"
# )
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
#   ),
#   stratified = createSmoothSettings(
#     type = "stratified",
#     settings = SmoothHte::createStratifiedSettings(),
#     label = "stratified"
#   )
# )
#
# res <- runAnalysis(
#   analysisSettings = analysisSettings,
#   simulationSettings = simulationSettings,
#   predictionSettings = predictionSettings,
#   smoothSettings = smoothSettings
# )
#
# evaluation <- res$evaluation
# evaluation$calibration %>%
#   reshape2::melt() %>%
#   ggplot2::ggplot(
#     ggplot2::aes(x = variable, y = value, fill = variable)
#   ) +
#   ggplot2::geom_boxplot() +
#   ggplot2::theme_bw()

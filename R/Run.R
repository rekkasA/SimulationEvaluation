#' @importFrom dplyr %>%
#' @export

runSimulation <- function(
  seed,
  simulationSettings,
  predictionSettings,
  smoothSettings,
  validationDataset
) {

  set.seed(seed)
  smoothLabels <- unlist(
    rlist::list.map(                     # extract the second element of a
      smoothSettings,                    # list of lists (here the label)
      .[2]
    )
  )
  names(smoothLabels) <- NULL

  res <- tryCatch(
    {
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
      simulatedDataset <- simulatedDataset %>%
        dplyr::mutate(
          riskLinearPredictor = predict(
            predictionModel,
            newdata = simulatedDataset %>%
              dplyr::mutate(treatment = 0)
          )
        )

      simulatedDataset0 <- simulatedDataset %>%
        dplyr::filter(treatment == 0)
      simulatedDataset1 <- simulatedDataset %>%
        dplyr::filter(treatment == 1)

      pehe <- calibration <- discrimination <- list()
      for (i in seq_along(smoothLabels)) {
        selectedRows <- rep(TRUE, nrow(validationDataset))
        smoothSettingsTmp <- smoothSettings[[i]]
        smoothType <- smoothSettingsTmp$type

        if (smoothType == "loess") {
          s0 <- SmoothHte::fitLoessHte(
            data = simulatedDataset0,
            settings = smoothSettingsTmp$settings
          )
          s1 <- SmoothHte::fitLoessHte(
            data = simulatedDataset1,
            settings = smoothSettingsTmp$settings
          )
        } else if (smoothType == "rcs") {
          s0 <- SmoothHte::fitRcsHte(
            data = simulatedDataset0,
            settings = smoothSettingsTmp$settings
          )
          s1 <- SmoothHte::fitRcsHte(
            data = simulatedDataset1,
            settings = smoothSettingsTmp$settings
          )
        } else if (smoothType == "locfit") {
          s0 <- SmoothHte::fitLocfitHte(
            data = simulatedDataset0,
            settings = smoothSettingsTmp$settings
          )
          s1 <- SmoothHte::fitLocfitHte(
            data = simulatedDataset1,
            settings = smoothSettingsTmp$settings
          )
        } else {
          stratifiedHte <- SmoothHte::fitStratifiedHte(
            data = simulatedDataset,
            settings = smoothSettingsTmp$settings
          )
        }

        if (smoothType == "stratified") {
          predictedBenefit <- SmoothHte::predictStratifiedBenefit(
            p             = plogis(validationDataset$riskLinearPredictor),
            stratifiedHte = stratifiedHte
          )
        } else {
          predictedBenefit <- SmoothHte::predictBenefit(
            p               = plogis(validationDataset$riskLinearPredictor),
            smoothControl   = s0,
            smoothTreatment = s1
          )
        }

        nas <- sum(is.na(predictedBenefit))
        if (nas > 0) {
          ParallelLogger::logWarn(
            paste(
              "There were", nas,
              "NAs produced for seed:", seed
            )
          )
          selectedRows[which(is.na(predictedBenefit))] <- FALSE
        }

        calibrationData <- data.frame(
          predictedBenefit = predictedBenefit[selectedRows],
          outcome          = validationDataset[selectedRows, ]$outcome,
          treatment        = validationDataset[selectedRows, ]$treatment
        )

        discriminationData <- data.frame(
          treatment        = validationDataset[selectedRows, ]$treatment,
          outcome          = validationDataset[selectedRows, ]$outcome,
          predictedBenefit = predictedBenefit[selectedRows]
        )

        calibration[[i]] <- calculateCalibrationForBenefit(
          data   = calibrationData,
          strata = 4,
          type   = smoothType
        )

        discrimination[[i]] <- calculateDiscriminationForBenefit(
          data   = discriminationData,
          strata = 4,
          type   = smoothType
        )

        pehe[[i]] <- SimulateHte::calculatePEHE(
          data             = validationDataset[selectedRows, ],
          predictedBenefit = predictedBenefit[selectedRows]
        )
      }
      names(pehe) <- names(calibration) <- names(discrimination) <- smoothLabels
      list(
        pehe           = pehe,
        discrimination = discrimination,
        calibration    = calibration
      )
    },
    error = function(e) {
      e$message
    }
  )
  if (is.character(res)) {
    ParallelLogger::logWarn(paste("Run failed for seed:"), seed)
    return(NULL)
  } else {
    ParallelLogger::logTrace(paste("Run finished for seed:"), seed)
    return(res)
  }
}


#' @importFrom data.table setDT
#' @export
runAnalysis <- function(
  analysisSettings,
  simulationSettings,
  predictionSettings,
  smoothSettings
) {

  analysisPath <- file.path(
    analysisSettings$saveDirectory,
    analysisSettings$analysisId
  )

  if (!dir.exists(analysisPath)) {
    dir.create(analysisPath, recursive = TRUE)
  }

  logFileName <- file.path(
    analysisPath,
    "log.txt"
  )

  ParallelLogger::clearLoggers()
  logger <- ParallelLogger::createLogger(
    name = "Simulation",
    threshold = "INFO",
    appenders = list(
      ParallelLogger::createFileAppender(
        layout = ParallelLogger::layoutParallel,
        fileName = logFileName
      ),
      ParallelLogger::createConsoleAppender(
        ParallelLogger::layoutTimestamp
      )
    )
  )
  ParallelLogger::registerLogger(logger)
  ParallelLogger::logInfo("Starting simulation")

  if (is.null(analysisSettings$seeds)) {
    x <- sample(
      x       = 1e5,
      size    = analysisSettings$replications,
      replace = FALSE
    )
    analysisSettings$seeds <- x
  } else {
    x <- analysisSettings$seeds
  }
  ParallelLogger::logInfo("Genarated seeds")
  ParallelLogger::logInfo("Starting generation of the validation dataset")
  validationDatabaseSettings <- simulationSettings$databaseSettings
  validationDatabaseSettings$numberOfObservations <- analysisSettings$validationSize
  validationDataset <- SimulateHte::runDataGeneration(
    databaseSettings        = validationDatabaseSettings,
    propensitySettings      = simulationSettings$propensitySettings,
    baselineRiskSettings    = simulationSettings$baselineRiskSettings,
    treatmentEffectSettings = simulationSettings$treatmentEffectSettings
  )
  ParallelLogger::logInfo("Done")

  ParallelLogger::logInfo("Running simulations...")
  cl <- ParallelLogger::makeCluster(analysisSettings$threads)
  res <- ParallelLogger::clusterApply(
    x                  = x,
    cl                 = cl,
    fun                = runSimulation,
    simulationSettings = simulationSettings,
    predictionSettings = predictionSettings,
    smoothSettings     = smoothSettings,
    validationDataset  = validationDataset
  )
  ParallelLogger::stopCluster(cl)
  ParallelLogger::logInfo("Done")

  evaluation <- list()
  for (i in 1:3) {
    tmp <- rlist::list.map(res, .[i])
    evaluation[[i]] <- do.call(dplyr::bind_rows, lapply(tmp, dplyr::bind_rows))
  }

  names(evaluation) <- c(
    "rmse",
    "discrimination",
    "calibration"
  )

  settings <- list(
    analysisSettings   = analysisSettings,
    simulationSettings = simulationSettings,
    predictionSettings = predictionSettings,
    smoothSettings     = smoothSettings
  )


  saveRDS(
    settings,
    file = file.path(
      analysisPath,
      "settings.rds"
    )
  )

  saveRDS(
    evaluation,
    file = file.path(
      analysisPath,
      "evaluation.rds"
    )
  )

  ParallelLogger::logInfo("Simulation finished without errors")
  ParallelLogger::unregisterLogger(logger)
  ParallelLogger::clearLoggers()

  logger <- ParallelLogger::createLogger(
    name = "SIMPLE",
    threshold = "INFO",
    appenders = list(
      ParallelLogger::createConsoleAppender(
        layout = ParallelLogger::layoutTimestamp
      )
    )
  )
  return(
    list(
      settings   = settings,
      evaluation = evaluation
    )
  )

}

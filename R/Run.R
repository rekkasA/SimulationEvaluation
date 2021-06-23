#' @importFrom dplyr %>%
#' @export

runSimulation <- function(
  seed,
  simulationSettings,
  predictionSettings,
  smoothSettings,
  validationDataset,
  includeAdaptive
) {

  #-----------------------------------------------------------------------------
  # Add a check when includeAdaptive = TRUE to ensure that a constant approach
  # and a smooth approach are included in the smoothSettings
  #-----------------------------------------------------------------------------

  set.seed(seed)
  smoothLabels <- unlist(
    rlist::list.map(                     # extract the element called label
      smoothSettings,                    # from each list
      label
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

      pehe <- calibration <- discrimination <- concordance <- list()
      for (i in seq_along(smoothLabels)) {
        selectedRows <- rep(TRUE, nrow(validationDataset))
        smoothSettingsTmp <- smoothSettings[[i]]$settings
        smoothType <- class(smoothSettingsTmp)[2]

        if (smoothType == "loess") {
          s0 <- SmoothHte::fitLoessHte(
            data = simulatedDataset0,
            settings = smoothSettingsTmp
          )
          s1 <- SmoothHte::fitLoessHte(
            data = simulatedDataset1,
            settings = smoothSettingsTmp
          )
        } else if (smoothType == "rcs") {
          smoothFit <- SmoothHte::fitRcsHte(
            data = simulatedDataset,
            settings = smoothSettingsTmp
          )
        } else if (smoothType == "locfit") {
          smoothFit <- SmoothHte::fitLocfitHte(
            data = simulatedDataset,
            settings = smoothSettingsTmp
          )
        } else if (smoothType == "stratified") {
          smoothFit <- SmoothHte::fitStratifiedHte(
            data = simulatedDataset,
            settings = smoothSettingsTmp
          )
        } else if (smoothType == "modelBased") {
          smoothFit <- SmoothHte::fitModelBasedHte(
            data = simulatedDataset,
            settings = smoothSettingsTmp
          )
        } else if (smoothType == "adaptive") {
          smoothFit <- SmoothHte::fitAdaptive(
            data = simulatedDataset,
            settings = smoothSettingsTmp
          )
        }

        if (smoothType == "stratified") {
          predictedBenefit <- SmoothHte::predictStratifiedBenefit(
            p             = plogis(validationDataset$riskLinearPredictor),
            stratifiedHte = smoothFit
          )
        } else if (smoothType == "loess") {
          predictedBenefit <- SmoothHte::predictBenefitLoess(
            p  = plogis(validationDataset$riskLinearPredictor),
            s0 = s0,
            s1 = s1
          )
        } else {
          predictedBenefit <- SmoothHte::predictSmoothBenefit(
            p         = plogis(validationDataset$riskLinearPredictor),
            smoothFit = smoothFit
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

        evaluationData <- dplyr::tibble(
          predictedBenefit = predictedBenefit[selectedRows],
          outcome          = validationDataset[selectedRows, ]$outcome,
          treatment        = validationDataset[selectedRows, ]$treatment
        )
        pehe[[i]] <- SimulateHte::calculatePEHE(
          data             = validationDataset[selectedRows, ],
          predictedBenefit = predictedBenefit[selectedRows]
        )


        tmp <- concordanceCai(evaluationData)
        concordance[i] <- tmp$concordance$value

        discrimination[[i]] <- SmoothHte::calculateCForBenefit(
          data = evaluationData
        )

        tmp <- SmoothHte::calculateCalibrationForBenefit(
          data = evaluationData
        )
        calibration[[i]] <- tmp$ici
      }
      names(pehe) <- names(discrimination) <- names(calibration) <- names(concordance) <- smoothLabels
      list(
        pehe           = pehe,
        discrimination = discrimination,
        calibration    = calibration,
        concordance    = concordance
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
  smoothSettings,
  includeAdaptive = TRUE
) {

  set.seed(analysisSettings$seed)
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

  # ---- x: contains seeds ----
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
  ParallelLogger::clusterRequire(cl, "dplyr")
  res <- ParallelLogger::clusterApply(
    x                  = x,
    cl                 = cl,
    fun                = runSimulation,
    simulationSettings = simulationSettings,
    predictionSettings = predictionSettings,
    smoothSettings     = smoothSettings,
    validationDataset  = validationDataset,
    includeAdaptive    = includeAdaptive
  )
  ParallelLogger::stopCluster(cl)
  ParallelLogger::logInfo("Done")

  evaluation <- list()
  for (i in 1:4) {
    tmp <- rlist::list.map(res, .[i])
    evaluation[[i]] <- do.call(dplyr::bind_rows, lapply(tmp, dplyr::bind_rows))
  }

  names(evaluation) <- c(
    "rmse",
    "discrimination",
    "calibration",
    "concordance"
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

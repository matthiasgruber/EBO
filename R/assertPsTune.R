assertPsTune = function(psTune) {
  # check class
  checkmate::assertClass(psTune, classes = c("ParamSet"))
  # stop if no hyperparameter space is passed
  if (getParamNr(psTune) == 0L) {
    stop("No hyperparameters were passed!")
  }
  # check if initial design is passed correctly
  if (!is.null(psTune[["pars"]]$design)) {
    # check if initial design is passed as a discrete parameter set
    if (!psTune[["pars"]]$design$type == "discrete") {
      stop("Tuning the initial design only works if it is passed as an discrete parameter!")
    }
    # check if initial design is passed as LHS function or random
    if (isFALSE(psTune[["pars"]]$design$values %in% c("maximinLHS", "optimumLHS", "randomLHS",
                                                      "geneticLHS", "random", "improvedLHS"))) {
      stop("Tuning the initial design only works for: maximinLHS, optimumLHS, randomLHS,
           geneticLHS, random, improvedLHS!")
    }
  }
  # check if amount of initial design is passed correctly
  if (!is.null(psTune[["pars"]]$amountDesign)) {
    # check if initial design is passed as a integer parameter set
    if (!psTune[["pars"]]$amountDesign$type == "integer") {
      stop("Tuning the amount of initial design only works if it is passed as an integer parameter!")
    }
  }
  # check if infill criteria is passed correctly
  if (is.null(psTune[["pars"]]$crit)) stop("Please pass at the minimum one infill criterion via the tuning parameter space!")
  if (!is.null(psTune[["pars"]]$crit)) {
    # check if initial design is passed as a discrete parameter set
    if (!psTune[["pars"]]$crit$type == "discrete") {
      stop("Tuning the infill criterion only works if it is passed as an discrete parameter!")
    }
    if (isFALSE(psTune[["pars"]]$crit$values %in% c("makeMBOInfillCritEI",
                                                    "makeMBOInfillCritAEI",
                                                    "makeMBOInfillCritCB",
                                                    "makeMBOInfillCritAdaCB",
                                                    "makeMBOInfillCritMeanResponse",
                                                    "makeMBOInfillCritStandardError"))) {
      stop("Tuning the infill criterion only works for: makeMBOInfillCritEI, makeMBOInfillCritAEI, makeMBOInfillCritCB,
           makeMBOInfillCritAdaCB, makeMBOInfillCritMeanResponse, makeMBOInfillCritStandardError!")
    }
  }
  # check if surrogate model is passed correctly
  if (is.null(psTune[["pars"]]$surrogate)) stop("Please pass at the minimum one surrogate via the tuning parameter space!")
  if (!is.null(psTune[["pars"]]$surrogate)) {
    # check if surrogate model is passed as a discrete parameter set
    if (!psTune[["pars"]]$surrogate$type == "discrete") {
      stop("Tuning the surrogate only works if it is passed as an discrete parameter!")
    }
    if (isFALSE(psTune[["pars"]]$surrogate$values %in% c("regr.randomForest",
                                                         "regr.km"))) {
      stop("Tuning the surrogate only works for: regr.randomForest, regr.km!")
    }
  }
  # check if kernel is connected to kriging and if right kernels are passed
  if (isTRUE(c("regr.km") %in% psTune[["pars"]]$surrogate$values)) {
    if (isTRUE(c("covtype") %in% names(psTune[["pars"]]))) {
      if (!(psTune[["pars"]][["covtype"]][["requires"]][[3]] == "regr.km")) {
        stop("Tuning the kernel of kriging has to be conntected to regr.km")
      }
      if (isFALSE(psTune[["pars"]][["covtype"]][["values"]] %in% c("gauss",
                                                                   "matern5_2",
                                                                   "matern3_2",
                                                                   "powexp"))) {
        stop("Tuning the kernels of kriging only works for: gauss, matern5_2, matern3_2 and powexp!")
      }
    }
  }
}

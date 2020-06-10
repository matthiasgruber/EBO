assertPsTuneES = function(psTune) {
  # check class
  checkmate::assertClass(psTune, classes = c("ParamSet"))

  # check names of hyperparameters
  name = names(psTune$pars)
  for (i in 1:length(name)) {
    checkmate::assertChoice(name[i], c("nu", "mue", "sigmaInit", "nSigma", "mutation", "tau",
                                       "stratReco", "objReco"))
  }

  # stop if no hyperparameter space is passed
  if (getParamNr(psTune) == 0L) {
    stop("No hyperparameters were passed!")
  }

  # check if nu is passed correctly
  if (!is.null(psTune[["pars"]]$nu)) {
    # check if nu is passed as integer
    if (!psTune[["pars"]]$nu$type == "integer") {
      stop("Tuning nu only works if it is passed as a integer parameter!")
    }
  }
  # check if mue is passed correctly
  if (!is.null(psTune[["pars"]]$mue)) {
    # check if mue is passed as integer
    if (!psTune[["pars"]]$mue$type == "integer") {
      stop("Tuning mue only works if it is passed as a integer parameter!")
    }
  }
  # check if sigmaInit is passed correctly
  if (!is.null(psTune[["pars"]]$sigmaInit)) {
    # check if sigmaInit is passed as numeric
    if (!psTune[["pars"]]$sigmaInit$type == "numeric") {
      stop("Tuning sigmaInit only works if it is passed as a numeric parameter!")
    }
  }
  # check if nSigma is passed correctly
  if (!is.null(psTune[["pars"]]$nSigma)) {
    # check if nSigma is passed as integer
    if (!psTune[["pars"]]$nSigma$type == "integer") {
      stop("Tuning nSigma only works if it is passed as a integer parameter!")
    }
  }

  # check if tau is passed correctly
  if (!is.null(psTune[["pars"]]$tau)) {
    # check if tau is passed as numeric
    if (!psTune[["pars"]]$tau$type == "numeric") {
      stop("Tuning tau only works if it is passed as a numeric parameter!")
    }
  }
  # check if mutation is passed correctly
  if (!is.null(psTune[["pars"]]$mutation)) {
    # check if mutation is passed as integer
    if (!psTune[["pars"]]$mutation$type == "integer") {
      stop("Tuning mutation only works if it is passed as a integer parameter!")
    }
    checkmate::assertIntegerish(psTune[["pars"]]$mutation$lower, lower = 1, upper = 2, any.missing = FALSE,
                                len = 1)
    checkmate::assertIntegerish(psTune[["pars"]]$mutation$upper, lower = 1, upper = 2, any.missing = FALSE,
                                len = 1)
    }
  # check if stratReco is passed correctly
  if (!is.null(psTune[["pars"]]$stratReco)) {
    # check if stratReco is passed as integer
    if (!psTune[["pars"]]$stratReco$type == "integer") {
      stop("Tuning stratReco only works if it is passed as a integer parameter!")
    }
    checkmate::assertIntegerish(psTune[["pars"]]$stratReco$lower, lower = 1, upper = 4, any.missing = FALSE,
                                len = 1)
    checkmate::assertIntegerish(psTune[["pars"]]$stratReco$upper, lower = 1, upper = 4, any.missing = FALSE,
                                len = 1)
  }
  # check if objReco is passed correctly
  if (!is.null(psTune[["pars"]]$objReco)) {
    # check if objReco is passed as integer
    if (!psTune[["pars"]]$objReco$type == "integer") {
      stop("Tuning objReco only works if it is passed as a integer parameter!")
    }
    checkmate::assertIntegerish(psTune[["pars"]]$objReco$lower, lower = 1, upper = 4, any.missing = FALSE,
                                len = 1)
    checkmate::assertIntegerish(psTune[["pars"]]$objReco$upper, lower = 1, upper = 4, any.missing = FALSE,
                                len = 1)
  }
}

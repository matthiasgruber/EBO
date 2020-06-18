optimizeES = function(configuration, objSPOTFunc, info, funcEvals) {

  # compute ES and define Hyperparameters
  res = configESFunc(instance = list(objSPOTFunc, info),
                     funcEvals = funcEvals,
                     #define Hyperparameters of ES Algrotihm
                     nu = ifelse(!is.null(configuration$nu), nu <- configuration$nu, nu <- 10),#if (!is.null(configuration$nu))  nu = configuration$nu,

                     mue = ifelse(!is.null(configuration$mue), mue <- configuration$mue, mue <- 10),

                     sigmaInit = ifelse(!is.null(configuration$sigmaInit), sigmaInit <- configuration$sigmaInit, sigmaInit <- 1.0),

                     nSigma = ifelse(!is.null(configuration$nSigma), nSigma <- configuration$nSigma, nSigma <- 1),

                     mutation = ifelse(!is.null(configuration$mutation), mutation <- configuration$mutation, mutation <- 1),

                     tau = ifelse(!is.null(configuration$tau), tau <- configuration$tau, tau <- 1.0),

                     stratReco = ifelse(!is.null(configuration$stratReco), stratReco <- configuration$stratReco, stratReco <- 2),

                     objReco = ifelse(!is.null(configuration$objReco), objReco <- configuration$objReco, objReco <- 2)
                     )

  return(res)
}

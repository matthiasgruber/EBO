optimizeDE = function(configuration, objSPOTFunc, info, funcEvals) {

  # compute DE and define Hyperparameters

  res = configDEFunc(instance = list(objSPOTFunc, info),
                     funcEvals = funcEvals,
                     #define Hyperparameters of DE Algrotihm
                     populationSize =  configuration$populationSize#

  )

  return(res)
}

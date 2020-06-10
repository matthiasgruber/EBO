optimizeGenoud = function(configuration, objSPOTFunc, info, funcEvals) {

  # compute Genoud and define Hyperparameters

  res = configGEFunc(instance = list(objSPOTFunc, info),
                     funcEvals = funcEvals,
                     #define Hyperparameters of Genoud Algrotihm
                     populationSize =  configuration$populationSize#


  )

  return(res)
}

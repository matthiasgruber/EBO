optimizeRace = function(configuration, objNormal, info, funcEvals) {

  # compute mlrMBO
  res = configRacingFunc(instance = list(objNormal, info),
                         funcEvals =  funcEvals,
                         nbConfigurations = configuration$nbConfigurations,
                         minNbSurvival = configuration$minNbSurvival)
  return(res)
}

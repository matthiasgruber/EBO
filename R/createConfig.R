createConfigMbo = function(funcEvals, configMbo) {
  configMbo = cbind(funcEvals, configMbo)
  mlrMBO = list(
    mlrMBO = configMbo
  )
  return(mlrMBO)
}


createConfigCmaesr = function(funcEvals, configCmaesr) {
  configCmaesr = cbind(funcEvals, configCmaesr)
  cmaesr = list(
    cmaesr = configCmaesr
  )
  return(cmaesr)
}

createConfigEs = function(funcEvals, configEs) {
  configEs = cbind(funcEvals, configEs)
  SpotES = list(
    SpotES = configEs
  )
  return(SpotES)
}

createConfigDe = function(funcEvals, configDe) {
  configDe = cbind(funcEvals, configDe)
  SpotDE = list(
    SpotDE = configDe
  )
  return(SpotDE)
}

createConfigGe = function(funcEvals, configGe) {
  configGe = cbind(funcEvals, configGe)
  SpotGE = list(
    SpotGE = configGe
  )
  return(SpotGE)
}

createConfigRacing = function(funcEvals) {
  irace = list(
    irace = as.data.frame(funcEvals)
  )
  return(irace)
}


createConfigRandom = function(funcEvals) {
  random = list(
    random = as.data.frame(funcEvals)
  )
  return(random)
}

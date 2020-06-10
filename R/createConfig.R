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
  es = list(
    es = configEs
  )
  return(es)
}

createConfigDe = function(funcEvals, configDe) {
  configDe = cbind(funcEvals, configDe)
  de = list(
    de = configDe
  )
  return(de)
}

createConfigGe = function(funcEvals, configGe) {
  configGe = cbind(funcEvals, configGe)
  ge = list(
    ge = configGe
  )
  return(ge)
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

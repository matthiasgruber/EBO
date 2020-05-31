executeComputation = function(reg, ncpus) {
  # define how many cpu cores will be used for the computation
  # if ncpus = NA --> default --> detect how many cores there are and
  # leave one for netflix
  if (is.na(ncpus)) ncpus = parallel::detectCores() - 1
  reg$cluster.functions = batchtools::makeClusterFunctionsSocket(ncpus = ncpus, fs.latency = 65)
  # submit jobs
  batchtools::submitJobs()
  # wait until computation is finished (ie TRUE in console)
  batchtools::waitForJobs()
  # get the Jobs which caused an error
  errors = batchtools::findErrors()
  # submit the errors again - maybe it will be enough ...
  batchtools::submitJobs(errors)
  # wait until computation is finished (ie TRUE in console)
  batchtools::waitForJobs()
}

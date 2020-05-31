plotAutoMbo = function(resMboDefault, resDmboTuned, minFuncEvals, funcEvals, repls, showInfo, info,
                       ncpus, seed, step, startTime, steps, hyperparamsDmbo) {

  optimizationPath = list()
  results = list()
  targetColumn = info$featureNumber+1
  for (i in 1:(repls*2)) {
    if (i <= repls*1) optimizationPath[[i]] = as.data.frame(resMboDefault[[i]][["optimizationPathMBO"]]$opt.path)
    if (i > repls*1) {
      if (i <= repls*2) {
        optimizationPath[[i]] = resDmboTuned[[steps]][[2]][[i-repls*1]]
        optimizationPath[[i]]["dob"] = c(rep(0,(info$featureNumber + 1)), 1:(funcEvals-(info$featureNumber+1)))
      }
    }
    # compute the best y found so far. iteration 0 = initial data
    # add a column to the data frame optimization path to save the best y
    optimizationPath[[i]]["bestY"] = NA
    numberExperiments = length(optimizationPath[[i]]$dob)
    numberInitialDesign = sum(optimizationPath[[i]]$dob == 0)
    # compute the best y over the initial data
    if (info$minimize == FALSE) {
      optimizationPath[[i]]$bestY[1:numberInitialDesign] <-
        max(optimizationPath[[i]][1:numberInitialDesign,targetColumn])
    }
    if (info$minimize == TRUE) {
      optimizationPath[[i]]$bestY[1:numberInitialDesign] <-
        min(optimizationPath[[i]][1:numberInitialDesign,targetColumn])
    }
    for (ii in (numberInitialDesign + 1):numberExperiments) {
      if (info$minimize == FALSE) {
        optimizationPath[[i]]$bestY[ii] <-
          max(optimizationPath[[i]][[info$y.name]][ii], optimizationPath[[i]]$bestY[ii - 1])
      }
      if (info$minimize == TRUE) {

        optimizationPath[[i]]$bestY[ii] <-
          min(optimizationPath[[i]][[info$y.name]][ii], optimizationPath[[i]]$bestY[ii - 1])
      }
    }
    y = optimizationPath[[i]]$bestY[numberInitialDesign:numberExperiments]
    x = optimizationPath[[i]]$dob[numberInitialDesign:numberExperiments]

    result = cbind(data.frame(y),data.frame(x))
    results[[i]] = result
  }
  results = unclass(results)
  results = as.data.frame(results)

  iterationCharacter = as.character(c(0:(funcEvals-numberInitialDesign)))

  auxiliaryVariable = 2

  resultsPlotable = list()

  for (j in 1:(2*repls)) {
    resultsPlotable[[j]] = t(results[1,seq(1+(auxiliaryVariable*(j-1)),
                                           auxiliaryVariable+(auxiliaryVariable*(j-1)),2)])
    for (i in 2:(funcEvals+1-numberInitialDesign)) {
      resultsPlotable[[j]] =
        rbind(resultsPlotable[[j]], t(results[i,seq(1+(auxiliaryVariable*(j-1)),
                                                    auxiliaryVariable+(auxiliaryVariable*(j-1)),2)]))
    }
    resultsPlotable[[j]] = data.frame(resultsPlotable[[j]])
    resultsPlotable[[j]]$iteration = c(rep(0:(funcEvals-numberInitialDesign), each = 1))
    resultsPlotable[[j]]$iteration = ordered(resultsPlotable[[j]]$iteration, levels = iterationCharacter)
  }

  namesBoxplot = c("SMBO: design: maximinLHS, amountDesign: see DMBO",
                   "DMBO tuned")

  hyperparamsDmbo[[1]] = data.frame(design = "maximinLHS",
                                    amountDesign = as.character(info$featureNumber+1),
                                    stringsAsFactors = FALSE)

  namesDmbo = list()
  hyperparameterDmboPaste = list()
  for (i in 1:steps) {
    hyperparamsDmbo[[i]]$y = NULL
    if (i > 1) {
      hyperparamsDmbo[[i]] = hyperparamsDmbo[[i]][,-which(is.na(hyperparamsDmbo[[i]]))]
    }
    namesDmbo[[i]] = colnames(hyperparamsDmbo[[i]])
    hyperparameterDmboPaste[[i]] = paste(
      lapply(namesDmbo[[i]], function(n) { paste(n, hyperparamsDmbo[[i]][n], sep = ": ") }), collapse = ", ")
  }
  stepCharacter = c(1:steps)
  namesBoxplot[2] = paste("DMBO tuned: ", paste(
    lapply(stepCharacter, function(n) { paste("step", stepCharacter[n], hyperparameterDmboPaste[[n]], sep = ": ") }), collapse = '\n'))

  for (j in 1:2) {
    for (i in (1:(repls))) {
      resultsPlotable[[i+((j-1)*(repls))]]$class = namesBoxplot[j]
    }
  }

  resultsPlotable = data.table::rbindlist(resultsPlotable)
  resultsPlotable$class = as.factor(resultsPlotable$class)

  markSteps = vector()
  for (i in 1:(steps - 1)) {
    markSteps[i] = (minFuncEvals * i + 1)
  }

  boxplotCurve = ggplot2::ggplot(resultsPlotable, aes(x = iteration, y = resultsPlotable..j.., fill = class)) +
    theme(legend.position = "bottom", legend.direction = "vertical") +
    stat_summary(fun = median, geom ="line", aes(group = class, color = class)) +
    #geom_boxplot(alpha = 0.3, outlier.shape = NA) +
    ylab(info$y.name) +
    geom_vline(xintercept = markSteps, color = "red")

  if (funcEvals > 50) {
    howMany = floor(length(iterationCharacter)/20)
    breaksVec = iterationCharacter[seq(1, length(iterationCharacter), howMany)]
    boxplotCurve = boxplotCurve + scale_x_discrete(breaks = breaksVec)
  }

  endTime <- Sys.time()
  timeTaken <- round(endTime - startTime,2)

  if (showInfo == TRUE) {
    boxplotCurve = addInfo(boxplotCurve, info, timeTaken, repls)
  }

  return(boxplotCurve)
}

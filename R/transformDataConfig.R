transformDataConfig = function(resMbo, repls, namesBoxplot, numberInstances = 6, funcEvals) {
  optimizationPath = list()
  results = list()
  targetColumn = info$featureNumber+1
  numberBoxplotCurve = length(namesBoxplot)
  for (i in 1:((repls*numberBoxplotCurve)*numberInstances)) {
    optimizationPath[[i]] = as.data.frame(resMBO[[i]][["optimizationPathMBO"]]$opt.path)
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

  iterationCharacter = as.character(c(0:funcEvals))

  auxiliaryVariable = length(results)/(numberBoxplotCurve*3)

  resultsPlotable = NULL

  for (j in 1:numberBoxplotCurve*3) {
    if (j == 1) {
      for (i in 1:(funcEvals+1)) {
        resultsPlotable[[j]] =
          rbind(resultsPlotable[[j]], t(results[i,seq(1+(auxiliaryVariable*(j-1)),
                                                      auxiliaryVariable+(auxiliaryVariable*(j-1)),2)]))
      }
    }

    if (j >= 2) {
      resultsPlotable[[j]] = t(results[1,seq(1+(auxiliaryVariable*(j-1)),
                                             auxiliaryVariable+(auxiliaryVariable*(j-1)),2)])
      for (i in 2:(funcEvals+1)) {
        resultsPlotable[[j]] = rbind(resultsPlotable[[j]],
                                     t(results[i,seq(1+(auxiliaryVariable*(j-1)),
                                                     auxiliaryVariable+(auxiliaryVariable*(j-1)),2)]))
      }
    }
    resultsPlotable[[j]] = data.frame(resultsPlotable[[j]])
    resultsPlotable[[j]]$class = namesBoxplot[j]
    resultsPlotable[[j]]$iteration = c(rep(0:funcEvals, each = repls))
    resultsPlotable[[j]]$iteration = ordered(resultsPlotable[[j]]$iteration, levels = iterationCharacter)
  }


  resultsPlotable = data.table::rbindlist(resultsPlotable)
  resultsPlotable$class = as.factor(resultsPlotable$class)

  return(resultsPlotable)
}

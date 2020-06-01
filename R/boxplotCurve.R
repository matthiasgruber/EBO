boxplotCurve = function(configResults) {

  #y.name = as.name(colnames(configResults)[1])

  #iters = array(0,c(length(configResults$iteration),1))

  #analyseItersData = configResults[((configResults$iteration == analyseIters[1]) | (configResults$iteration == (analyseIters[2]))),]

  #n = length(configResults$iteration)

  #for(i in 1:n){
  #  ifelse((configResults[i,3] <= analyseIters[1]), iters[i] <- "baseIters", iters[i] <- "addIters")
  #}

  #configResults = cbind(configResults, iters)
  #colnames(configResults)[4] = "iters"
  #colnames(configResults)[1] = 'y_best'

  #configResults$iters = ordered(configResults$iters, levels = c("baseIters","addIters"))

  ###
  y.name = as.name(colnames(configResults)[1])
  numberBoxplotCurve = nlevels(configResults$class)
  repls = length(which(configResults$iteration == "0"))/numberBoxplotCurve
  namesBoxplot = levels(configResults$class)
  itersMbo = nlevels(configResults$iteration)-1

  colnames(configResults)[1] = 'y_best'

  # get final_ybest median
  #final_iteration = configResults[configResults$iteration == itersMbo,]
  #final_ybest = median(final_iteration)
  # get 10% improvement of final_ybest


  bxp = ggplot2::ggplot(configResults, aes(x = iteration, y = y_best, fill = class)) +
    theme(legend.position = "bottom", legend.direction = "vertical") +
    geom_boxplot(outlier.shape = NA) +
    ylab(y.name) +
    #geom_vline(xintercept = baseIters + 1, color = "blue")
    stat_summary(fun = median, geom ="line", aes(group=class, color = class))
  #scale_y_continuous(expand = c(0,0), limits = c(0,4.8))
  #boxplotCurve = bxp + facet_wrap( ~ iters, scales = "free")

  return(bxp)
}

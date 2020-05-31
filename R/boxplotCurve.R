boxplotCurve = function(configResults, analyseIters = NA) {

  y.name = as.name(colnames(configResults)[1])

  iters = array(0,c(length(configResults$iteration),1))

  #analyseItersData = configResults[((configResults$iteration == analyseIters[1]) | (configResults$iteration == (analyseIters[2]))),]

  n = length(configResults$iteration)

  for(i in 1:n){
    ifelse((configResults[i,3] <= analyseIters[1]), iters[i] <- "baseIters", iters[i] <- "addIters")
  }

  configResults = cbind(configResults, iters)
  colnames(configResults)[4] = "iters"
  colnames(configResults)[1] = 'y_best'

  configResults$iters = ordered(configResults$iters, levels = c("baseIters","addIters"))


  bxp = ggplot2::ggplot(configResults, aes(x = iteration, y = y_best, fill = class)) +
    theme(legend.position = "bottom", legend.direction = "vertical") +
    geom_boxplot() +
    ylab(y.name) +
    #geom_vline(xintercept = baseIters + 1, color = "blue")
  stat_summary(fun.y = median, geom ="line", aes(group=class, color = class)) +
  geom_boxplot(outlier.shape = NA)
  #scale_y_continuous(expand = c(0,0), limits = c(0,4.8))
  boxplotCurve = bxp + facet_wrap( ~ iters, scales = "free")


  return(boxplotCurve)
}

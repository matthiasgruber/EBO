addItersTest = function(configResults, baseIters = 20, addIters = 10, minimize = TRUE) {

  # create info from configResults dataframe
  y.name = as.name(colnames(configResults)[1])

  numberBoxplotCurve = nlevels(configResults$class)
  repls = length(which(configResults$iteration == "0"))/numberBoxplotCurve
  namesBoxplot = levels(configResults$class)
  itersMbo = nlevels(configResults$iteration)-1

  colnames(configResults)[1] = 'y_best'

  # create date for running Tests
  #testdata = configResults[configResults$V1 != 0,]

  testdata = configResults[((configResults$iteration == baseIters) | (configResults$iteration == (baseIters+addIters))),]

  ### write an algorithm to get results in right structure for running test

  #testdata$iteration = NULL
  colnames(testdata) = c("y_best", "class", "iters")

  testdata$iters = ordered(testdata$iters, levels = c(baseIters,(baseIters+addIters)))

  test = vector(mode = "list", length = numberBoxplotCurve)
  summary = vector(mode = "list", length = numberBoxplotCurve)

  for(r in 1:numberBoxplotCurve) {
    test[[r]] = testdata[(testdata$class == namesBoxplot[r]),]
    names(test)[r] = namesBoxplot[r]
    summary[[r]] = test[[r]] %>%
      ggpubr::group_by(iters, class) %>%
      rstatix::get_summary_stats(y_best, type = "five_number")
  }


  # create test for each config
  #steps = repls*2

  #end_first = steps
  #end_last = repls*2*numberBoxplotCurve
  #end_index = seq(from = end_first, to = end_last, by = steps)

  #start_first = 1
  #start_last = end_last-steps+1
  #start_index = seq(from = start_first, to = start_last, by = steps)

  min_max = ifelse(minimize == TRUE, "g", "l")

  testResults = vector(mode = "list", length = numberBoxplotCurve)

  for(r in 1:numberBoxplotCurve) {
    testResults[[r]] = wilcox.test(test[[r]]$y_best ~ test[[r]]$iters, alternative = min_max, data = test[[r]], paired = TRUE)
    names(testResults)[r] = namesBoxplot[r]
  }

  p = ggplot(data = testdata, aes(x=class, y=y_best)) +
    geom_boxplot(aes(fill=iters))

  bxp = p + facet_wrap( ~ class, scales = "free")

  return(list(bxp, summary, testResults))
}

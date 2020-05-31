testConfigs = function(configResults) {

  # get critical info from configResults dataframe
  y.name = as.name(colnames(configResults)[1])
  numberBoxplotCurve = nlevels(configResults$class)
  repls = length(which(configResults$iteration == "0"))/numberBoxplotCurve
  namesBoxplot = levels(configResults$class)
  itersMbo = nlevels(configResults$iteration)-1

  colnames(configResults)[1] = 'y_best'

  # create new test_dataframe
  testdata = configResults[configResults$iteration == itersMbo,]

  summary = testdata %>%
    ggpubr::group_by(class) %>%
    rstatix::get_summary_stats(y_best, type = c("five_number"))

  if(numberBoxplotCurve == "2") {

    boxplot = ggpubr::ggpaired(testdata, x = "class", y = "y_best", point.size = 0, line.size = 0,
                               order = namesBoxplot,
                               ylab = "y_best", xlab = "class")

    testdata$class = ordered(testdata$class, levels = namesBoxplot)

    wilcox.test(testdata$y_best ~ testdata$class, alternative = "two.sided", data = testdata, paired = FALSE)


    return(list(summary, plot(boxplot), test))


  } else {

    boxplot = ggpubr::ggboxplot(testdata, x = "class", y = "y_best",
                                color = "class", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                                order = namesBoxplot,
                                ylab = "y_best", xlab = "class")

    testdata$class = ordered(testdata$class, levels = namesBoxplot)

    test = kruskal.test(y_best ~ class, data = testdata)

    comparison = pairwise.wilcox.test(testdata$y_best, testdata$class,
                                      p.adjust.method = "BH")

    return(list(summary, plot(boxplot), test, comparison))
  }

}


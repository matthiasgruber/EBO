addInfo = function(plot, info, timeTaken, repls) {
  # add information to plot
  plot = plot +
    labs(title = paste("data:",info$dataName)) +
    labs(subtitle = paste(paste(" feature(s) name:", paste(info[["featureName"]], collapse = ", ")), '\n',
                          paste("feature(s) type:", paste(info[["featureType"]], collapse = ", ")), '\n',
                          paste("target(s) name:", paste(info[["y.name"]], collapse = ", ")), '\n',
                          paste("direction:", paste("maximize")), '\n',
                          paste("replications:", paste(repls)))) +
    labs(caption = paste("time:", timeTaken, units(timeTaken)))

  return(plot)
}

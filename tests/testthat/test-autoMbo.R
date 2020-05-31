library(testthat)
library(checkmate)

test_that("Test if works for mixed ps + every argument in use + maximize", {
  set.seed(1)
  data <- data.frame(a=runif(50,10,5555),b=runif(50,-30000,-500))
  data$ratio <- rowSums(data)
  data$ratio <- data$ratio/max(data$ratio)
  colnames(data) <- c("a","t","y")

  target = c("y")

  minimize = FALSE
  funcEvals = 13
  minFuncEvals = 5
  itersMboTune = 1

  repls = 2

  plot = EBO::autoMbo(data, target, minimize, funcEvals, minFuncEvals, itersMboTune, repls, seed = 1)


  expect_equal(class(plot), c("gg", "ggplot"))
  expect_equal(length(plot), 9)
  expect_equal(length(plot$data), 3)
  expect_equal(class(plot$data$class), "factor")
  expect_equal(class(plot$data$iteration), c("ordered", "factor"))
  expect_equal(length(plot$data$iteration), 88) # mbo's * repls * (iters + 1) --> 3 * 2 * (10 + 1) = 66
})

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generateConfigdata.R
\name{generateConfigdata}
\alias{generateConfigdata}
\title{Generate benchmark data of \code{mlrMBO::mbo()} optimization runs}
\usage{
generateConfigdata(
  task,
  funcEvals = 50,
  paramsMBO = NULL,
  namesBoxplot = c("default"),
  repls = 20,
  showInfo = TRUE,
  ncpus = NA,
  seed = 1
)
}
\arguments{
\item{task}{[\code{EBO:: task()}]\cr
Task defines the problem setting.}

\item{funcEvals}{[\code{integer(1)}]\cr
Define the number of function evaluations.\cr
Default is 50.}

\item{paramsMBO}{[\code{data.table::data.table()}]\cr
A data.table containing design, amountDesign, control and surrogate as lists.
The data.table has to be defined as the expample below.}

\item{repls}{[\code{integer(1)}]\cr
Define how often each configuration is run for the benchmark.\cr
Default is 20}

\item{showInfo}{[\code{logical(1)}]\cr
Should some information be shown in the plot? \cr
Default is `TRUE`.}

\item{ncpus}{[\code{numeric(1)}]\cr
Define how many cpu cores are used for the benchmark.\cr
Default is NA, which uses all cores and leave one for netflix.}

\item{seed}{[\code{numeric(1)}]\cr
Define the seed used for the computation. Will be set by \code{batchtools}.
Which means the jobs get the seed plus the job.id as their unique seed. \cr
Default is one.}

\item{namesBoxplotCurve}{[\code{character}]\cr
The names for the \code{mlrMBO} configurations
Default is `default`.}

\item{psOpt}{[\code{ParamHelpers::ParamSet()}]\cr
Collection of parameters and their constraints for optimization.}

\item{simulation}{[\code{character}]\cr
The black box function e.g. model for the \code{mlrMBO}
Default is `regr.randomForest`.}

\item{minimize}{[\code{logical(1)}]\cr
Should the target be minimized? \cr
Default is `TRUE`.}
}
\value{
Benchmark data for each configuration.
}
\description{
This function benchmarks the \code{mlrMBO::mbo()} function on different configurations. The
resulting data can be used for several EBO functions such as EBO::boxplotCurve(), EBO::testAddIters(),
EBO::testConfigs().
}
\examples{
\dontrun{

set.seed(1)

library(mlrMBO)
library(ParamHelpers)
library(mlr)

# define infillCrit
ctrl = mlrMBO::makeMBOControl()
ctrl = mlrMBO::setMBOControlInfill(ctrl, crit = mlrMBO::makeMBOInfillCritEI())

# define MBO configuration
paramsMBO = data.table::data.table(
 design = list("maximinLHS","randomLHS", "random"),
 amountDesign = list(12),
 control = list(ctrl),
 surrogate = list(mlr::makeLearner("regr.km", predict.type = "se"))
)

namesBoxplot = c("maximinLHS",
                "randomLHS",
                "random")

# define runs of each algorithm
repls = 10

# define function evaluations
funcEvals = 32


data <- data.frame(a = runif(50,10,5555), b = runif(50,-30000,-500),
                   c = runif(50,0,1000))
data$ratio <- rowSums(data[,1:3]^2)
data$ratio <- data$ratio/max(data$ratio)
colnames(data) <- c("power", "time", "pressure","ratio")


psOpt = ParamHelpers::makeParamSet(
  ParamHelpers::makeIntegerParam("power", lower = 10, upper = 5555),
  ParamHelpers::makeIntegerParam("time", lower = -30000, upper = -500),
  ParamHelpers::makeNumericParam("pressure", lower = 0, upper = 1000),
)

task = task(
 simulation = "regr.randomForest",
 data = data,
 target = "ratio",
 psOpt = psOpt,
 minimize = FALSE
)

# generate configData
configResults = generateConfigdata(task, funcEvals = funcEvals, paramsMBO,
                                       namesBoxplot = namesBoxplot, repls = repls)
}
}
\references{
[\code{mlrMBO::mbo()}]

Bernd Bischl, Jakob Richter, Jakob Bossek, Daniel Horn, Janek Thomas and Michel Lang; mlrMBO: A Modular Framework for Model-Based Optimization of Expensive Black-Box Functions, Preprint: \code{\link{https://arxiv.org/abs/1703.03373}} (2017).
}
\seealso{
\code{\link{optimize::plotBenchmark()}} \code{\link{optimize::plotMboContourPlot()}}
}

generateProblemList = function(taskList) {

  numberProblems = length(taskList)

  problemList = vector(mode = "list", length = numberProblems)

  for(i in 1:numberProblems){
    problemList[[i]] = vector(mode = "list", length = 3)

    # name sublist
    names(problemList[[i]])[[1]] = 'data'
    names(problemList[[i]])[[2]] = 'psOpt'
    names(problemList[[i]])[[3]] = 'target'

    # define data frames
    problemList[[i]][[1]] = taskList[[i]]$data

    # define parameter spaces
    problemList[[i]][[2]] = taskList[[i]]$psOpt

    # define targets
    problemList[[i]][[3]] = taskList[[i]]$target
  }
  return(problemList)
}

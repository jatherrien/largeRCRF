

#' Save Random Forests
#'
#' Saves a random forest for later use, given that the base R
#' \code{\link{base::save}} function doesn't work for this package.
#'
#' @param forest The forest to save.
#' @param directory The directory that should be created to save the trees in.
#'   Note that if the directory already exists, an error will be displayed
#'   unless \code{overwrite} is set to TRUE.
#' @param overwrite Should the function overwrite an existing forest; FALSE by
#'   default.

#' @export
#' @seealso \code{\link{train}}, \code{\link{load_forest}}
#' @examples
#' # Regression Example
#' x1 <- rnorm(1000)
#' x2 <- rnorm(1000)
#' y <- 1 + x1 + x2 + rnorm(1000)
#'
#' data <- data.frame(x1, x2, y)
#' forest <- train(y ~ x1 + x2, data,
#'  ntree=100, numberOfSplits = 5, mtry = 1, nodeSize = 5)
#'
#' save_forest(forest, "trees")
#' new_forest <- load_forest("trees")
save_forest <- function(forest, directory, overwrite=FALSE){
  check_and_create_directory(directory, overwrite)
  
  saveTrees(forest, directory)
  
  # Next save the response combiners and the split finders
  saveForestComponents(directory, 
                       covariateList=forest$covariateList,
                       params=forest$params,
                       forestCall=forest$call)
  
}

saveTrees <- function(forest, directory){
  # This function assumes that directory is free for us to write in. 
  
  forest.java <- forest$javaObject
  
  # First save the trees
  tree.collection.java <- .jcall(forest.java, makeResponse(.class_List), "getTrees")
  numberOfTrees <- forest$params$ntree
  width = round(log10(numberOfTrees))+1
  treeNames <- paste0(directory, "/tree-", formatC(1:numberOfTrees, width=width, format="d", flag="0"), ".tree")
  for(i in 1:numberOfTrees){
    treeName <-treeNames[i]
    tree.java <- .jcall(tree.collection.java, makeResponse(.class_Object), "get", as.integer(i-1))
    tree.java <- .jcast(tree.java, .class_Serializable)
    .jcall(.class_DataUtils, "V", "saveObject", tree.java, treeName)
  }
  
}

saveForestComponents <- function(directory, covariateList, params, forestCall){
  
  nodeResponseCombiner <- params$nodeResponseCombiner
  nodeResponseCombiner.java <- .jcast(nodeResponseCombiner$javaObject, .class_Serializable)
  .jcall(.class_DataUtils, "V", "saveObject", nodeResponseCombiner.java, paste0(directory, "/nodeResponseCombiner.jData"))
  nodeResponseCombiner$javaObject <- NULL
  
  splitFinder <- params$splitFinder
  splitFinder.java <- .jcast(splitFinder$javaObject, .class_Serializable)
  .jcall(.class_DataUtils, "V", "saveObject", splitFinder.java, paste0(directory, "/splitFinder.jData"))
  splitFinder$javaObject <- NULL
  
  forestResponseCombiner <- params$forestResponseCombiner
  forestResponseCombiner.java <- .jcast(forestResponseCombiner$javaObject, .class_Serializable)
  .jcall(.class_DataUtils, "V", "saveObject", forestResponseCombiner.java, paste0(directory, "/forestResponseCombiner.jData"))
  forestResponseCombiner$javaObject <- NULL
  
  covariateList <- .jcast(covariateList, .class_Serializable)
  .jcall(.class_DataUtils, "V", "saveObject", covariateList, paste0(directory, "/covariateList.jData"))
  
  saveRDS(object=params, file=paste0(directory, "/parameters.rData"))
  saveRDS(object=forestCall, file=paste0(directory, "/call.rData"))
}

check_and_create_directory <- function(directory, overwrite){
  if(file.exists(directory) & !overwrite){
    stop(paste(directory, "already exists; will not modify it. Please remove/rename it or set overwrite=TRUE"))
  }
  else if(file.exists(directory) & overwrite){
    unlink(directory)
  }
  
  dir.create(directory)
}
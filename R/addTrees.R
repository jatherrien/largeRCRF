


#' Add Trees
#'
#' Add more trees to an existing forest. Most parameters are extracted from the
#' previous forest.
#'
#' @param forest An existing forest.
#' @param numTreesToAdd The number of trees to add.
#' @param savePath If saving the forest, the directory to save to. Default is
#'   \code{NULL}. Note that you need to re-specify the path if you're modifying
#'   a previously saved forest.
#' @param savePath.overwrite If \code{savePath} is pointing to an existing
#'   directory, possibly containing another forest, this specifies what should
#'   be done.
#' @param cores The number of cores to be used for training the new trees.
#' @param displayProgress A logical indicating whether the progress should be
#'   displayed to console; default is \code{TRUE}. Useful to set to FALSE in
#'   some automated situations.
#'
#' @return A new forest with the original and additional trees.
#' @export
#' 
addTrees <- function(forest, numTreesToAdd, savePath = NULL, savePath.overwrite = c("warn", "delete", "merge"), cores = getCores(), displayProgress = TRUE){
  if(is.null(forest$dataset)){
    stop("Training dataset must be connected to forest before more trees can be added; this can be done manually by using connectToData")
  }
  
  numTreesToAdd <- as.integer(numTreesToAdd)
  
  if(numTreesToAdd <= 0){
    stop("numTreesToAdd must be a positive integer")
  }
  
  if(is.null(savePath.overwrite) | length(savePath.overwrite)==0 | !(savePath.overwrite[1] %in% c("warn", "delete", "merge"))){
    stop("savePath.overwrite must be one of c(\"warn\", \"delete\", \"merge\")")
  }
  
  newTreeCount <- forest$params$ntree + as.integer(numTreesToAdd)
  
  treeTrainer <- createTreeTrainer(responseCombiner=forest$params$nodeResponseCombiner,
                                   splitFinder=forest$params$splitFinder,
                                   covariateList=forest$covariateList,
                                   numberOfSplits=forest$params$numberOfSplits,
                                   nodeSize=forest$params$nodeSize,
                                   maxNodeDepth=forest$params$maxNodeDepth,
                                   mtry=forest$params$mtry,
                                   splitPureNodes=forest$params$splitPureNodes)
  
  forestTrainer <- createForestTrainer(treeTrainer=treeTrainer,
                                       covariateList=forest$covariateList,
                                       treeResponseCombiner=forest$params$forestResponseCombiner,
                                       dataset=forest$dataset,
                                       ntree=forest$params$ntree + numTreesToAdd,
                                       randomSeed=forest$params$randomSeed,
                                       saveTreeLocation=savePath,
                                       displayProgress=displayProgress)
  
  params <- list(
    splitFinder=forest$params$splitFinder,
    nodeResponseCombiner=forest$params$nodeResponseCombiner,
    forestResponseCombiner=forest$params$forestResponseCombiner,
    ntree=forest$params$ntree + numTreesToAdd,
    numberOfSplits=forest$params$numberOfSplits,
    mtry=forest$params$mtry,
    nodeSize=forest$params$nodeSize,
    splitPureNodes=forest$params$splitPureNodes,
    maxNodeDepth = forest$params$maxNodeDepth,
    randomSeed=forest$params$randomSeed
  )
  
  initial.forest.optional <- .object_Optional(forest$javaObject)
  
  # We'll be saving an offline version of the forest
  if(!is.null(savePath)){
    
    if(file.exists(savePath)){ # we might have to remove the folder or display an error
      
      if(savePath.overwrite[1] == "warn"){
        stop(paste(savePath, "already exists; will not modify it. Please remove/rename it or set the savePath.overwrite to either 'delete' or 'merge'"))
      } else if(savePath.overwrite[1] == "delete"){
        unlink(savePath, recursive=TRUE)
      } else if(savePath.overwrite[1] == "merge"){
        warning("Assuming that the previous forest at savePath is the provided forest argument; if not true then your results will be suspect")
        initial.forest.optional <- .object_Optional(NULL) # Java backend requires we be explicit about whether we're providing an in-memory initial forest or starting from a previous directory
      }
      
    }
    
    if(savePath.overwrite[1] != "merge"){
      dir.create(savePath)
    }
    
    # First save forest components (so that if the training crashes mid-way through it can theoretically be recovered by the user)
    saveForestComponents(savePath, 
                         covariateList=forest$covariateList,
                         params=params,
                         forestCall=match.call())
    
    if(cores > 1){
      .jcall(forestTrainer, "V", "trainParallelOnDisk", initial.forest.optional, as.integer(cores))
    } else {
      .jcall(forestTrainer, "V", "trainSerialOnDisk", initial.forest.optional)
    }
    
    # Need to now load forest trees back into memory
    forest.java <- .jcall(.class_DataUtils, makeResponse(.class_Forest), "loadForest", savePath, forest$params$forestResponseCombiner$javaObject)
    
    
  }
  else{ # save directly into memory
    if(cores > 1){
      forest.java <- .jcall(forestTrainer, makeResponse(.class_Forest), "trainParallelInMemory", initial.forest.optional, as.integer(cores))
    } else {
      forest.java <- .jcall(forestTrainer, makeResponse(.class_Forest), "trainSerialInMemory", initial.forest.optional)
    }
  }
  
  
  
  
  forestObject <- list(call=match.call(), params=params, javaObject=forest.java, covariateList=forest$covariateList, dataset=forest$dataset)
  
  class(forestObject) <- "JRandomForest"
  return(forestObject)
  
}



#' Load Random Forest
#'
#' Loads a random forest that was saved using \code{\link{saveForest}}.
#'
#' @param directory The directory created that saved the previous forest.
#' @param forest.output Specifies whether the forest loaded should be loaded
#'   into memory, or reflect the saved files where only one tree is loaded at a
#'   time.
#' @param maxTreeNum If for some reason you only want to load the number of
#'   trees up until a certain point, you can specify maxTreeNum as a single
#'   number.
#' @return A JForest object; see \code{\link{train}} for details.
#' @export
#' @seealso \code{\link{train}}, \code{\link{saveForest}}
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
#' saveForest(forest, "trees")
#' new_forest <- loadForest("trees")
loadForest <- function(directory, forest.output = c("online", "offline"), maxTreeNum = NULL){
  
  if(is.null(forest.output) | length(forest.output)==0 | !(forest.output[1] %in% c("online", "offline"))){
    stop("forest.output must be one of c(\"online\", \"offline\")")
  }
  
  # First load the response combiners and the split finders
  nodeResponseCombiner.java <- .jcall(.class_DataUtils, makeResponse(.class_Object), "loadObject", paste0(directory, "/nodeResponseCombiner.jData"))
  nodeResponseCombiner.java <- .jcast(nodeResponseCombiner.java, .class_ResponseCombiner)
  
  splitFinder.java <- .jcall(.class_DataUtils, makeResponse(.class_Object), "loadObject", paste0(directory, "/splitFinder.jData"))
  splitFinder.java <- .jcast(splitFinder.java, .class_SplitFinder)
  
  forestResponseCombiner.java <- .jcall(.class_DataUtils, makeResponse(.class_Object), "loadObject", paste0(directory, "/forestResponseCombiner.jData"))
  forestResponseCombiner.java <- .jcast(forestResponseCombiner.java, .class_ForestResponseCombiner)
  
  covariateList <- .jcall(.class_DataUtils, makeResponse(.class_Object), "loadObject", paste0(directory, "/covariateList.jData"))
  covariateList <- .jcast(covariateList, .class_List)
  
  params <- readRDS(paste0(directory, "/parameters.rData"))
  call <- readRDS(paste0(directory, "/call.rData"))
  
  params$nodeResponseCombiner$javaObject <- nodeResponseCombiner.java
  params$splitFinder$javaObject <- splitFinder.java
  params$forestResponseCombiner$javaObject <- forestResponseCombiner.java
  
  forest <- loadForestArgumentsSpecified(directory, params$nodeResponseCombiner, params$splitFinder, 
                                         params$forestResponseCombiner, covariateList, call,
                                         params$ntree, params$numberOfSplits, params$mtry,
                                         params$nodeSize, params$maxNodeDepth, params$splitPureNodes,
                                         params$randomSeed, forest.output, maxTreeNum)
  
  return(forest)
  
}

# Internal function - if you really need to use it yourself (say to load forests
# saved directly through the Java interface into R), then look at the loadForest
# function to see how this function is used. I'm also open to writing a function
# that uses the Java version's settings yaml file to recreate the forest, but
# I'd appreciate knowing that someone's going to use it first (email me; see
# README).
loadForestArgumentsSpecified <- function(treeDirectory, nodeResponseCombiner, splitFinder,
                                         forestResponseCombiner, covariateList.java, call,
                                         ntree, numberOfSplits, mtry, nodeSize,
                                         maxNodeDepth = 100000, splitPureNodes=TRUE,
                                         randomSeed=NULL, forest.output = "online", maxTreeNum = NULL){
  
  params <- list(
    splitFinder=splitFinder,
    nodeResponseCombiner=nodeResponseCombiner,
    forestResponseCombiner=forestResponseCombiner,
    ntree=ntree,
    numberOfSplits=numberOfSplits,
    mtry=mtry,
    nodeSize=nodeSize,
    splitPureNodes=splitPureNodes,
    maxNodeDepth=maxNodeDepth,
    randomSeed=randomSeed
  )
  
  forest.java <- NULL
  if(forest.output[1] == "online"){
    castedForestResponseCombiner <- .jcast(forestResponseCombiner$javaObject, .class_ResponseCombiner) # OnlineForest constructor takes a ResponseCombiner
    
    if(is.null(maxTreeNum)){
      forest.java <- .jcall(.class_DataUtils, makeResponse(.class_OnlineForest), "loadOnlineForest",
                            treeDirectory, castedForestResponseCombiner)
    } else{
      tree.file.array <- .jcall(.class_RUtils, paste0("[", makeResponse(.class_File)), "getTreeFileArray", 
                                treeDirectory, as.integer(maxTreeNum), evalArray = FALSE)
      forest.java <- .jcall(.class_DataUtils, makeResponse(.class_OnlineForest), "loadOnlineForest",
                            tree.file.array, castedForestResponseCombiner)
      
    }
    
  } else{ # offline forest
    if(is.null(maxTreeNum)){
      path.as.file <- .jnew(.class_File, treeDirectory)
      forest.java <- .jnew(.class_OfflineForest, path.as.file, forestResponseCombiner$javaObject)
    } else{
      tree.file.array <- .jcall(.class_RUtils, paste0("[", makeResponse(.class_File)), "getTreeFileArray", 
                                treeDirectory, as.integer(maxTreeNum), evalArray = FALSE)
      forest.java <- .jnew(.class_OfflineForest, tree.file.array, forestResponseCombiner$javaObject)
    }
  }
  
  
  
  forestObject <- list(call=call, javaObject=forest.java, covariateList=covariateList.java, params=params)
  class(forestObject) <- "JRandomForest"
  
  return(forestObject)
  
}

# Internal function to calculate how many CPU cores are available.
getCores <- function(){
  cores <- NA
  if (requireNamespace("parallel", quietly = TRUE)){
    cores <- parallel::detectCores()
  }

  if (is.na(cores)){
    message("Unable to detect how many cores are available; defaulting to only using one. Feel free to override this by pre-specifying the cores argument.")
    cores <- 1
  }

  return(cores)
}

train.internal <- function(dataset, splitFinder, 
                           nodeResponseCombiner, forestResponseCombiner, ntree, 
                           numberOfSplits, mtry, nodeSize, maxNodeDepth, 
                           splitPureNodes, savePath, savePath.overwrite, 
                           forest.output, cores, randomSeed, displayProgress){
  
  # Some quick checks on parameters
  ntree <- as.integer(ntree)
  numberOfSplits <- as.integer(numberOfSplits)
  mtry <- as.integer(mtry)
  nodeSize <- as.integer(nodeSize)
  maxNodeDepth <- as.integer(maxNodeDepth)
  cores <- as.integer(cores)

  if (ntree <= 0){
    stop("ntree must be strictly positive.")
  }
  if (numberOfSplits < 0){
    stop("numberOfSplits cannot be negative.")
  }
  if (mtry <= 0){
    stop("mtry must be strictly positive. If you want to try all covariates, you can set it to be very large.")
  }
  if (nodeSize <= 0){
    stop("nodeSize must be strictly positive.")
  }
  if (maxNodeDepth <= 0){
    stop("maxNodeDepth must be strictly positive")
  }
  if (cores <= 0){
    stop("cores must be strictly positive")
  }
  
  if(is.null(savePath.overwrite) | length(savePath.overwrite)==0 | !(savePath.overwrite[1] %in% c("warn", "delete", "merge"))){
    stop("savePath.overwrite must be one of c(\"warn\", \"delete\", \"merge\")")
  }
  
  if(is.null(forest.output) | length(forest.output)==0 | !(forest.output[1] %in% c("online", "offline"))){
    stop("forest.output must be one of c(\"online\", \"offline\")")
  }
  
  if(is.null(splitFinder)){
    splitFinder <- splitFinderDefault(dataset$responses)
  }
  
  if(is.null(nodeResponseCombiner)){
    nodeResponseCombiner <- nodeResponseCombinerDefault(dataset$responses)
  }
  
  if(is.null(forestResponseCombiner)){
    forestResponseCombiner <- forestResponseCombinerDefault(dataset$responses)
  }
  


  if(class(nodeResponseCombiner) != "ResponseCombiner"){
    stop("nodeResponseCombiner must be a ResponseCombiner")
  }
  if(class(splitFinder) != "SplitFinder"){
    stop("splitFinder must be a SplitFinder")
  }
  if(class(forestResponseCombiner) != "ResponseCombiner"){
    stop("forestResponseCombiner must be a ResponseCombiner")
  }
  
  treeTrainer <- createTreeTrainer(responseCombiner=nodeResponseCombiner,
                                   splitFinder=splitFinder,
                                   covariateList=dataset$covariateList,
                                   numberOfSplits=numberOfSplits,
                                   nodeSize=nodeSize,
                                   maxNodeDepth=maxNodeDepth,
                                   mtry=mtry,
                                   splitPureNodes=splitPureNodes)

  forestTrainer <- createForestTrainer(treeTrainer=treeTrainer,
                                       covariateList=dataset$covariateList,
                                       treeResponseCombiner=forestResponseCombiner,
                                       dataset=dataset$dataset,
                                       ntree=ntree,
                                       randomSeed=randomSeed,
                                       saveTreeLocation=savePath,
                                       displayProgress=displayProgress)
  
  params <- list(
    splitFinder=splitFinder,
    nodeResponseCombiner=nodeResponseCombiner,
    forestResponseCombiner=forestResponseCombiner,
    ntree=ntree,
    numberOfSplits=numberOfSplits,
    mtry=mtry,
    nodeSize=nodeSize,
    splitPureNodes=splitPureNodes,
    maxNodeDepth = maxNodeDepth,
    randomSeed=randomSeed
  )
  
  # We'll be saving an offline version of the forest
  if(!is.null(savePath)){
    
    if(file.exists(savePath)){ # we might have to remove the folder or display an error
      
      if(savePath.overwrite[1] == "warn"){
        stop(paste(savePath, "already exists; will not modify it. Please remove/rename it or set the savePath.overwrite to either 'delete' or 'merge'"))
      } else if(savePath.overwrite[1] == "delete"){
        unlink(savePath)
      }
      
    }

    if(savePath.overwrite[1] != "merge"){
      dir.create(savePath)
    }
    
    # First save forest components (so that if the training crashes mid-way through it can theoretically be recovered by the user)
    saveForestComponents(savePath, 
                         covariateList=dataset$covariateList,
                         params=params,
                         forestCall=match.call())
    
    forest.java <- NULL
    if(cores > 1){
      forest.java <- .jcall(forestTrainer, makeResponse(.class_OfflineForest), "trainParallelOnDisk", .object_Optional(), as.integer(cores))
    } else {
      forest.java <- .jcall(forestTrainer, makeResponse(.class_OfflineForest), "trainSerialOnDisk", .object_Optional())
    }
    
    if(forest.output[1] == "online"){
      forest.java <- convertToOnlineForest.Java(forest.java)
    }

  }
  else{ # save directly into memory
    if(cores > 1){
      forest.java <- .jcall(forestTrainer, makeResponse(.class_OnlineForest), "trainParallelInMemory", .object_Optional(), as.integer(cores))
    } else {
      forest.java <- .jcall(forestTrainer, makeResponse(.class_OnlineForest), "trainSerialInMemory", .object_Optional())
    }
  }
  

  forestObject <- list(params=params, javaObject=forest.java,
                       covariateList=dataset$covariateList, dataset=dataset$dataset)

  class(forestObject) <- "JRandomForest"
  return(forestObject)

}


#' Train Random Forests
#'
#' Trains the random forest. The type of response the random forest can be
#' trained on varies depending on the \code{splitFinder},
#' \code{nodeResponseCombiner}, and the \code{forestResponseCombiner}
#' parameters. Make sure these are compatible with each other, and with the
#' response you plug in. \code{splitFinder} should work on the responses you are
#' providing; \code{nodeResponseCombiner} should combine these responses into
#' some intermediate product, and \code{forestResponseCombiner} combines these
#' intermediate products into the final output product. Note that
#' \code{nodeResponseCombiner} and \code{forestResponseCombiner} can be inferred
#' from the data (so feel free to not specify them), and \code{splitFinder} can
#' be inferred but you might want to change its default.
#'
#' @param formula You may specify the response and covariates as a formula
#'   instead; make sure the response in the formula is still properly
#'   constructed.
#' @param data A data.frame containing the columns of the predictors and
#'   responses.
#' @param splitFinder A split finder that's used to score splits in the random
#'   forest training algorithm. See \code{\link{CompetingRiskSplitFinders}} or
#'   \code{\link{WeightedVarianceSplitFinder}}. If you don't specify one, this
#'   function tries to pick one based on the response. For
#'   \code{\link{CR_Response}} without censor times, it will pick a
#'   \code{\link{LogRankSplitFinder}}; while if censor times were provided it
#'   will pick \code{\link{GrayLogRankSplitFinder}}; for integer or numeric
#'   responses it picks a \code{\link{WeightedVarianceSplitFinder}}.
#' @param nodeResponseCombiner A response combiner that's used to combine
#'   responses for each terminal node in a tree (regression example; average the
#'   observations in each tree into a single number). See
#'   \code{\link{CR_ResponseCombiner}} or \code{\link{MeanResponseCombiner}}. If
#'   you don't specify one, this function tries to pick one based on the
#'   response. For \code{\link{CR_Response}} it picks a
#'   \code{\link{CR_ResponseCombiner}}; for integer or numeric responses it
#'   picks a \code{\link{MeanResponseCombiner}}.
#' @param forestResponseCombiner A response combiner that's used to combine
#'   predictions across trees into one final result (regression example; average
#'   the prediction of each tree into a single number). See
#'   \code{\link{CR_FunctionCombiner}} or \code{\link{MeanResponseCombiner}}. If
#'   you don't specify one, this function tries to pick one based on the
#'   response. For \code{\link{CR_Response}} it picks a
#'   \code{\link{CR_FunctionCombiner}}; for integer or numeric responses it
#'   picks a \code{\link{MeanResponseCombiner}}.
#' @param ntree An integer that specifies how many trees should be trained.
#' @param numberOfSplits A tuning parameter specifying how many random splits
#'   should be tried for a covariate; a value of 0 means all splits will be
#'   tried (with an exception for factors, who might have too many splits to
#'   feasibly compute).
#' @param mtry A tuning parameter specifying how many covariates will be
#'   randomly chosen to be tried in the splitting process. This value must be at
#'   least 1.
#' @param nodeSize The algorithm will not attempt to split a node that has
#'   observations less than 2*\code{nodeSize}; this guarantees that any two
#'   sibling terminal nodes together have an average size of at least
#'   \code{nodeSize}; note that it doesn't guarantee that every node is at least
#'   as large as \code{nodeSize}.
#' @param maxNodeDepth This parameter is analogous to \code{nodeSize} in that it
#'   controls tree length; by default \code{maxNodeDepth} is an extremely high
#'   number and tree depth is controlled by \code{nodeSize}.
#' @param na.penalty This parameter controls whether predictor variables with
#'   NAs should be penalized when being considered for a best split. Best splits
#'   (and the associated score) are determined on only non-NA data; the penalty
#'   is to take the best split identified, and to randomly assign any NAs
#'   (according to the proportion of data split left and right), and then
#'   recalculate the corresponding split score, when is then compared with the
#'   other split candiate variables. This penalty adds some computational time,
#'   so it may be disabled for some variables. \code{na.penalty} may be
#'   specified as a vector of logicals indicating, for each predictor variable,
#'   whether the penalty should be applied to that variable. If it's length 1
#'   then it applies to all variables. Alternatively, a single numeric value may
#'   be provided to indicate a threshold whereby the penalty is activated only
#'   if the proportion of NAs for that variable in the training set exceeds that
#'   threshold.
#' @param splitPureNodes This parameter determines whether the algorithm will
#'   split a pure node. If set to FALSE, then before every split it will check
#'   that every response is the same, and if so, not split. If set to TRUE it
#'   forgoes that check and splits it. Prediction accuracy won't change under
#'   any sensible \code{nodeResponseCombiner}; as all terminal nodes from a
#'   split pure node should give the same prediction, so this parameter only
#'   affects performance. If your response is continuous you'll likely
#'   experience faster train times by setting it to TRUE. Default value is TRUE.
#' @param savePath If set, this parameter will save each tree of the random
#'   forest in this directory as the forest is trained. Use this parameter if
#'   you need to save memory while training. See also \code{\link{loadForest}}
#' @param savePath.overwrite This parameter controls the behaviour for what
#'   happens if \code{savePath} is pointing to an existing directory. If set to
#'   \code{warn} (default) then \code{train} refuses to proceed. If set to
#'   \code{delete} then all the contents in that folder are deleted for the new
#'   forest to be trained. Note that all contents are deleted, even those files
#'   not related to \code{largeRCRF}. Use only if you're sure it's safe. If set
#'   to \code{merge}, then the files describing the forest (such as its
#'   parameters) are overwritten but the saved trees are not. The algorithm
#'   assumes (without checking) that the existing trees are from a previous run
#'   and starts from where it left off. This option is useful if recovering from
#'   a crash.
#' @param forest.output This parameter only applies if \code{savePath} has been
#'   set; set to 'online' (default) and the saved forest will be loaded into
#'   memory after being trained. Set to 'offline' and the forest is not saved
#'   into memory, but can still be used in a memory unintensive manner.
#' @param cores This parameter specifies how many trees will be simultaneously
#'   trained. By default the package attempts to detect how many cores you have
#'   by using the \code{parallel} package and using all of them. You may specify
#'   a lower number if you wish. It is not recommended to specify a number
#'   greater than the number of available cores as this will hurt performance
#'   with no available benefit.
#' @param randomSeed This parameter specifies a random seed if reproducible,
#'   deterministic forests are desired.
#' @param displayProgress A logical indicating whether the progress should be
#'   displayed to console; default is \code{TRUE}. Useful to set to FALSE in
#'   some automated situations.
#' @export
#' @return A \code{JRandomForest} object. You may call \code{predict} or
#'   \code{print} on it.
#' @seealso \code{\link{predict.JRandomForest}}
#' @note If saving memory is a concern, you can replace \code{covariateData} or
#'   \code{data} with an environment containing one element called \code{data}
#'   as the actual dataset. After the data has been imported into Java, but
#'   before the forest training begins, the dataset in the environment is
#'   deleted, freeing up memory in R.
#' @examples
#' # Regression Example
#' x1 <- rnorm(1000)
#' x2 <- rnorm(1000)
#' y <- 1 + x1 + x2 + rnorm(1000)
#'
#' data <- data.frame(x1, x2, y)
#' forest <- train(y ~ x1 + x2, data, WeightedVarianceSplitFinder(),
#'   MeanResponseCombiner(), MeanResponseCombiner(), ntree=100,
#'   numberOfSplits = 5, mtry = 1, nodeSize = 5)
#'
#' # Fix x2 to be 0
#' newData <- data.frame(x1 = seq(from=-2, to=2, by=0.5), x2 = 0)
#' ypred <- predict(forest, newData)
#'
#' plot(ypred ~ newData$x1, type="l")
#'
#' # Competing Risk Example
#' x1 <- abs(rnorm(1000))
#' x2 <- abs(rnorm(1000))
#'
#' T1 <- rexp(1000, rate=x1)
#' T2 <- rweibull(1000, shape=x1, scale=x2)
#' C <- rexp(1000)
#' u <- pmin(T1, T2, C)
#' delta <- ifelse(u==T1, 1, ifelse(u==T2, 2, 0))
#'
#' data <- data.frame(x1, x2)
#'
#' forest <- train(CR_Response(delta, u) ~ x1 + x2, data,
#'    LogRankSplitFinder(1:2), CR_ResponseCombiner(1:2),
#'    CR_FunctionCombiner(1:2), ntree=100, numberOfSplits=5,
#'    mtry=1, nodeSize=10)
#' newData <- data.frame(x1 = c(-1, 0, 1), x2 = 0)
#' ypred <- predict(forest, newData)
train <- function(formula, data, splitFinder = NULL, nodeResponseCombiner = NULL,
                  forestResponseCombiner = NULL, ntree, numberOfSplits, mtry,
                  nodeSize, maxNodeDepth = 100000, na.penalty = TRUE, splitPureNodes=TRUE, 
                  savePath = NULL, savePath.overwrite = c("warn", "delete", "merge"), 
                  forest.output = c("online", "offline"),
                  cores = getCores(), randomSeed = NULL, displayProgress = TRUE){
  
  dataset <- processFormula(formula, data, na.penalty = na.penalty)
  
  forest <- train.internal(dataset, splitFinder = splitFinder,
                           nodeResponseCombiner = nodeResponseCombiner,
                           forestResponseCombiner = forestResponseCombiner,
                           ntree = ntree, numberOfSplits = numberOfSplits,
                           mtry = mtry, nodeSize = nodeSize, maxNodeDepth = maxNodeDepth,
                           splitPureNodes = splitPureNodes, savePath = savePath,
                           savePath.overwrite = savePath.overwrite, forest.output = forest.output,
                           cores = cores, randomSeed = randomSeed, displayProgress = displayProgress)
  
  forest$call <- match.call()
  forest$formula <- formula

  return(forest)
}

createForestTrainer <- function(treeTrainer, 
                                covariateList, 
                                treeResponseCombiner, 
                                dataset,
                                ntree, 
                                randomSeed, 
                                saveTreeLocation,
                                displayProgress){
  builderClassReturned <- makeResponse(.class_ForestTrainer_Builder)

  builder <- .jcall(.class_ForestTrainer, builderClassReturned, "builder")

  builder <- .jcall(builder, builderClassReturned, "treeTrainer", treeTrainer)
  builder <- .jcall(builder, builderClassReturned, "covariates", covariateList)
  builder <- .jcall(builder, builderClassReturned, "treeResponseCombiner", treeResponseCombiner$javaObject)
  builder <- .jcall(builder, builderClassReturned, "data", dataset)
  builder <- .jcall(builder, builderClassReturned, "ntree", as.integer(ntree))
  builder <- .jcall(builder, builderClassReturned, "displayProgress", displayProgress)
  
  if(!is.null(randomSeed)){
    builder <- .jcall(builder, builderClassReturned, "randomSeed", .jlong(randomSeed))
  }
  else{
    builder <- .jcall(builder, builderClassReturned, "randomSeed", .jlong(as.integer(Sys.time())))
  }
  
  if(!is.null(saveTreeLocation)){
    builder <- .jcall(builder, builderClassReturned, "saveTreeLocation", saveTreeLocation)
  }
  

  forestTrainer <- .jcall(builder, makeResponse(.class_ForestTrainer), "build")
  return(forestTrainer)
}

createTreeTrainer <- function(responseCombiner, splitFinder, covariateList, numberOfSplits, nodeSize, maxNodeDepth, mtry, splitPureNodes){
  builderClassReturned <- makeResponse(.class_TreeTrainer_Builder)

  builder <- .jcall(.class_TreeTrainer, builderClassReturned, "builder")
  
  responseCombinerCasted <- .jcast(responseCombiner$javaObject, .class_ResponseCombiner) # might need to cast a ForestResponseCombiner down
  
  builder <- .jcall(builder, builderClassReturned, "responseCombiner", responseCombinerCasted)
  builder <- .jcall(builder, builderClassReturned, "splitFinder", splitFinder$javaObject)
  builder <- .jcall(builder, builderClassReturned, "covariates", covariateList)
  builder <- .jcall(builder, builderClassReturned, "numberOfSplits", as.integer(numberOfSplits))
  builder <- .jcall(builder, builderClassReturned, "nodeSize", as.integer(nodeSize))
  builder <- .jcall(builder, builderClassReturned, "maxNodeDepth", as.integer(maxNodeDepth))
  builder <- .jcall(builder, builderClassReturned, "mtry", as.integer(mtry))
  builder <- .jcall(builder, builderClassReturned, "checkNodePurity", !splitPureNodes)

  treeTrainer <- .jcall(builder, makeResponse(.class_TreeTrainer), "build")
  return(treeTrainer)
}


recover_forest_predictable <- function(tree_directory, settingsPath) {
  
  settings.java <- load_settings(settingsPath)
  
  nodeResponseCombiner.java <- .jcall(settings.java, makeResponse(.class_ResponseCombiner), "getResponseCombiner")
  splitFinder.java <- .jcall(settings.java, makeResponse(.class_SplitFinder), "getSplitFinder")
  forestResponseCombiner.java <- .jcall(settings.java, makeResponse(.class_ResponseCombiner), "getTreeCombiner")
  
  covariateList <- .jcall(settings.java, makeResponse(.class_List), "getCovariates")
  
  params <- readRDS(paste0(directory, "/parameters.rData"))
  call <- readRDS(paste0(directory, "/call.rData"))
  
  params$nodeResponseCombiner$javaObject <- nodeResponseCombiner.java
  params$splitFinder$javaObject <- splitFinder.java
  params$forestResponseCombiner$javaObject <- forestResponseCombiner.java
  
  forest <- load_forest_args_provided(directory, params$nodeResponseCombiner, params$splitFinder, params$forestResponseCombiner, covariateList, params, call)
  
  return(forest)
  
}

load_settings <- function(settingsPath) {
  settingsFile <- .jnew(.class_File, settingsPath)
  settings.java <- .jcall(.class_Settings, makeResponse(.class_Settings), "load", settingsFile)
  
  return(settings.java)
}

#' @export
load_covariate_list_from_settings <- function(settingsPath){
  settings.java = load_settings(settingsPath)
  covariateList <- .jcall(settings.java, makeResponse(.class_List), "getCovariates")
  return(covariateList)
}
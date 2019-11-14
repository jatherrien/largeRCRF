
#' Convert to Online Forest
#'
#' Some forests are too large to store in memory and have been saved to disk.
#' They can still be used, but their performance is much slower. If there's
#' enough memory, they can easily be converted into an in-memory forest that is
#' faster to use.
#'
#' @param forest The offline forest.
#'
#' @return An online, in memory forst.
#' @export
#' 
convertToOnlineForest <- function(forest){
  old.forest.object <- forest$javaObject
  
  if(getJavaClass(old.forest.object) == "ca.joeltherrien.randomforest.tree.OnlineForest"){
    
    warning("forest is already in-memory")
    return(forest)
    
  } else if(getJavaClass(old.forest.object) == "ca.joeltherrien.randomforest.tree.OfflineForest"){
    
    forest$javaObject <- convertToOnlineForest.Java(old.forest.object)
    return(forest)
    
  } else{
    stop("'forest' is not an online or offline forest")
  }
  
}

# Internal function
convertToOnlineForest.Java <- function(forest.java){
  offline.forest <- .jcast(forest.java, .class_OfflineForest)
  online.forest <- .jcall(offline.forest, makeResponse(.class_OnlineForest), "createOnlineCopy")
  return(online.forest)
}
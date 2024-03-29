
#' Naive Concordance
#'
#' Used to calculate a concordance index error. The user needs to supply a list
#' of mortalities, with each item in the list being a vector for the
#' corresponding event. To calculate mortalities a user should look to
#' \code{\link{extractMortalities}}.
#'
#' @return A vector of 1 minus the concordance scores, with each element
#'   corresponding to one of the events. To be clear, the lower the score the
#'   more accurate the model was.
#'
#' @param responses A list of responses corresponding to the provided
#'   mortalities; use \code{\link{CR_Response}}.
#' @param predictedMortalities A list of mortality vectors; each element of the
#'   list should correspond to one of the events in the order of event 1 to J,
#'   and should be a vector of the same length as responses.
#' @export
#' @references Section 3.2 of Wolbers, Marcel, Paul Blanche, Michael T. Koller,
#'   Jacqueline C M Witteman, and Thomas A Gerds. 2014. “Concordance for
#'   Prognostic Models with Competing Risks.” Biostatistics 15 (3): 526–39.
#'   https://doi.org/10.1093/biostatistics/kxt059.
#'
#' @examples
#' data <- data.frame(delta=c(1,1,0,0,2,2), T=1:6, x=1:6)
#'
#' model <- train(CR_Response(delta, T) ~ x, data, ntree=100, numberOfSplits=0, mtry=1, nodeSize=1)
#'
#' newData <- data.frame(delta=c(1,0,2,1,0,2), T=1:6, x=1:6)
#' predictions <- predict(model, newData)
#'
#' mortalities <- list(
#'   extractMortalities(predictions, 1, 6),
#'   extractMortalities(predictions, 2, 6)
#' )
#'
#' naiveConcordance(CR_Response(newData$delta, newData$T), mortalities)
#' 
naiveConcordance <- function(responses, predictedMortalities){
  if(is.null(responses)){
    stop("responses cannot be null")
  }
  
  if(is.null(predictedMortalities)){
    stop("predictedMortalities cannot be null")
  }
  if(!is.list(predictedMortalities)){
    stop("predictedMortalities must be a list")
  }
  
  responseList = responses$javaObject
  responseLength = .jcall(responseList, "I", "size")
  
  events = as.integer(1:length(predictedMortalities))
  
  concordances = numeric(length(predictedMortalities))

  for(event in events){
    if(length(predictedMortalities[[event]]) != responseLength){
      stop("Every mortality vector in predictedMortalities must be the same length as responses")
    }
    
    # Need to turn predictedMortalities into an array of doubles
    mortality = .jarray(predictedMortalities[[event]], "D")

    concordances[event] = 1 - .jcall(.class_CompetingRiskUtils, "D", "calculateConcordance", responseList, mortality, event)

  }

  return(concordances)

}

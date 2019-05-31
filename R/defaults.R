splitFinderDefault <- function(responses){
  if(class(responses) == "CompetingRiskResponses"){
    # get all of the events
    deltas <- unique(sort(responses$eventIndicator))
    deltas <- deltas[!(deltas %in% as.integer(0))]

    return(LogRankSplitFinder(deltas))
  } else if(class(responses) == "CompetingRiskResponsesWithCensorTimes"){
    # get all of the events
    deltas <- sort(unique(responses$eventIndicator))
    deltas <- deltas[!(deltas %in% as.integer(0))]

    return(GrayLogRankSplitFinder(deltas))
  }
  else if(class(responses) == "numeric" | class(responses) == "integer" | class(responses) == "JNumeric"){
    return(WeightedVarianceSplitFinder())
  }
  else{
    stop("Unable to determine an appropriate split finder for this response; please specify one manually.")
  }
}


nodeResponseCombinerDefault <- function(responses){
  if(class(responses) == "CompetingRiskResponses" | class(responses) == "CompetingRiskResponsesWithCensorTimes"){
    # get all of the events
    deltas <- unique(sort(responses$eventIndicator))
    deltas <- deltas[!(deltas %in% as.integer(0))]

    return(CR_ResponseCombiner(deltas))
  } else if(class(responses) == "numeric" | class(responses) == "integer" | class(responses) == "JNumeric"){
    return(MeanResponseCombiner())
  }
  else{
    stop("Unable to determine an appropriate node response combiner for this response; please specify one manually")
  }
}

forestResponseCombinerDefault <- function(responses){
  if(class(responses) == "CompetingRiskResponses" | class(responses) == "CompetingRiskResponsesWithCensorTimes"){
    # get all of the events
    deltas <- unique(sort(responses$eventIndicator))
    deltas <- deltas[!(deltas %in% as.integer(0))]

    return(CR_FunctionCombiner(deltas))
  } else if(class(responses) == "numeric" | class(responses) == "integer" | class(responses) == "JNumeric"){
    return(MeanResponseCombiner())
  }
  else{
    stop("Unable to determine an appropriate forest response combiner for this response; please specify one manually.")
  }
}

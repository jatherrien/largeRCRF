
#' Competing Risk Response
#'
#' Takes vectors of event time and event type and turns it into the internal
#' objects used throughout the package. The result of this function shouldn't be
#' used directly, but should instead by provided as the \code{y} parameter in
#' \code{\link{train}}.
#'
#' @param delta A vector of integers detailing the event that occurred. A value
#'   of 0 denotes that censoring occurred first and that time was recorded.
#' @param u A vector of numerics detailing the recorded event times (possibly
#'   censored).
#' @param C If the censoring times are known for all observations, they can be
#'   included which allows for \code{\link{GrayLogRankSplitFinder}} to be used.
#'   Default is \code{NULL}.
#'
#' @details To be clear, if T1,...TJ are the J different competing risks, and C
#'   is the censoring time, then \code{u[i] = min(T1[i], ...TJ[i], C[i])}; and
#'   \code{delta[i]} denotes which time was the minimum, with a value of 0 if
#'   C[i] was the smallest.
#' @export
#' @examples
#' T1 <- rexp(10)
#' T2 <- rweibull(10, 2, 2)
#' C <- rexp(10)
#'
#' u <- pmin(T1, T2, C)
#' delta <- ifelse(u == T1, 1, ifelse(u == T2, 2, 0))
#'
#' responses <- CR_Response(delta, u)
#' # Then use responses in train or naiveConcordance
CR_Response <- function(delta, u, C = NULL){
  if(is.null(C)){
    return(Java_CompetingRiskResponses(delta, u))
  } else{
    return(Java_CompetingRiskResponsesWithCensorTimes(delta, u, C))
  }
}


# Internal function
Java_CompetingRiskResponses <- function(delta, u){
  
  if(length(delta) != length(u)){
    stop("delta and u must be of the same length")
  }
  
  if(anyNA(delta) | is.null(delta)){
    stop("delta must be specified")
  }
  
  if(anyNA(u) | is.null(u)){
    stop("u must be specified")
  }
  
  delta <- as.integer(delta)
  u <- as.double(u)
  
  delta.java <- .jarray(delta, contents.class="I")
  u.java <- .jarray(u, contents.class="D")
  
  responses.java.list <- .jcall(.class_RUtils, makeResponse(.class_List),
                                "importCompetingRiskResponses", delta.java, u.java)
  
  responses <- list(javaObject=responses.java.list, eventIndicator=delta, eventTime=u)
  class(responses) <- "CompetingRiskResponses"
  
  return(responses)
}

# Internal function
Java_CompetingRiskResponsesWithCensorTimes <- function(delta, u, C){
  
  if(length(delta) != length(u) | length(u) != length(C)){
    stop("delta, u, and C must be of the same length")
  }
  
  if(anyNA(delta) | is.null(delta)){
    stop("delta must be specified")
  }
  
  if(anyNA(u) | is.null(u)){
    stop("u must be specified")
  }
  
  if(anyNA(C) | is.null(C)){
    stop("C must be specified")
  }
  
  delta <- as.integer(delta)
  u <- as.double(u)
  C <- as.double(C)
  
  delta.java <- .jarray(delta, contents.class="I")
  u.java <- .jarray(u, contents.class="D")
  C.java <- .jarray(C, contents.class="D")
  
  responses.java.list <- .jcall(.class_RUtils, makeResponse(.class_List),
                                "importCompetingRiskResponsesWithCensorTimes", delta.java, u.java, C.java)
  
  responses <- list(javaObject=responses.java.list, eventIndicator=delta, eventTime=u, censorTime=C)
  class(responses) <- "CompetingRiskResponsesWithCensorTimes"
  
  return(responses)
}


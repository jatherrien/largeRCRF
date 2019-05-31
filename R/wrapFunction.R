wrapFunction <- function(mf){
  f <- function(x){

    y <- vector(mode="numeric", length=length(x))
    for(i in 1:length(x)){
      y[i] <- .jcall(mf, "D", "evaluate", x[i])
    }

    return(y)

  }
}

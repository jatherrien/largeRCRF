# Internal function
createRightContinuousStepFunction <- function(x, y, defaultY){
  x.java <- .jarray(as.numeric(x))
  y.java <- .jarray(as.numeric(y))
  
  # as.numeric is explicitly required in case integers were accidently passed
  # in.
  newFun <- .jnew(.class_RightContinuousStepFunction, as.numeric(x), as.numeric(y), as.numeric(defaultY))
  return(newFun)
  
}
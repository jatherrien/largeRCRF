.onLoad <- function(libname, pkgname) {
  # rJava needs to be initialized with the path to the class files
  .jpackage(pkgname, lib.loc=libname, morePaths = "inst/java/")
}

#' @import rJava
NULL

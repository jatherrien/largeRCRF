.onLoad <- function(libname, pkgname) {
  # rJava expects a folder called 'java' to be present in the root of the
  # package directory; but this isn't true when devtools::test() is run (instead
  # it's in the inst folder which won't be present if the package was actually
  # loaded from the R library). Thus with morePaths we make sure it loads up the
  # jar in this situation.
  .jpackage(pkgname, lib.loc=libname, morePaths="inst/java/largeRCRF-library-1.0-SNAPSHOT.jar")
  
}

#' @import rJava
NULL

# These functions are not exported, so I won't provide their documentation either.
# I.e. it's not a mistake that the documentation below lacks the " ' " on each line.

# Covariates
#
# Creates a covariate for use in the Java code. These functions don't need to
# be directly run by a user, as loadData and train will detect, create and use
# such covariate objects.
#
# @name covariates
#
# @param name The name of the covariate, that later values will be placed
#   under.
# @return An internal rJava object for later internal use.
# @keywords internal
# @examples
# # This is unnecessary for a user to do
#
# # Create a covariate
# booleanCovariate <- Java_BooleanCovariate("x1")
# factorCovariate <- Java_FactorCovariate("x2", c("cat", "dog", "mouse"))
# numericCovariate <- Java_NumericCovariate("x3")
#
# # Call the approriate Java method
# # The Java createValue method always takes in a String
# value1 <- .jcall(booleanCovariate, "Lca/joeltherrien/randomforest/covariates/Covariate$Value;", "createValue", "true")
# value2 <- .jcall(factorCovariate, "Lca/joeltherrien/randomforest/covariates/Covariate$Value;", "createValue", "dog")
# value3 <- .jcall(numericCovariate, "Lca/joeltherrien/randomforest/covariates/Covariate$Value;", "createValue", "3.14")
NULL

# @rdname covariates
Java_BooleanCovariate <- function(name, index){
    covariate <- .jnew(.class_BooleanCovariate, name, as.integer(index))
    covariate <- .jcast(covariate, .class_Object) # needed for later adding it into Java Lists

    return(covariate)
}

# @rdname covariates
# @param levels The levels of the factor as a character vector
Java_FactorCovariate <- function(name, index, levels){
    levelsArray <- .jarray(levels, makeResponse(.class_String))
    levelsList <- .jcall("java/util/Arrays", "Ljava/util/List;", "asList", .jcast(levelsArray, "[Ljava/lang/Object;"))

    covariate <- .jnew(.class_FactorCovariate, name, as.integer(index), levelsList)
    covariate <- .jcast(covariate, .class_Object) # needed for later adding it into Java Lists

    return(covariate)
}

# @rdname covariates
Java_NumericCovariate <- function(name, index){
    covariate <- .jnew(.class_NumericCovariate, name, as.integer(index))
    covariate <- .jcast(covariate, .class_Object) # needed for later adding it into Java Lists

    return(covariate)
}

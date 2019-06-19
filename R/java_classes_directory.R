# This file keeps track of the different Java classes used. Whenever refactoring
# happens in the Java code, this file should be updated and (hopefully) nothing
# will break.

# General Java objects
.class_Object <- "java/lang/Object"
.class_String <- "java/lang/String"
.class_List <- "java/util/List"
.class_ArrayList <- "java/util/ArrayList"
.class_Collection <- "java/util/Collection"
.class_Serializable <- "java/io/Serializable"
.class_File <- "java/io/File"

# Utility Classes
.class_DataUtils <- "ca/joeltherrien/randomforest/utils/DataUtils"
.class_RUtils <- "ca/joeltherrien/randomforest/utils/RUtils"
.class_CompetingRiskUtils <- "ca/joeltherrien/randomforest/responses/competingrisk/CompetingRiskUtils"
.class_Settings <- "ca/joeltherrien/randomforest/Settings"

# Misc. Classes
.class_RightContinuousStepFunction <- "ca/joeltherrien/randomforest/utils/RightContinuousStepFunction"

 # TreeTrainer & its Builder
.class_TreeTrainer <- "ca/joeltherrien/randomforest/tree/TreeTrainer"
.class_TreeTrainer_Builder <- "ca/joeltherrien/randomforest/tree/TreeTrainer$TreeTrainerBuilder"

# ForestTrainer & its Builder
.class_ForestTrainer <- "ca/joeltherrien/randomforest/tree/ForestTrainer"
.class_ForestTrainer_Builder <- "ca/joeltherrien/randomforest/tree/ForestTrainer$ForestTrainerBuilder"


# Covariate classes
.class_Covariate <- "ca/joeltherrien/randomforest/covariates/Covariate"
.class_BooleanCovariate <- "ca/joeltherrien/randomforest/covariates/bool/BooleanCovariate"
.class_FactorCovariate <- "ca/joeltherrien/randomforest/covariates/factor/FactorCovariate"
.class_NumericCovariate <- "ca/joeltherrien/randomforest/covariates/numeric/NumericCovariate"

# Forest class
.class_Forest <- "ca/joeltherrien/randomforest/tree/Forest"

# ResponseCombiner classes
.class_ResponseCombiner <- "ca/joeltherrien/randomforest/tree/ResponseCombiner"
.class_CompetingRiskResponseCombiner <- "ca/joeltherrien/randomforest/responses/competingrisk/combiner/CompetingRiskResponseCombiner"
.class_CompetingRiskFunctionCombiner <- "ca/joeltherrien/randomforest/responses/competingrisk/combiner/CompetingRiskFunctionCombiner"
.class_MeanResponseCombiner <- "ca/joeltherrien/randomforest/responses/regression/MeanResponseCombiner"

# SplitFinder classes
.class_SplitFinder <- "ca/joeltherrien/randomforest/tree/SplitFinder"
.class_GrayLogRankSplitFinder <- "ca/joeltherrien/randomforest/responses/competingrisk/splitfinder/GrayLogRankSplitFinder"
.class_LogRankSplitFinder <- "ca/joeltherrien/randomforest/responses/competingrisk/splitfinder/LogRankSplitFinder"
.class_WeightedVarianceSplitFinder <- "ca/joeltherrien/randomforest/responses/regression/WeightedVarianceSplitFinder"

.object_Optional <- function(forest=NULL){
  if(is.null(forest)){
    return(.jcall("java/util/Optional", "Ljava/util/Optional;", "empty"))
  } else{
    forest <- .jcast(forest, .class_Object)
    return(.jcall("java/util/Optional", "Ljava/util/Optional;", "of", forest))
  }
  
}

# When a class object is returned, rJava often often wants L prepended and ; appended. 
# So a list that returns "java/lang/Object" should show "Ljava/lang/Object;"
# This function does that.
makeResponse <- function(className){
  return(paste0("L", className, ";"))
}
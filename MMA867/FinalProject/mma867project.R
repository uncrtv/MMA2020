#### MMA 867 Project Supporting Functions ####

#### Model test for thresholds
model_test <- function(probabilities, actual) {
	# (num, factor -> matrix)
	# From a vector of probabilities and actual classifications (from known data), return the TPR and FPR at different thresholds.

	require("caret")

  thresholds <- vector()
  sens_v <- vector()
  spec_v <- vector()
  
  for (i in 1:19) {
    threshold <- i * 0.05
    classifications <- as.factor(as.integer(probabilities > threshold))
    
    confusion_matrix <- confusionMatrix(classifications, actual, positive = '1')

    sens <- confusion_matrix$byClass[1]
    spec <- confusion_matrix$byClass[2]
    
    thresholds <- rbind(thresholds, threshold)
    sens_v <- rbind(sens_v, sens)
    spec_v <- rbind(spec_v, 1 - spec)
  }
  output <- cbind(thresholds, sens_v, spec_v)
  colnames(output) <- c("Threshold", "True Positive Rate", "False Positive Rate")
  rownames(output) <- NULL
  # print(output)
  return(output)
}

model_test_verbose <- function(probabilities, actual) {
  # (num, factor -> matrix)
  # From a vector of probabilities and actual classifications (from known data), return the TPR and FPR at different thresholds.
  
  require("caret")
  
  for (i in 1:9) {
    threshold <- i * 0.1
    classifications <- as.factor(as.integer(probabilities > threshold))
    confusion_matrix <- confusionMatrix(classifications, actual, positive = '1')
    
    cat("\n\n----------\n\nThreshold: ", threshold, "\n", sep = '')
    print(confusion_matrix$byClass)
  }
}
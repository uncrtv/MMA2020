#### MMA 867 Assignment 3 Supporting Functions ####

#### Model test for thresholds
a3.model_test <- function(probabilities, actual) {
	# (num, factor -> matrix)
	# From a vector of probabilities and actual classifications (from known data), return the profit at different thresholds.
	thresholds <- vector()
	profits <- vector()
	opportunity_costs <- vector()

	for (i in 10:90) {
		threshold <- i * 0.01
		classifications <- as.factor(as.integer(probabilities > threshold))
		# cat("Threshold: ", threshold, "\n\n", sep = '')
		confusion_matrix <- confusionMatrix(classifications, actual, positive = '1')
		# For debugging purposes
		# print(confusion_matrix)
		# cat("---------------\n\n")

		true_positive <- confusion_matrix$table[2,2]
		true_negative <- confusion_matrix$table[1,1]
		false_positive <- confusion_matrix$table[2,1]
		false_negative <- confusion_matrix$table[1,2]

		profit <- true_negative * 1500 - false_negative * 5000
		opportunity_cost <- false_positive * 1500

		thresholds <- rbind(thresholds, threshold)
		profits <- rbind(profits, profit)
		opportunity_costs <- rbind(opportunity_costs, opportunity_cost)
	}
	output <- cbind(thresholds, profits, opportunity_costs)
	colnames(output) <- c("Threshold", "Profit", "Opportunity Cost")
	rownames(output) <- NULL
	# print(output)
	return(output)
}

#### Output maximum profit
a3.max_profit <- function(probabilities, actual) {
	# (num, factor -> num)
	# Output the maximum profit for the model.
	profit_table <- a3.model_test(probabilities, actual)
	return(max(profit_table[,2]))
}
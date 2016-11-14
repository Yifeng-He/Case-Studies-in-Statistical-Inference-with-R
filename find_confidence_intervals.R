# **************************************************************************************
# Problem 1: find Confidence interval for a sample mean with t-statistic

# Suppose we want to estimate the average weight of an adult male in Dekalb County, Georgia. We draw a random sample 
# of 1,000 men from a population of 1,000,000 men and weigh them. We find that the average man in our sample weighs 
# 180 pounds, and the standard deviation of the sample is 30 pounds. What is the 95% confidence interval.

# define function to find the confidence interval for a sample mean
findConfidenceIntervalForMean_tScore = function(sampleMean, sampleStd, confidenceLevel, numSamples) {
  # accumulated probability
  accumulatedProbability = 1-(1-confidenceLevel)/2.0
  # degree of freedoms
  df = numSamples-1
  # find t-value for a given accumulated probability
  tValue = qt(accumulatedProbability, df)
  # standard error for the population
  standardError = sampleStd/sqrt(numSamples)
  upperBound = sampleMean + tValue*standardError
  lowerBound = sampleMean - tValue*standardError
  c(lowerBound, upperBound)
}

print("Problem 1: find Confidence interval for a sample mean with t-statistic")
print("The confidence interval with 95% confidence is: ")
print(findConfidenceIntervalForMean_tScore(180, 30, 0.95, 1000))
cat("\n")

# **************************************************************************************
# Problem 2: find Margin of Error for a sample mean with t-statistic

# Nine hundred (900) high school freshmen were randomly selected for a national survey. Among survey participants, 
# the mean grade-point average (GPA) was 2.7, and the standard deviation was 0.4. What is the margin of error, 
# assuming a 95% confidence level?

# define function to find Margin of Error for a sample mean
findMaginOfErrorForMean_tScore = function(sampleMean, sampleStd, confidenceLevel, numSamples) {
  findConfidenceIntervalForMean_tScore(sampleMean, sampleStd, confidenceLevel, numSamples) - sampleMean
}

print("Problem 2: find Margin of Error for a sample mean with t-statistic")
print("The margin of error with 95% confidence is: ")
print(findMaginOfErrorForMean_tScore(2.7, 0.4, 0.95, 900))
cat("\n")

# **************************************************************************************
# Problem 3: find Confidence interval for a smaple proportion with t-statistic

# A major metropolitan newspaper selected a simple random sample of 1,600 readers from their list of 100,000 subscribers. 
# They asked whether the paper should increase its coverage of local news. Forty percent of the sample wanted more local news. 
# What is the 99% confidence interval for the proportion of readers who would like more coverage of local news?

# define function to find the confidence interval for a sample proportion
findConfidenceIntervalForProportion_tScore = function(sampleProportion, confidenceLevel, numSamples) {
  # accumulated probability
  accumulatedProbability = 1-(1-confidenceLevel)/2.0
  # degree of freedoms
  df = numSamples-1
  # find t-value for a given accumulated probability
  tValue = qt(accumulatedProbability, df)
  # standard error 
  standardError = sqrt(sampleProportion*(1-sampleProportion))/sqrt(numSamples)
  upperBound = sampleProportion + tValue*standardError
  lowerBound = sampleProportion - tValue*standardError
  c(lowerBound, upperBound)
}

print("Problem 3: find Confidence interval for a smaple proportion with t-statistic")
print("The confidence interval with 99% confidence is: ")
print(findConfidenceIntervalForProportion_tScore(0.4, 0.99, 1600))
cat("\n")

# **************************************************************************************
# Problem 4: find confidence interval for difference between proportions with z-statistic

# Suppose the Cartoon Network conducts a nation-wide survey to assess viewer attitudes toward Superman. 
# Using a simple random sample, they select 400 boys and 300 girls to participate in the study. Forty percent of the boys 
# say that Superman is their favorite character, compared to thirty percent of the girls. What is the 90% confidence interval 
# for the true difference in attitudes toward Superman?

# define function to find the confidence interval for Difference Between Proportions
findConfidenceIntervalForProportionDifference_zScore = function(sampleProportion1, sampleProportion2, confidenceLevel, numSamples1, numSamples2) {
  # accumulated probability
  accumulatedProbability = 1-(1-confidenceLevel)/2.0
  # find z-value for a given accumulated probability using standard nornal distribution
  zValue = qnorm(accumulatedProbability)
  # standard error 
  standardError = sqrt( sampleProportion1*(1-sampleProportion1)/numSamples1 + sampleProportion2*(1-sampleProportion2)/numSamples2)
  upperBound = sampleProportion1 - sampleProportion2 + zValue*standardError
  lowerBound = sampleProportion1 - sampleProportion2 - zValue*standardError
  c(lowerBound, upperBound)
}

print("Problem 4: find confidence interval for difference between proportions with z-statistic")
print("The confidence interval with 90% confidence is: ")
print(findConfidenceIntervalForProportionDifference_zScore(0.4, 0.3, 0.9, 300, 400))
cat("\n")

# **************************************************************************************
# Problem 5: find confidence interval for difference between means with t-statistic

# Suppose that simple random samples of college freshman are selected from two universities - 15 students from school A and 20 students 
# from school B. On a standardized test, the sample from school A has an average score of 1000 with a standard deviation of 100. 
# The sample from school B has an average score of 950 with a standard deviation of 90.
# What is the 90% confidence interval for the difference in test scores at the two schools, assuming that test scores came from normal 
# distributions in both schools? (Hint: Since the sample sizes are small, use a t score as the critical value.)

# define function to find confidence interval for difference between means with t-statistic
findConfidenceIntervalForMeanDifference_tScore = function(sampleMean1, sampleMean2, sampleStd1, sampleStd2, confidenceLevel, numSamples1, numSamples2) {
  # accumulated probability
  accumulatedProbability = 1-(1-confidenceLevel)/2.0
  # the degree of freedoms
  df =  (sampleStd1^2/numSamples1 + sampleStd2^2/numSamples2)^2 / 
    ((sampleStd1^2 /numSamples1)^2 / (numSamples1 - 1)  + (sampleStd2^2 /numSamples2)^2 / (numSamples2 - 1) )
  # find t-value for a given accumulated probability using a Student's t-distribution
  tValue = qt(accumulatedProbability, df)
  # standard error 
  standardError = sqrt( sampleStd1^2/numSamples1 + sampleStd2^2/numSamples2)
  # condidence interval
  upperBound = sampleMean1 - sampleMean2 + tValue*standardError
  lowerBound = sampleMean1 - sampleMean2 - tValue*standardError
  c(lowerBound, upperBound)
}

print("Problem 5: find confidence interval for difference between means with t-statistic")
print("The confidence interval with 90% confidence is: ")
print(findConfidenceIntervalForMeanDifference_tScore(1000, 950, 100, 90, 0.9, 15, 20))
cat("\n")

# **************************************************************************************
# Problem 6: find confidence interval for difference between means with z-statistic

# The local baseball team conducts a study to find the amount spent on refreshments at the ball park. Over the course of the season 
# they gather simple random samples of 500 men and 1000 women. For men, the average expenditure was $20, with a standard deviation 
# of $3. For women, it was $15, with a standard deviation of $2.
# What is the 99% confidence interval for the spending difference between men and women? Assume that the two populations are independent 
# and normally distributed.

# define function to find confidence interval for difference between means with z-statistic
findConfidenceIntervalForMeanDifference_zScore = function(sampleMean1, sampleMean2, sampleStd1, sampleStd2, confidenceLevel, numSamples1, numSamples2) {
  # accumulated probability
  accumulatedProbability = 1-(1-confidenceLevel)/2.0
  # find z-value for a given accumulated probability 
  zValue = qnorm(accumulatedProbability)
  # standard error 
  standardError = sqrt( sampleStd1^2/numSamples1 + sampleStd2^2/numSamples2)
  # condidence interval
  upperBound = sampleMean1 - sampleMean2 + zValue*standardError
  lowerBound = sampleMean1 - sampleMean2 - zValue*standardError
  c(lowerBound, upperBound)
}

print("Problem 6: find confidence interval for difference between means with z-statistic")
print("The confidence interval with 99% confidence is: ")
print(findConfidenceIntervalForMeanDifference_zScore(20, 15, 3, 2, 0.99, 500, 1000))
cat("\n")

# **************************************************************************************
# Problem 7: find confidence interval for mean difference between matched pairs with t-statistic 

# Twenty-two students were randomly selected from a population of 1000 students. The sampling method was simple random sampling. 
# All of the students were given a standardized English test and a standardized math test. Test results are summarized below.
# Student 	English 	Math 	Difference, d 	(d - d_avg)^2
# 1 	95 	90 	5 	16
# 2 	89 	85 	4 	9
# 3 	76 	73 	3 	4
# 4 	92 	90 	2 	1
# 5 	91 	90 	1 	0
# 6 	53 	53 	0 	1
# 7 	67 	68 	-1 	4
# 8 	88 	90 	-2 	9
# 9 	75 	78 	-3 	16
# 10 	85 	89 	-4 	25
# 11 	90 	95 	-5 	36
# 
# Student 	English 	Math 	Difference, d 	(d - d_avg)^2
# 12 	85 	83 	2 	1
# 13 	87 	83 	4 	9
# 14 	85 	83 	2 	1
# 15 	85 	82 	3 	4
# 16 	68 	65 	3 	4
# 17 	81 	79 	2 	1

# 18 	84 	83 	1 	0
# 19 	71 	60 	11 	100/
# 20 	46 	47 	-1 	4
# 21 	75 	77 	-2 	9
# 22 	80 	83 	-3 	16
# Find the 90% confidence interval for the mean difference between student scores on the math and English tests. Assume that the mean 
# differences are approximately normally distributed.

# define function to find confidence interval for mean difference between matched pairs with t-statistic
findConfidenceIntervalForMeanPairDifference_tScore = function(vector1, vector2, confidenceLevel) {
  # accumulated probability
  accumulatedProbability = 1-(1-confidenceLevel)/2.0
  # number of pairs
  numberPairs = length(vector1)
  # difference vector
  vectorDifference = vector1 - vector2
  # degree of freedoms
  df = numberPairs - 1
  # find t-value for a given accumulated probability using a Student's t-distribution
  tValue = qt(accumulatedProbability, df)
  # standard error 
  standardError = sd(vectorDifference)/sqrt(numberPairs)
  # condidence interval
  upperBound = mean(vectorDifference) + tValue*standardError
  lowerBound = mean(vectorDifference) - tValue*standardError
  c(lowerBound, upperBound)
}

print("Problem 7: find confidence interval for mean difference between matched pairs with t-statistic")
print("The confidence interval with 90% confidence is: ")
EnglishScores = c(95,89,76,92,91,53,67,88,75,85,90,85,87,85,85,68,81,84,71,46,75,80)
MathScores = c(90,85,73,90,90,53,68,90,78,89,95,83,83,83,82,65,79,83,60,47,77,83)
print(findConfidenceIntervalForMeanPairDifference_tScore(EnglishScores, MathScores, 0.9))
cat("\n")

# **************************************************************************************
# Problem 8: find confidence interval for the slope of the regression line with t-statistic

# The local utility company surveys 101 randomly selected customers. For each survey participant, 
# the company collects the following: annual electric bill (in dollars) and home size (in square feet). 
# Output from a regression analysis appears below.

# Regression equation:   Annual bill = 0.55 * Home size + 15

# Predictor 	Coef 	standard error  	
# Constant 	  15 	     3 	           
# Home size 	0.55 	   0.24 	       
# 
# What is the 99% confidence interval for the slope of the regression line?

# define function to find confidence interval for the slope of the regression line with t-statistic
# stadare error for the slope: SE = sqrt [ sum(y_observed_i - y_estaimate_i)^2 / (n - 2) ] / sqrt [ sum(x_observed_i - x_avg)^2 ] 
findConfidenceIntervalForSlope_tScore = function(sampleSlope, slopeStd, confidenceLevel, numSamples) {
  # accumulated probability
  accumulatedProbability = 1-(1-confidenceLevel)/2.0
  # the degree of freedoms
  df = numSamples - 1 
  # find t-value for a given accumulated probability using a Student's t-distribution
  tValue = qt(accumulatedProbability, df)
  # condidence interval
  upperBound = sampleSlope + tValue*slopeStd
  lowerBound = sampleSlope - tValue*slopeStd
  c(lowerBound, upperBound)
}

print("Problem 8: find confidence interval for the slope of the regression line with t-statistic")
print("The confidence interval with 99% confidence is: ")
print(findConfidenceIntervalForSlope_tScore(0.55, 0.24, 0.99, 101))
cat("\n")





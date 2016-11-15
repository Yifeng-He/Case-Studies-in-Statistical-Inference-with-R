# **************************************************************************************
# Problem 1: Hypothesis Test for a Proportion with Two-Tailed z-Test

# The CEO of a large electric utility claims that 80 percent of his 1,000,000 customers are very satisfied 
# with the service they receive. To test this claim, the local newspaper surveyed 100 customers, using simple 
# random sampling. Among the sampled customers, 73 percent say they are very satisified. Based on these findings, 
# can we reject the CEO's hypothesis that 80% of the customers are very satisfied? Use a 0.05 level of significance. 

# define function to find the P-value with two-tailed z-test
findPValueForProportion_TwoTailed_zTest = function(claimedProportion, actualProportion, numSamples) {
  # standard error
  standardError = sqrt( claimedProportion*(1-claimedProportion)/numSamples )
  # z-value
  zValue = (actualProportion - claimedProportion)/standardError
  # the cumulative probability for the z value for standard normal distribution
  cumulativeProbability = pnorm(zValue)
  # the p-value with two tails
  if (cumulativeProbability < 0.5 ) {
    pValue = 2*cumulativeProbability
  }
  else {
    pValue = 2*(1-cumulativeProbability)
  }
  return(pValue)
}

# Explanation: In this problem, the level of significance is 0.05, which means the confidence level is 0.95. 
# The null hypothesis is p = 0.8, whcih can be explained as: in 95% of times, the percentage calculated from samples
# should be within the confidence interval.
# Based on the P-Value in this test (e.g., 0.73 from 100 customers), we know that the result is within the confidence
# interval, therefore, we cannot reject the null hypothesis.

# hypothesis test for h0: p == 0.8
pValue = findPValueForProportion_TwoTailed_zTest(0.8, 0.73, 100)
significanceLevel = 0.05
cat("Problem 1: Hypothesis Test for a Proportion with Two-Tailed z-Test")
cat("\nP-value is", pValue, ", level of significance is", significanceLevel, ".\n")
if (pValue < significanceLevel) {
  cat("The null hypothesis cannot be accepted.")
}
if (pValue >= significanceLevel) {
  cat("The null hypothesis cannot be rejected.")
}

# **************************************************************************************
# Problem 2: Hypothesis Test for a Proportion with One-Tailed z-Test

# Suppose the previous example is stated a little bit differently. Suppose the CEO claims that at least 80 percent of 
# the company's 1,000,000 customers are very satisfied. Again, 100 customers are surveyed using simple random sampling. 
# The result: 73 percent are very satisfied. Based on these results, should we accept or reject the CEO's hypothesis? 
# Assume a significance level of 0.05. 

# define function to find the P-value with one-tailed z-test
findPValueForProportion_OneTailed_zTest = function(claimedProportion, actualProportion, numSamples) {
  # standard error
  standardError = sqrt( claimedProportion*(1-claimedProportion)/numSamples )
  # z-value
  zValue = (actualProportion - claimedProportion)/standardError
  # the cumulative probability for the z value for standard normal distribution
  cumulativeProbability = pnorm(zValue)
  # the p-value with one tail
  pValue = cumulativeProbability
  return(pValue)
}

# hypothesis test for h0: p >= 0.8
pValue = findPValueForProportion_OneTailed_zTest(0.8, 0.73, 100)
significanceLevel = 0.05
cat("\n\n")
cat("Problem 2: Hypothesis Test for a Proportion with One-Tailed z-Test")
cat("\nP-value is", pValue, ", level of significance is", significanceLevel, ".\n")
if (pValue < significanceLevel) {
  cat("The null hypothesis cannot be accepted.")
}
if (pValue >= significanceLevel) {
  cat("The null hypothesis cannot be rejected.")
}

# **************************************************************************************
# Problem 3: Hypothesis Test for Difference Between Proportions with Two-Tailed z-Test

# Suppose the Acme Drug Company develops a new drug, designed to prevent colds. The company states that the drug is equally 
# effective for men and women. To test this claim, they choose a a simple random sample of 100 women and 200 men from a 
# population of 100,000 volunteers.
# At the end of the study, 38% of the women caught a cold; and 51% of the men caught a cold. Based on these findings, 
# can we reject the company's claim that the drug is equally effective for men and women? Use a 0.05 level of significance. 

# define function to find the P-value with two-tailed z-test
findPValueForProportionDifference_TwoTailed_zTest = function(claimedProportionDifference, actualProportion1, actualProportion2, numSamples1, numSamples2) {
  # actual difference bwteen two proportions
  actualProportionDifference = actualProportion1 - actualProportion2
  # standard error
  standardError = sqrt( actualProportion1*(1-actualProportion1)/numSamples1 + actualProportion2*(1-actualProportion2)/numSamples2)
  # z-value
  zValue = (actualProportionDifference - claimedProportionDifference)/standardError
  # the cumulative probability for the z value for standard normal distribution
  cumulativeProbability = pnorm(zValue)
  # the p-value with two tails
  if (cumulativeProbability < 0.5 ) {
    pValue = 2*cumulativeProbability
  }
  else {
    pValue = 2*(1-cumulativeProbability)
  }
  return(pValue)
}

# hypothesis test for h0: p1 - p2 == 0
pValue = findPValueForProportionDifference_TwoTailed_zTest(0, 0.38, 0.51, 100, 200)
significanceLevel = 0.05
cat("\n\n")
cat("Problem 3: Hypothesis Test for Difference Between Proportions with Two-Tailed z-Test")
cat("\nP-value is", pValue, ", level of significance is", significanceLevel, ".\n")
if (pValue < significanceLevel) {
  cat("The null hypothesis cannot be accepted.")
}
if (pValue >= significanceLevel) {
  cat("The null hypothesis cannot be rejected.")
}

# **************************************************************************************
# Problem 4: Hypothesis Test for Difference Between Proportions with One-Tailed z-Test

# Suppose the previous example is stated a little bit differently. Suppose the Acme Drug Company develops a new drug, 
# designed to prevent colds. The company states that the drug is more effective for women than for men. To test this claim, 
# they choose a a simple random sample of 100 women and 200 men from a population of 100,000 volunteers.
# At the end of the study, 38% of the women caught a cold; and 51% of the men caught a cold. Based on these findings, 
# can we conclude that the drug is more effective for women than for men? Use a 0.01 level of significance. 

# define function to find the P-value with one-tailed z-test
findPValueForProportionDifference_OneTailed_zTest = function(claimedProportionDifference, actualProportion1, actualProportion2, numSamples1, numSamples2) {
  # actual difference bwteen two proportions
  actualProportionDifference = actualProportion1 - actualProportion2
  # standard error
  standardError = sqrt( actualProportion1*(1-actualProportion1)/numSamples1 + actualProportion2*(1-actualProportion2)/numSamples2)
  # z-value
  zValue = (actualProportionDifference - claimedProportionDifference)/standardError
  # the cumulative probability for the z value for standard normal distribution
  cumulativeProbability = pnorm(zValue)
  # the p-value with one tail
  pValue = (1-cumulativeProbability)
  return(pValue)
}

# hypothesis test for h0: p1 - p2 < 0 (p1 is the proportion of women caught cold)
pValue = findPValueForProportionDifference_OneTailed_zTest(0, 0.38, 0.51, 100, 200)
significanceLevel = 0.01
cat("\n\n")
cat("Problem 4: Hypothesis Test for Difference Between Proportions with One-Tailed z-Test")
cat("\nP-value is", pValue, ", level of significance is", significanceLevel, ".\n")
if (pValue < significanceLevel) {
  cat("The null hypothesis cannot be accepted.")
}
if (pValue >= significanceLevel) {
  cat("The null hypothesis cannot be rejected.")
}

# **************************************************************************************
# Problem 5: Hypothesis Test for a Mean with Two-Tailed t-Test

# An inventor has developed a new, energy-efficient lawn mower engine. He claims that the engine will run continuously 
# for 5 hours (300 minutes) on a single gallon of regular gasoline. From his stock of 2000 engines, the inventor selects 
# a simple random sample of 50 engines for testing. The engines run for an average of 295 minutes, with a standard 
# deviation of 20 minutes. Test the null hypothesis that the mean run time is 300 minutes against the alternative 
# hypothesis that the mean run time is not 300 minutes. Use a 0.05 level of significance. (Assume that run times for 
# the population of engines are normally distributed.) 

# define function to find the P-value with two-tailed t-test
findPValueForMean_TwoTailed_tTest = function(claimedMean, actualMean, actualStd, numSamples) {
  # standard error
  standardError = actualStd/sqrt(numSamples) 
  # degrees of freedom
  df = numSamples - 1
  # t-value
  tValue = (actualMean - claimedMean)/standardError
  # the cumulative probability for the t value based on student t distribution
  cumulativeProbability = pt(tValue, df)
  # the p-value with two tails
  if (cumulativeProbability < 0.5 ) {
    pValue = 2*cumulativeProbability
  }
  else {
    pValue = 2*(1-cumulativeProbability)
  }
  return(pValue)
}

# hypothesis test for h0: u == 300 
pValue = findPValueForMean_TwoTailed_tTest(300, 295, 20, 50)
significanceLevel = 0.05
cat("\n\n")
cat("Problem 5: Hypothesis Test for a Mean with Two-Tailed t-Test")
cat("\nP-value is", pValue, ", level of significance is", significanceLevel, ".\n")
if (pValue < significanceLevel) {
  cat("The null hypothesis cannot be accepted.")
}
if (pValue >= significanceLevel) {
  cat("The null hypothesis cannot be rejected.")
}

# **************************************************************************************
# Problem 6: Hypothesis Test for a Mean with One-Tailed t-Test

# Bon Air Elementary School has 1000 students. The principal of the school thinks that the average IQ of students 
# at Bon Air is at least 110. To prove her point, she administers an IQ test to 20 randomly selected students. 
# Among the sampled students, the average IQ is 108 with a standard deviation of 10. Based on these results, should 
# the principal accept or reject her original hypothesis? Assume a significance level of 0.01. (Assume that test scores 
# in the population of engines are normally distributed.) 

# define function to find the P-value with one-tailed t-test
findPValueForMean_OneTailed_tTest = function(claimedMean, actualMean, actualStd, numSamples) {
  # standard error
  standardError = actualStd/sqrt(numSamples) 
  # degrees of freedom
  df = numSamples - 1
  # t-value
  tValue = (actualMean - claimedMean)/standardError
  # the cumulative probability for the t value based on student t distribution
  cumulativeProbability = pt(tValue, df)
  # the p-value with one tail
  pValue = cumulativeProbability
  return(pValue)
}

# hypothesis test for h0: u >= 110 
pValue = findPValueForMean_OneTailed_tTest(110, 108, 10, 20)
significanceLevel = 0.01
cat("\n\n")
cat("Problem 6: Hypothesis Test for a Mean with One-Tailed t-Test")
cat("\nP-value is", pValue, ", level of significance is", significanceLevel, ".\n")
if (pValue < significanceLevel) {
  cat("The null hypothesis cannot be accepted.")
}
if (pValue >= significanceLevel) {
  cat("The null hypothesis cannot be rejected.")
}

# **************************************************************************************
# Problem 7: Hypothesis Test for Difference Between Means with Two-Tailed t-Test

# Within a school district, students were randomly assigned to one of two Math teachers - Mrs. Smith and 
# Mrs. Jones. After the assignment, Mrs. Smith had 30 students, and Mrs. Jones had 25 students.
# At the end of the year, each class took the same standardized test. Mrs. Smith's students had an 
# average test score of 78, with a standard deviation of 10; and Mrs. Jones' students had an average test score of 85, 
# with a standard deviation of 15.
# Test the hypothesis that Mrs. Smith and Mrs. Jones are equally effective teachers. Use a 0.10 level of significance. 
# (Assume that student performance is approximately normal.) 

# define function to find the P-value with two-tailed t-test
findPValueForMeanDifference_TwoTailed_tTest = function(claimedMeanDifference, actualMean1, actualMean2, actualStd1, actualStd2, numSamples1, numSamples2) {
  # actual mean difference
  actualMeanDifference = actualMean1 - actualMean2
  # standard error
  standardError = sqrt( actualStd1^2/numSamples1 + actualStd2^2/numSamples2 ) 
  # the degrees of freedom
  df =  (actualStd1^2/numSamples1 + actualStd2^2/numSamples2)^2 / 
    ((actualStd1^2 /numSamples1)^2 / (numSamples1 - 1)  + (actualStd2^2 /numSamples2)^2 / (numSamples2 - 1) )  
  # t-value
  tValue = (actualMeanDifference - claimedMeanDifference)/standardError
  # the cumulative probability for the t value based on student t distribution
  cumulativeProbability = pt(tValue, df)
  # the p-value with two tails
  if (cumulativeProbability < 0.5 ) {
    pValue = 2*cumulativeProbability
  }
  else {
    pValue = 2*(1-cumulativeProbability)
  }
  return(pValue)
}

# hypothesis test for h0: u1 - u2 == 0 
pValue = findPValueForMeanDifference_TwoTailed_tTest(0, 78, 85, 10, 15, 30, 25)
significanceLevel = 0.10
cat("\n\n")
cat("Problem 7: Hypothesis Test for Difference Between Means with Two-Tailed t-Test")
cat("\nP-value is", pValue, ", level of significance is", significanceLevel, ".\n")
if (pValue < significanceLevel) {
  cat("The null hypothesis cannot be accepted.")
}

if (pValue >= significanceLevel) {
  cat("The null hypothesis cannot be rejected.")
}

# **************************************************************************************
# Problem 8: Hypothesis Test for Difference Between Means with One-Tailed t-Test

# The Acme Company has developed a new battery. The engineer in charge claims that the new battery will operate 
# continuously for at least 7 minutes longer than the old battery.
# To test the claim, the company selects a simple random sample of 100 new batteries and 100 old batteries. 
# The old batteries run continuously for 190 minutes with a standard deviation of 20 minutes; the new batteries, 
# 200 minutes with a standard deviation of 40 minutes.
# Test the engineer's claim that the new batteries run at least 7 minutes longer than the old. Use a 0.05 level 
# of significance. (Assume that there are no outliers in either sample.) 

# define function to find the P-value with one-tailed t-test
findPValueForMeanDifference_OneTailed_tTest = function(claimedMeanDifference, actualMean1, actualMean2, actualStd1, actualStd2, numSamples1, numSamples2) {
  # actual mean difference
  actualMeanDifference = actualMean1 - actualMean2
  # standard error
  standardError = sqrt( actualStd1^2/numSamples1 + actualStd2^2/numSamples2 ) 
  # the degrees of freedom
  df =  (actualStd1^2/numSamples1 + actualStd2^2/numSamples2)^2 / 
    ((actualStd1^2 /numSamples1)^2 / (numSamples1 - 1)  + (actualStd2^2 /numSamples2)^2 / (numSamples2 - 1) )  
  # t-value
  tValue = (actualMeanDifference - claimedMeanDifference)/standardError
  # the cumulative probability for the t value based on student t distribution
  cumulativeProbability = pt(tValue, df)
  # the p-value with one tails
  pValue = cumulativeProbability
  return(pValue)
}

# hypothesis test for h0: u1 - u2 >= 7 
pValue = findPValueForMeanDifference_OneTailed_tTest(7, 200, 190, 40, 20, 100, 100)
significanceLevel = 0.05
cat("\n\n")
cat("Problem 8: Hypothesis Test for Difference Between Means with One-Tailed t-Test")
cat("\nP-value is", pValue, ", level of significance is", significanceLevel, ".\n")
if (pValue < significanceLevel) {
  cat("The null hypothesis cannot be accepted.")
}
if (pValue >= significanceLevel) {
  cat("The null hypothesis cannot be rejected.")
}

# **************************************************************************************
# Problem 9: Hypothesis Test for Difference Between Paired Means with Two-Tailed t-Test

# Forty-four sixth graders were randomly selected from a school district. Then, they were divided into 22 matched 
# pairs, each pair having equal IQ's. One member of each pair was randomly selected to receive special training. 
# Then, all of the students were given an IQ test. Test results are summarized below.
# 
# Pair 	Training 	No training 	Difference (d) 	(d - d)^2
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
# Pair 	Training 	No training 	Difference (d) 	(d - d)^2
# 12 	85 	83 	2 	1
# 13 	87 	83 	4 	9
# 14 	85 	83 	2 	1
# 15 	85 	82 	3 	4
# 16 	68 	65 	3 	4
# 17 	81 	79 	2 	1
# 18 	84 	83 	1 	0
# 19 	71 	60 	11 	100
# 20 	46 	47 	-1 	4
# 21 	75 	77 	-2 	9
# 22 	80 	83 	-3 	16
# 
# ??(d - d)2 = 270
# d_avg = 1
# 
# Do these results provide evidence that the special training helped or hurt student performance? Use an 0.05 
# level of significance. Assume that the mean differences are approximately normally distributed.

# define function to find the P-value with two-tailed t-test
findPValueForMeanPairDifference_TwoTailed_tTest = function(calimedMean, vector1, vector2) {
  # number of pairs
  numberPairs = length(vector1)
  # difference vector
  vectorDifference = vector1 - vector2
  # the mean
  meanPairDifference = mean(vectorDifference)
  # standard error 
  standardError = sd(vectorDifference)/sqrt(numberPairs)  
  # degrees of freedom
  df = numberPairs - 1
  # t-value
  tValue = (meanPairDifference - calimedMean)/standardError
  # the cumulative probability for the t value based on student t distribution
  cumulativeProbability = pt(tValue, df)
  # the p-value with two tails
  if (cumulativeProbability < 0.5 ) {
    pValue = 2*cumulativeProbability
  }
  else {
    pValue = 2*(1-cumulativeProbability)
  }
  return(pValue)
}

# hypothesis test for h0: u_d == 0 
vector1 = c(95,89,76,92,91,53,67,88,75,85,90,85,87,85,85,68,81,84,71,46,75,80)
vector2 = c(90,85,73,90,90,53,68,90,78,89,95,83,83,83,82,65,79,83,60,47,77,83)
pValue = findPValueForMeanPairDifference_TwoTailed_tTest(0, vector1, vector2)
significanceLevel = 0.05
cat("\n\n")
cat("Problem 9: Hypothesis Test for Difference Between Paired Means with Two-Tailed t-Test")
cat("\nP-value is", pValue, ", level of significance is", significanceLevel, ".\n")
if (pValue < significanceLevel) {
  cat("The null hypothesis cannot be accepted.")
}
if (pValue >= significanceLevel) {
  cat("The null hypothesis cannot be rejected.")
}

# **************************************************************************************
# Problem 10: Hypothesis Test for Goodness of Fit with Chi-squared Test

# Acme Toy Company prints baseball cards. The company claims that 30% of the cards are rookies, 60% veterans, 
# and 10% are All-Stars.
# Suppose a random sample of 100 cards has 50 rookies, 45 veterans, and 5 All-Stars. Is this consistent with 
# Acme's claim? Use a 0.05 level of significance.

# define function to find the P-value with chi-squared test
findPValueForFitGoodness_chiSquaredTest = function(vectorClaimedDistribution, vectorActualCounts, numSamples) {
  # number of classes
  numberClasses = length(vectorClaimedDistribution)
  # degrees of freedom
  df = numberClasses - 1
  
  # chi-squared value is given by ??^2 = sum((O_i - E_i)^2 / E_i), where O_i is the obaserved count for class-i
  # E_i is the expected count for class-i
  expectedCounts = numSamples*vectorClaimedDistribution
  chiSquaredValue = sum( (expectedCounts - vectorActualCounts)^2/expectedCounts )

  # the cumulative probability for the chi-squared value
  cumulativeProbability = pchisq(chiSquaredValue, df)
  # the p-value with one tail (Chi-squared test is always one-tailed)
  pValue = 1-cumulativeProbability
  return(pValue)
}

# hypothesis test for h0: the proportion of rookies, veterans, and All-Stars is 30%, 60% and 10%, respectively.
vectorClaimedDistribution = c(0.3, 0.6, 0.1)
vectorActualCounts = c(50, 45, 5)
pValue = findPValueForFitGoodness_chiSquaredTest(vectorClaimedDistribution, vectorActualCounts, 100)
significanceLevel = 0.05
cat("\n\n")
cat("Problem 10: Hypothesis Test for Goodness of Fit with Chi-squared Test")
cat("\nP-value is", pValue, ", level of significance is", significanceLevel, ".\n")
if (pValue < significanceLevel) {
  cat("The null hypothesis cannot be accepted.")
}
if (pValue >= significanceLevel) {
  cat("The null hypothesis cannot be rejected.")
}

# **************************************************************************************
# Problem 11: Hypothesis Test for Homogeneity with Chi-squared Test

# In a study of the television viewing habits of children, a developmental psychologist selects a random sample 
# of 300 first graders - 100 boys and 200 girls. Each child is asked which of the following TV programs they 
# like best: The Lone Ranger, Sesame Street, or The Simpsons. 

# Results are shown in the contingency table below.
#               Lone Ranger 	Sesame Street 	The Simpsons     Row total
# Boys 	           50 	       30 	           20 	             100
# Girls 	         50 	       80 	           70 	             200
# Column total 	   100 	       110 	           90 	             300
# 
# Do the boys' preferences for these TV programs differ significantly from the girls' preferences? Use a 0.05 level of significance.

# define function to find the P-value with chi-squared test
findPValueForHomogeneity_chiSquaredTest = function(vector1, vector2, numGroups) {
  # number of samples in group 1
  numberSamples1 = sum(vector1)
  # number of samples in group 2
  numberSamples2 = sum(vector2)
  # number of classes
  numberClasses = length(vector1)
  # degrees of freedom
  df = (numberClasses - 1) * (numGroups - 1 )
  # observed count matrix
  observedCountMatrix = rbind(vector1, vector2) 
  
  # find expected count matrix
  columnTotalVector = vector1 + vector2
  totalSamples = sum(vector1) + sum(vector2)
  expectedMatrixRow1 = columnTotalVector * sum(vector1)/totalSamples
  expectedMatrixRow2 = columnTotalVector * sum(vector2)/totalSamples
  expectedCountMatrix = rbind(expectedMatrixRow1, expectedMatrixRow2)
  
  # chi-squared value 
  chiSquaredValue = sum( (observedCountMatrix - expectedCountMatrix)^2/expectedCountMatrix )
  # the cumulative probability for the chi-squared value
  cumulativeProbability = pchisq(chiSquaredValue, df)
  # the p-value with one tail 
  pValue = 1-cumulativeProbability
  return(pValue)
}

# hypothesis test for h0: the proportion of boys who prefer the Lone Ranger is identical to the proportion of girls. 
# Similarly, for the other programs.
vectorGroup1 = c(50, 30, 20)
vectorGroup2 = c(50, 80, 70)
pValue = findPValueForHomogeneity_chiSquaredTest(vectorGroup1, vectorGroup2, 2)
significanceLevel = 0.05
cat("\n\n")
cat("Problem 11: Hypothesis Test for Homogeneity with Chi-squared Test")
cat("\nP-value is", pValue, ", level of significance is", significanceLevel, ".\n")
if (pValue < significanceLevel) {
  cat("The null hypothesis cannot be accepted.")
}
if (pValue >= significanceLevel) {
  cat("The null hypothesis cannot be rejected.")
}

# **************************************************************************************
# Problem 12: Hypothesis Test for Independence with Chi-squared Test

# A public opinion poll surveyed a simple random sample of 1000 voters. Respondents were classified by gender 
# (male or female) and by voting preference (Republican, Democrat, or Independent). Results are shown in the 
# contingency table below.
# 
#              Republican 	Democrat 	Independent     Row total
# Male 	        200 	       150 	        50 	          400
# Female 	      250 	       300 	        50 	          600
# Column total 	450 	       450 	       100 	          1000
# 
# Is there a gender gap? Do the men's voting preferences differ significantly from the women's preferences? 
# Use a 0.05 level of significance.

# define function to find the P-value with chi-squared test
findPValueForIndependence_chiSquaredTest = function(vector1, vector2, numGroups) {
  # number of samples in group 1
  numberSamples1 = sum(vector1)
  # number of samples in group 2
  numberSamples2 = sum(vector2)
  # number of classes
  numberClasses = length(vector1)
  # degrees of freedom
  df = (numberClasses - 1) * (numGroups - 1 )
  # observed count matrix
  observedCountMatrix = rbind(vector1, vector2) 
  
  # find expected count matrix
  columnTotalVector = vector1 + vector2
  totalSamples = sum(vector1) + sum(vector2)
  expectedMatrixRow1 = columnTotalVector * sum(vector1)/totalSamples
  expectedMatrixRow2 = columnTotalVector * sum(vector2)/totalSamples
  expectedCountMatrix = rbind(expectedMatrixRow1, expectedMatrixRow2)
  
  # chi-squared value 
  chiSquaredValue = sum( (observedCountMatrix - expectedCountMatrix)^2/expectedCountMatrix )
  # the cumulative probability for the chi-squared value
  cumulativeProbability = pchisq(chiSquaredValue, df)
  # the p-value with one tail (Chi-squared test is always one-tailed)
  pValue = 1-cumulativeProbability
  return(pValue)
}

# hypothesis test for h0: Gender and voting preferences are independent
vectorMale = c(200, 150, 50)
vectorFemale = c(250, 300, 50)
pValue = findPValueForIndependence_chiSquaredTest(vectorMale, vectorFemale, 2)
significanceLevel = 0.05
cat("\n\n")
cat("Problem 12: Hypothesis Test for Independence with Chi-squared Test")
cat("\nP-value is", pValue, ", level of significance is", significanceLevel, ".\n")
if (pValue < significanceLevel) {
  cat("The null hypothesis cannot be accepted.")
}
if (pValue >= significanceLevel) {
  cat("The null hypothesis cannot be rejected.")
}


# **************************************************************************************
# Problem 13: Hypothesis Test for Regression Slope with Two-Tailed t-Test

# The local utility company surveys 101 randomly selected customers. For each survey participant, 
# the company collects the following: annual electric bill (in dollars) and home size (in square feet). 
# Output from a regression analysis appears below.
# Regression equation:   Annual bill = 0.55 * Home size + 15

# Predictor 	Coef 	standard_error  	
# Constant 	  15 	     3 	            
# Home size 	0.55 	 0.24 	         
# 
# Is there a significant linear relationship between annual bill and home size? Use a 0.05 level of significance.

# define function to find the P-value with two-tailed t-test
# stadare error for the slope: SE = sqrt [ sum((y_observed_i - y_estaimate_i)^2) / (n - 2) ] / sqrt [ sum((x_observed_i - x_avg)^2) ]
findPValueForRegressionSlope_TwoTailed_tTest = function(claimedSlope, sampleSlope, sampleStd, numSamples) {
  # degrees of freedom
  df = numSamples - 1
  # t-value
  tValue = (sampleSlope - claimedSlope)/sampleStd
  # the cumulative probability for the t value based on student t distribution
  cumulativeProbability = pt(tValue, df)
  # the p-value with two tails
  if (cumulativeProbability < 0.5 ) {
    pValue = 2*cumulativeProbability
  }
  else {
    pValue = 2*(1-cumulativeProbability)
  }
  return(pValue)
}

# hypothesis test for h0: The slope of the regression line is equal to zero
pValue = findPValueForRegressionSlope_TwoTailed_tTest(0, 0.55, 0.24, 101)
significanceLevel = 0.05
cat("\n\n")
cat("Problem 13: Hypothesis Test for Regression Slope with Two-Tailed t-Test")
cat("\nP-value is", pValue, ", level of significance is", significanceLevel, ".\n")
if (pValue < significanceLevel) {
  cat("The null hypothesis cannot be accepted.")
}
if (pValue >= significanceLevel) {
  cat("The null hypothesis cannot be rejected.")
}











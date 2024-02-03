###Parametric Tests###
#   Parametric test is a kind of the hypothesis test which gives generalizations for generating
#   records regarding the mean of the primary/original population.This is often the assumption
#   that the population data are normally distributed.

###t-test###
#   The sample size should be less than 30.
#   Comparing the mean of the 2 samples.
#   t-test is difference between sample mean and population mean.

###One Sample t-test###
#   We use one sample t-test to determine whether or not the sample mean is statistically
#   different from population mean or in other words, whether the sample comes from a population
#   with specific mean or not.

#Things to remember :
#  Before running the test there are some assumptions that has to be met for the one sample
# t-test.First observation has to be independent which other they shouldn't have any affect on each
# other and also the sample should be collected randomly and finally the distribution of 
# samples should have approximately normal distribution or the sample size should be large
# enough.We can use this formula to calculate t-test 

#Formula : t =  x-bar - μ/ S/√n


#load the dataset
Data = read.csv(file.choose(), header = T)

#check 10 first lines of data
head(Data, 10)

#Problem Statement:
#The average IQ in this sample is statistically higher than the average IQ level in the 
#population which was 95 for this particular sample.Is it greater than the sample mean or not?

#Sample mean
mean(Data$IQ)

#extract IQ from dataset
IQ = Data$IQ

#Hypothesis test 
#H0 : no difference between sample mean and population mean.
#H1 : sample mean > 95

#Visualize the data
#so i can add the mean of this data to this plot so using the abline mean of IQ i can add
#the horizontal line with the value of the mean of IQ and the color of blue and the line
#width of 3 so this blue line is the sample mean which was 101 and this black line is median
#which divides the data 50% of the data below 50% of the data above.So you want to see whether
#these two lines the difference between the population mean and the sample mean is this a
#statistically significant different or not.
#two lines 
boxplot(IQ , ylab = "IQ", las = 2)
abline(h = 95, col = "red", lwd = 3)
abline(h = mean(IQ), col = "blue", lwd = 3)

#Two methods to solve this problem
#1.Table method or using the formula
#2.The p-value technique

###Method1 : Table
#By putting values in the formula : t =  x-bar - μ/ S/√n
T1 = (mean(IQ) - 95)/(sd(IQ)/sqrt(20))
T1

#to find a critical value in the t-table for one tail test you need to know the degree of
#freedom and confidence interval

#degree of freedom is n - 1
20 - 1

#degree of freemdom is 19 and we're going to do this test and 95% CI so significance level
#will be 1 - 0.95 which is 0.05 so the intersection of these two gives us the critical
#value which is 1.729 and our t-test is 2.03 which shows that t-test value is greater than 
#the critical value which means that it falls in the critical region and it rejects the
#null hypothesis so it rejects that there is no difference between sample mean and population
#mean so we can accept that the sample mean and population mean is greater than 95 so the i
#interpretation is we can conclude that we've enough evidence that the individuals in this
#group have higher IQ level than the national average and it is statistically significant

#interpretation
#t-test (2.03) > critical value (1.73)

###Method 2: p-value technique

#so the alternative hypothesis is for this case is only greater than so the alternative
#hypothesis is greater so if it was a left tail we should use less if it was a two sided
#test we should use two.sided 

t.test(IQ, alternative = "greater", mu = 95, conf.level = 0.95)
------------------------------------------------------------------------------------------



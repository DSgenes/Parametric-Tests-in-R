###Paired t-test###
#  The paired t-test can be used when you've two dependent or matched or related samples like
#  before and after studies or pre-test, post-test studies.

# Formula : d bar/ s/√n

#Example1.
#The amount of lactic acid in the blood was examined for 10 men, before and after a strenous
#exercise, with the results in the following table 
#(a)Test if exercise changes the level of lactic acid in blood.Use significance level α = 0.01.
#(b)Find a 95% CI for the mean change in the blood lactose level.

###Paired t-test of lactic acid###

#load the dataset
Data = read.csv(file.choose(), header = T)

##check the data 
head(Data)

#Visualize the data
boxplot(Data$Before, Data$After, col = c("red", "blue"))

#Hypothesis Testing
#H0 : difference of mean of lactic acid = 0
#H1 : difference of mean of lactic acid ≠ 0

#In other words:
#H0 : μd = 0 (There's no improvement)
#Ha : μd < 0 (sometimes it can be less than sometimes can be greater than 0 sometimes it can
#              be not)

#Method1: t-Table & Formula

#According to the formula you've to measure the difference and then standard deviation of
#it and then divided by a square root of sample size 
diff = Data$After - Data$Before

T1 = mean(diff)/(sd(diff)/sqrt(10))

#interpretation at the 95% CI
#t-test (4.93) > 1.83 --> reject the null hypothesis

#so the difference of the mean of lactic acid before and after of the traning is not 0 which
#means that there was improvement from a statistical viewpoint at the population level so
#and if i want to test this at 99% Confidence level that one will be alpha will be 0.01
#degree of freedom is n-1 which is 9 so it's 2.82 again it rejects the null hypothesis
#which means that we're 99% confident that the difference is statistically significant

#Method2. p-value Technique
#interpretation at the 99% CI
#t-test (4.93) > 2.82

t.test(Data$After, Data$Before, 
       alternative = "two.sided",
       mu = 0, paired = T, var.equal = FALSE, 
       conf.level = 0.99)
-----------------------------------------------------------------------------------------
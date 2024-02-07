###Analysis of Variance (ANOVA)###
#  ANOVA is an extension to t-test and is used when you'd like to compare the means of two or
#  more populations so notices that this is an inferential statistics because based on the sample 
#  data we want to make inferences about the population.

#Hypothesis Testing :
#H0 : The means of all the (population) groups are equal.
#H1 : Atleast one mean is different.

#In notational form :
#The null hypothesis in the ANOVA states
#        H0 : μ1 = μ2 = ... = μk
#        H1 : At least one mean is different.

###Assumptions###
#1.Data is randomly collected.
#2.They're independent observations.
#3.They've normality assumptions that each group has approximately normal distribution.
#4.Equality of variance that variance of each group is equal to the others.

#Note : Assumptions 2 and 3 can be relaxed when large samples are used.

###Analysis of Variance for plants weights###
#load built-in dataset
data("PlantGrowth")

#explore data
head(PlantGrowth)
summary(PlantGrowth)

#levels for group
levels(PlantGrowth$group)

#Extract Variables
weight = PlantGrowth$weight
group = PlantGrowth$group

#Compare means of weights for each category
tapply(weight, group, mean)

#check the number of observations for each category
tapply(weight, group, length)

###Data Visualization###
boxplot(weight~group, main = "Plant vs Weight",
        xlab = "Plant Group",
        col = rainbow(3))

#run ANOVA test
aov(weight~group)

###Measure the F-statistics###
#Using this sum square of the group divided by the degree of freedom and then sum score of
#the groups divided by the degree of freedom 

(3.76634/2)/(10.49209/27)

#using this f-statistics we can make decision whether we can reject null hypothesis or we
#fail to reject null hypothesis if i save the result of ANOVA test in an object so you can
#see it gives you the F value and F-statistics 
Results = aov(weight~group)
Results
summary(Results)

#so here the probability or p-value is less than 0.05 which shows that this test is significant
#and is not due to chance so we can conclude that there is enough evidence that atleast one
#mean is different from other means at the population level

#ANOVA cannot tell us which two groups are different we need to run post-hoc test and post-hoc
#test is Tukey's HSD test 

###Post-hoc test###
TukeyHSD(Results)

#so you can see that p-value adjusted of group trt2-trt1 is less than 0.05 so this group is
#significantly different from each other at 95% confidence level

#to see the results at 99% confidence level
TukeyHSD(Results, conf.level = 0.99)

#still these two treatments treatment 1 and treatment 2 are significant different from each
#other at 99% confidence level

#equality of variance
bartlett.test(weight~group)

#so the null hypothesis in Bartlett's test states that all of the variances are equal and 
#if we here the p-value is not less than 0.05 so we fail to reject the null hypothesis
#in other words all of these samples have equal variance which is one assumptions of the
#ANOVA test.
------------------------------------------------------------------------------------------
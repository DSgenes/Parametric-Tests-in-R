###Unpaired (Independent) t-test###
#   The unpaired or independent two sample t-test is used to compare the means of two independent
#   groups so we want to determine if two population means are equal or not or in other words
#   if the two groups come from the same population or not.Both groups have approximately normal
#   distribution.
#   
###Two Sample t-test###
#   It can be broken down into two categories:
#   1.Independent or unpaired t-test
#   2.Dependent or paired t-test

###Unpaired t-test(Equal Variance) Formula:
#   t = (x bar - x2 bar)/Sp√(1/n) + (1/n2)
#   Sp = √(n1-1).S1square + (n2 - 1).S2square/(n1+n2-2)
#   df = n1 + n2 - 2

#Unpaired t-test (Unequal Variance)
#   t- = (x1 bar - x2 bar)/√(S1square/n1) + (S2square/n2)
#   d.f = [(S1square/n1) + (S2square/n2)]square/(S1square/n1)square/n1-1 + (S2square/n2)square/n2-1

#Two sample unpaired t-test 
#load the dataset
data = read.csv(file.choose(), header = T)

#check the data
head(data, 10)
names(data)

Gender = data$GENDER
Weight = data$BIRTH.WEIGHT

#Visualize the data
boxplot(Weight~Gender, col = c("blue", "red"))

#male and female mean Weight
mean(Weight)

#male and female mean Weight
weight.female = mean(Weight[Gender=="Female"])
weight.male = mean(Weight[Gender=="Male"])

weight.female
weight.male

#here you can see that weight of male is greater than the weight of female 

#H0 : difference of weight = 0
#H1 : difference of weight ≠ 0

t.test(Weight[Gender=="Male"], Weight[Gender=="Female"],
       alternative = "two.sided",
       mu = 0, paired = FALSE, var.equal = FALSE, 
       conf.level = 0.95)

#so here you can see that the p-value is too small it is less than 0.05 so we reject the
#null hypothesis and accept the alternate hypothesis which means we're 95% confident that
#the difference of weight of male and female at the population level is not 0 so they are
#different so the birth-weight of male and the birth-weight of female is different

#Check equality of variance
var.test(Weight[Gender=="Female"], Weight[Gender=="Male"])

#so that uses F-test so F-test is used to compare two variances


t.test(Weight[Gender=="Male"], Weight[Gender=="Female"],
       alternative = "two.sided",
       mu = 0, paired = FALSE, var.equal = T, 
       conf.level = 0.95)
-----------------------------------------------------------------------------------------
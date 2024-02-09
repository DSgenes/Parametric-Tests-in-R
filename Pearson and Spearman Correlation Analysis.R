
####Correaltion Analysis###
#     - To examine the association between two variables and to identify the direction
#       and strength of association
#Types - 1. Pearson Correlation (Values are on continuous scale)
#        2. Spearman Rank Correlation (data can be even ordinal)

install.packages('ggpubr')
library(ggpubr)

#load the data
data = read.csv(file.choose(), header=T)

#normality check
#We want to see is there any linear association between these two continuous variables
#Does these two continuous variables follow normal distribution or not
ggqqplot(data$WAIST)

ggqqplot(data$ARM.CIRC)

#We also use shapiro test if th p-value is above 0.05 it means to accept the null hypothesis 
shapiro.test(data$WAIST)

#H0 : no correlation between waist and arm
#H1 : correlation between waist and arm circumference

ggscatter(data, x='ARM.CIRC', y='WAIST')

ggscatter(data, x='ARM.CIRC', y='WAIST',
          add = 'reg.line',
          conf.inf=T)

#Pearson Correlation
#1.The values close to +1 shows strong positive correlation
#      (if x increases y also increases)
#2.The values close to -1 shows negative correlation
#      (if x decreases y also decreases)
#3.The values close to 0 shows weak or no correlation


#Pearson Test

cor.test(data$WAIST, data$ARM.CIRC, method = 'pearson')

#p-value is very very small which means that it rejects H0 that there's no coorelation between 
#waist and size so we accept H1 that there's a coorelation between waist and size
#the value for the sample coorelation is almost 90% and at 95% confidence interval the correlation
#is between 82% and 94% so the value is very close to 1

ggscatter(data, x='ARM.CIRC', y='WAIST',
          add='reg.line',
          conf.int = T,
          cor.method = 'pearson',
          cor.coef = T)
------------------------------------------------------------------------------------------------------
###Spearman Rank Correlation###

# Measures the statistical association between the ranks of paired observations for two variables.
# Data can be even ordinal.
# Monotonic relationship between variables. Monotonic means as the value of one variable increases
# the value of the other variable never decreases or as the value of one variable decreases the value
# of the other variable never increases.
  
data('cars')

#check normality
ggqqplot(cars$dist)

shapiro.test(cars$dist)

ggscatter(data=cars, x='speed', y='dist')  

ggscatter(data=cars, x='speed', y='dist',
          add='reg.line')

#H0: no correlation between ranked speed and ranked distance.
#H1: Significant correlation between ranked speed and ranked distance.

cor.test(cars$speed, cars$dist, method = 'spearman', exact=F)

#the p-value is very very small so it rejects the H0 and accepts the H1 that there's
#a significant correlation between ranked speed and ranked distance.

ggscatter(data = cars, x='speed', y='dist',
          add='reg.line',
          cor.method = 'spearman',
          conf.int = T,
          cor.coef = T)
------------------------------------------------------------------------------------------------------
###Non-Parametric Tests###
#   Non-parametric tests are also called distribution free test and the data doesn't have to
# follow normal distribution or any other distribution.

#When do we use non-parametirc tests?
#1.Non-normal data
#2.Small sample size
#3.Presence of outliers
#4.Nominal or ordinal data

#Main advantage :
#  Useful for a wide range of data types and applications

#Main disadvantange :
#  Less powerful compared to parametric tests

#The alternate versions used for non-parametric tests:
#Parametric Tests             Non-Parametric
#1 Sample t-test          Sign Test/Wilcoxon Signed Rank test
#Paired t-test            Sign Test/Wilcoxon Signed Rank test
#2 Sample t-test          Mann Whitney Utest/Wilcoxon Sum Rank test
#One way ANOVA            Kruskal Wallis test
#Pearson Correlation      Spearman rank correlation
----------------------------------------------------------------------------------------
#To practice non-parametric tests
#install the ggpubr
install.packages('ggpubr')
library(ggpubr)

#1 Sample np test
#load the built-in data
data('CO2')

#Check normality
ggqqplot(CO2$uptake)
#run shapiro test
#if the p-value less than 0.05 it means that the distribution is not normal
shapiro.test(CO2$uptake)

#The median CO2 uptake for these places for Mississippi and Quebec
#is less than 30 at the population level.

#H0 : median uptake is 30
#H1 : median uptake is < 30
wilcox.test(CO2$uptake, mu = 30, alternative = "less")

###2 Sample unpaired np test###
boxplot(CO2$uptake~CO2$Type)

#to check the normal distribution for Quebec is record number 1 to 42
Q = CO2[1:42,]
ggqqplot(Q$uptake)

#run shapiro test
shapiro.test(Q$uptake)

#H0 : median CO2 (M=Q)
#H1 : median CO2 (M≠Q)

#run wilcox test
wilcox.test(CO2$uptake~CO2$Type, paired = F)
#so the median for this two locations are significantly different from
#each other to calculate the exact p-value
wilcox.test(CO2$uptake~CO2$Type, paired = F, exact = F)

###ANOVA np test###
boxplot(CO2$uptake ~ CO2$Plant)

#H0 : medians CO2 uptake are similar
#H1 : some medians CO2 uptakes are different

kruskal.test(CO2$uptake ~ CO2$Plant)

#we need to run an extra test to see which pairs are different
pairwise.wilcox.test(CO2$uptake, CO2$Plant, p.adjust.method = 'BH')

###Paired np test###
#load the data
data = read.csv(file.choose(), header = T)

#H0 : median difference in pains is 0
#H1 : median difference in pains ≠ 0

wilcox.test(data$Before, data$After, paired = T)
-----------------------------------------------------------------------------------------
###Chi-squared Independent Test###
#    Chi-square independent test is to investigate the association between two categorical 
#   variable.Is there any association between two categorical variable or not?

#Formula : Χsquare = Σ (Oi - Ei)square/Ei

#                   Oi : Observed value (Actual data)
#                   Ei : Expected Value
#                   Ei = row total * column total/Sample size 

###Assumptions of chi-square###
#  Random samples
#  Mutually exclusive categories
#  Independent observations
#  Expected frequency count for each category of the variable is at least 5.

#H0 and H1:

#The statistical null hypothesis is :
#H0 : No associations between variable A and variable B.They are independent/No effect.

#Alternative hypothesis is :
#H1 : Variable A and variable B are associated.They are dependent.
-----------------------------------------------------------------------------------------
#Example.
#A public opinion poll surveyed a simple random sample of 1000 voters.Respondents were classified
#by gender(male or female) and by voting preference (Republican, Democrat, or independent).Is
#there any significant association between gender and voting preference(at 95% conf.level)?

#Hypothesis Testing:
#H0 : Gender and voting preference are independent (no impact)
#H1 : Gender and voting preference are dependent.

#Association between gender and voting

#load the data
data = read.csv(file.choose(), header = T)

#to check levels
data$Gender = as.factor(data$Gender)
levels(data$Gender)

data$Voting.Preference = as.factor(data$Voting.Preference)
levels(data$Voting.Preference)

#Create contingency table
table1 = table(data$Gender, data$Voting.Preference)
table1

#Plot the data using barplot for categorical variables
#Stacked Barplot
barplot(table1, legend.text = T, col = c("blue", 
                                         "red"))

#Side by side barplot
barplot(table1, legend.text = T, col = c("blue", 
                                         "red"), beside = T)
#H0 : Gender and voting are independent
#H1 : Gender and voting are dependent

#run the chi-square test
#the p-value is very very small so we reject the null hypothesis that gender and voting 
#are independent and accept the alternate hypothesis that gender and voting are dependent

chisq.test(table1)

#now look at some characteristics of the result for expected values
results = chisq.test(table1)
results$expected

#so if you look at the expected values all of them are above 5 so there was one of the
#assumptions of the chi-square test but what if one of the cell count for expected value
#one of the cell count is less than 5 so in that case we've to use fisher's exact test
-----------------------------------------------------------------------------------------
###Fisher's exact Test###
#  So fisher's test is useful when we've small sample size especially when one of the cells
# or few of the cells they've less than 5 counts for the expected values.
  
#Assumptions for fisher's exact test :
# Sample size should be very small
# Cell count for expected value should be less than 5
  
#Hypothesis Testing
#H0 : Smoking and status are independent
#H1 : Smoking and status are dependent

#Create a dataframe for 15 observations
data1 = data.frame(
   ID = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15),
   Smoking = c("Smoker", "Smoker", "Non-Smoker", "Non-Smoker","Non-Smoker","Non-Smoker",
               "Non-Smoker","Non-Smoker","Non-Smoker","Smoker","Smoker","Smoker","Smoker",
               "Smoker","Non-Smoker"),
   Status = c("Athelete", "Athelete", "Athelete", "Athelete", "Athelete", "Athelete", 
             "Athelete", "Athelete", "Athelete", "Athelete", "Non-Athelete", "Non-Athelete",
             "Non-Athelete","Non-Athelete","Non-Athelete"))
  
#check levels
data1$Smoking = as.factor(data1$Smoking)
levels(data1$Smoking)

data1$Status = as.factor(data1$Status)
levels(data1$Status)

#Create contingency table
table2 = table(data1$Smoking, data1$Status)
table2

#run fisher exact test
fisher.test(table2)

#so the p-value is smaller than 0.05 so we can reject the null hypothesis that smoking and
#status are independent from each other which means they are dependent to each other 
----------------------------------------------------------------------------------------
###McNemar's test###
#  In mcnemar's test the sample is collected randomly they've the equal chance of being
# selected.They're dependent observations.What makes it different from the paired t-test
# is we've the categorical variable.It's only based on when you've two categories or mutually
# exclusive.  

#Assumptions :
# Dependent observations
# Mutually exclusive categories(only 2 categories)

#Example: McNemar's test
#A researcher wants to investigate the impact of an intervention on smoking.In this hypothetical
#study, 50 participants were recruited to take part, consisting of 25 smokers and 25 
#non-smokers.All participants watched on emotive video showing the impact that deaths from
#smoking-related cancers had on families.Two weeks after this video intervention, the same
#participants were asked whether they remained smokers or non-smokers.

#Create the dataset
data2 = data.frame(
        Before = c("Non-smoker", "Non-smoker", "Smoker", "Smoker"),
        After = c("Non-smoker", "Smoker", "Non-smoker", "Smoker"),
        Count = c(20, 3, 18, 9))
data2

#check the levels
data2$Before = as.factor(data2$Before)
data2$Before

data2$After = as.factor(data2$After)
data2$After

#Create contingency table
table3 = table(data2$Before, data2$After)
table3

#run McNemar's test
mcnemar.test(table3)
-----------------------------------------------------------------------------------------
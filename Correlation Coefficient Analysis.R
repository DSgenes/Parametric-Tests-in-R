
###Correlation Analysis###
#   An important measure for the linear association between two quantitative 
#   variables.

#Calculating correlation coefficient of two equally long numeric vectors x 
#and y.If x and y contains missing values, we can remove them from the 
#calculation.

#load the data
bm = read.csv(file.choose(), header=T)

head(bm) #to check the first six lines of data

#Scatter plot matrix for numeric variables

plot(upper_arm_length ~ height,
     data = bm,
     main = "Upper Arm Length vs. Body Height of Women (Age 20-25)",
     xlab = "Body height (cm)",
     ylab = "Upper arm length (cm)")

#Exclude missing values

#A correlation coefficient of 1 implies that all data points fall exactly on
#a straight line with positive slope.

cor(bm$height, bm$upper_arm_length)

cor(bm$height, bm$upper_arm_length, use = "complete.obs")

#Generating scatterplot for all numeric columns

#Through this we can quickly determine which pairs of variables are roughly
#linearly associated.

plot(bm, main = "Body Measures in 2017-2018 NHANES")

#Calculating correlation coefficient for each pair of columns

cor(bm)

#Rows contain null values
bm[52, ]

bm[32, ]

#Using 'complete.obs' for removing a row

cor(bm, use = "complete.obs")

#'pairwise.complete.obs' will only ignore pair of variables that 
#involve NA

cor(bm, use = "pairwise.complete.obs")

cor(bm$height, bm$upper_arm_length, use = "complete.obs")

cor(bm$height, bm$upper_arm_length, use = "pairwise.complete.obs")
----------------------------------------------------------------------------

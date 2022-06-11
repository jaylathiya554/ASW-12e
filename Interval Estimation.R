# ------------------------------Interval Estimation----------------------------

### Point Estimation of Population mean

# In this we need to sample mean and use it
# as an estimate of the corresponding population parameter.

data = read.csv(file.choose(),header = T)
View(data)
maths = data$Maths
phy = data$Physics
chem = data$Chemistry
percentage = data$Percentage
height = data$Height
weight = data$Weight
mean(maths)
mean(phy)
mean(chem)
mean(percentage)
mean(height)
mean(weight)
chem1 = c(chem,NA)
mean(chem1)

# Why we can not find mean of Height an Weight?

library(MASS)
mean(height,na.rm = T)   # skip missing values
mean(weight,na.rm = T)


### Interval Estimation of population Mean: Sigma Known

# After we found a point estimate of population mean, 
# we would need a way to quantify its accuracy.

# Assume the population standard deviation sigma of the student height in 
# survey is 8.5 Find the margin of error and interval estimate at
# 95% confidence level.

height
h = na.omit(height)
h

# compute the standard error of the mean.

n=length(h)
n
sigma = 8.5  # Population Standard Deviation
stderr = sigma/sqrt(n)  # Standard Error of Mean
stderr

# Since there are two tails of the normal distribution, the 95% confidence level
# would imply the 97 5th percentile of the normal distribution at the
# upper tail. Therefore, z(alpha/2) is given by qnorm(.975). we multiply it with
# the standard error of the mean sem and get the margin of error.

z=qnorm(0.975)
z1=qnorm(0.025)
z1
z
E = z*stderr # margin of error
E
E1 = z1*stderr # margin of error
E1

# Confidence Interval

xbar = mean(h)
xbar+c(-E,E)


# Alternative solution:

install.packages("TeachingDemos")
library("TeachingDemos")

z.test(h,sd=sigma)


### Interval Estimation of population Mean: Sigma Unknown

# After we found a point estimate of the population mean,
# we would need a way to quantify its accuracy. Here,
# we discuss the case where the population variance is not assumed.


# without assuming the population standard deviation of the student height 
# in survey, find the margin of error and interval estimate at
# 95% confidence level.


height
h = na.omit(height)
h

# compute the standard error of the mean.

n = length(h)
n
s = sd(h) # Sample Standard Deviation
SE = s/sqrt(n)  # Stranded Error of Mean
SE

# Since there are two tails of the student t distribution,
# the 95% Confidence level would imply the 97.5th percentile of
# the student t distribution at the upper tail. Therefore, t??/2
# is given by qt(.975, df=n-1). we multiply it with the
# standard error estimate SE and get the margin of error.

t=qt(0.975, df=n-1)
E=t*SE #Margin of Error
E

# Confidence Interval

xbar = mean(h)  # sample mean
xbar+c(-E,E)

# Alternative solution:

t.test(h)


### Sampling size of population mean

# The quality of a sample survey can be improved by increasing the sample size.

# Assume the population standard deviation ?? of the student height in 
# survey is 8.5 Find the sample size needed to achieve a 
# 1.2 centimeters margin of error at 95% confidence level.

# Since there are two tails of the normal distribution, the 95% confidence 
# level would imply the 9\.5th percentile of the normal distribution at
# the upper tail. Therefore, z?????? 2 is given by qnorm(.975).

z= qnorm(.975)
sigma= 8.5
E = 1.2
n = z^2*sigma/E^2
n

#_______________________________________________________________________________

### Point Estimate of Population Proportion 

# Multiple choice questionnaires in a survey are often used ti determine 
# the population of a population with certain characteristic.
# for example, we can estimate the population of female students in the
# university based on the result in the sample data set survey.

# Find a point estimate of the female students proportion from survey.

gender = data$Gender
g= na.omit(gender)
g
n = length(g)
n
k= sum(g=="Female")
k
pbar = k/n
pbar



### Interval Estimation of population proportion

# After we found a point sample estimate of the population proportion,
# we would need to estimate its confidence interval.


# Compute the margin of error and estimate interval for the female students 
# population in survey at 95% confidence level.

gender = data$Gender
g= na.omit(gender)
g
n = length(g)
n
k= sum(g=="Female")
k
pbar = k/n
pbar

# Estimate Standard Error

SE = sqrt((pbar*(1-pbar))/n)
SE

# Since there are two tails of the normal distribution,
# the 95% confidence level would the 97.5th percentile of
# the normal distribution at the upper tail. Therefore, z?????? 2 is given by
# qnorm(.975). Hence we multiply with the Standard error estimate 
# SE and compute the margin of error.

E = qnorm(0.975)*SE
E

# Confidence Interval

pbar+c(-E,E)

# Alternative Solution

prop.test(k,n)


### Sample size of population proportion

# The quality of a sample survey can be improved by increasing the sample size.

# Using a 50% planned proportion estimate, find the sample size needed to
# achieve 5% margin of error for the female student survey at
# 95% confidence level.

# Since there are two tails of the normal distribution,
# the 95% confidence level would imply the 97.5th percentile of
# the normal distribution at upper tail. Therefore, z?????? 2 
# is given by qnorm(.975).

z = qnorm(.975)
p = 0.5
E = 0.05
n = ((z^2)*p*(1-0))/E^2
n

1-0.90
1-0.1/2




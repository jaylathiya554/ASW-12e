#Lower Tail of population Mean with Known Variance------------------

# The null hypothesis of the lower tail test of he population mean can be 
# expressed as follows: U >= U0
# where U0 is a hypothesized lower bound of the true population mean U.

# Suppose the manufacturer claims that the mean lifetime of a light bulb is more
# the 10,000 hours. In a sample of 30 light bulbs, it was found the they only 
# last 9900 hours on average.
# Assume the population standard deviation is 120 hours.
# At 0.05 significance level, can we reject the claim by the manufacturer?

# Null hypothesis: U>= 10000

xbar = 9900 # Sample Mean
mu0 = 10000 # hypothesized mean value
sigma = 120 # population standard deviation
n = 30 # Sample size
z = (xbar - mu0)/(sigma/sqrt(n))  # Test Statistic
z

# critical value at 0.05 significance level

alpha = 0.05
z.alpha = qnorm(1-alpha)
-z.alpha # critical value


# Alternative Solution

# Instead of using the critical value, we apply the pnorm function to compute 
# the lower tail p_value of the test statistic.
# As it runs out to be less than the 0.05 significance level, 
# we reject the null hypothesis that U>=1000.

pval = pnorm(z)
pval


# Another Solution

xabr = 9900 # Sample Mean
mu0 = 10000 # hypothesized value
sigma = 120 # population standard deviation
n = 30 # Sample size

sample = rnorm(30, mean=xbar, sd=sigma)
sample


# install.packages("TeachingDemos")
library("TeachingDemos")
z.test(sample, mu=9900, sd=sigma, alternative = "less")



#Upper Tail of population Mean with Known Variance-----------------


# The null hypothesis of the upper tail test of the population mean can be 
# expressed as follows: U <= U0
# where U0 is a hypothesized upper bound of the true population mean U.

# Suppose the food label on a cooking bag states that there is at most
# 2 grams of saturated fat in a signal cookie. In a sample of 35 cookies,
# it is found that the mean amount of saturated fat per cookie is 2.1 grams.
# Assume that population standard deviation is 0.25 grams.
# At 0.05significance level, can we reject the claim on food label?

# null hypothesis: mu<=2

xbar = 2.1 # Sample Mean
mu0 = 2 # hypothesized value
sigma = 0.25 # population standard deviation
n = 35 # Sample size
z = (xbar - mu0)/(sigma/sqrt(n))  # Test Statistic
z


# critical value at 0.05 significance level

alpha = 0.05
z.alpha = qnorm(1-alpha)
-z.alpha # critical value


# Alternative Solution

# Instead of using the critical value, we apply the pnorm function to compute 
# the lower tail p_value of the test statistic.
# As it runs out to be less than the 0.05 significance level, 
# we reject the null hypothesis that U<=2.

pval = pnorm(z, lower.tail = FALSE)
pval #upper tail p-value


# Another Solution

xabr = 2.1 # Sample Mean
mu0 = 2 # hypothesized value
sigma = 0.25 # population standard deviation
n = 35 # Sample size

sample = rnorm(35, mean=xbar, sd=sigma)
sample

z.test(sample, mu=mu0, sd=sigma, alternative = "greater")





#Two-Tailed Tail Test of population Mean with Known Variance------------

### Upper Tail of population Mean with Known Variance

# The null hypothesis of the upper tail test of the population mean can be 
# expressed as follows: U = U0
# where U0 is a hypothesized upper bound of the true population mean U.

# Suppose the mean weight of the king penguins found in an Antarctic colony
# last year was 15.4 kg. In a sample of 35 penguins same time this year
# in the same colony, the mean penguin weight is 14.6kg.
# At 0.05 significance level, can we reject the null hypothesis that the mean
# penguin weight does not differ from last year?

xbar = 14.6 # Sample Mean
mu0 = 15.4 # hypothesized value
sigma = 2.5 # population standard deviation
n = 35 # Sample size
z = (xbar - mu0)/(sigma/sqrt(n))  # Test Statistic
z

# critical value at 0.05 significance level

alpha = 0.05
z.alpha = qnorm(1-alpha/2)
-z.alpha # critical value


# Alternative Solution

# Instead of using the critical value, we apply the pnorm function to compute 
# the lower tail p_value of the test statistic.
# As it runs out to be less than the 0.05 significance level, 
# we reject the null hypothesis that U<=2.

pval = 2 * pnorm(z) # lower tail
pval #upper tail p-value


# Another Solution

xabr = 14.6 # Sample Mean
mu0 = 15.4 # hypothesized value
sigma = 2.5 # population standard deviation
n = 35 # Sample size

sample = rnorm(35, mean=xbar, sd=sigma)
sample

z.test(sample, mu=mu0, sd=sigma)





#Lower Tail of population Mean :sigma Unknown----------------


# The null hypothesis of the lower tail test of he population mean can be 
# expressed as follows: U >= U0
# where U0 is a hypothesized lower bound of the true population mean U.

# Suppose the manufacturer claims that the mean lifetime of a light bulb is more
# the 10,000 hours. In a sample of 30 light bulbs, it was found the they only 
# last 9900 hours on average.
# Assume the population standard deviation is 125 hours.
# At 0.05 significance level, can we reject the claim by the manufacturer?

# Null hypothesis: U>= 10000

xbar = 9900 # Sample Mean
mu0 = 10000 # hypothesized value
s = 125 # population standard deviation
n = 30 # Sample size
t = (xbar - mu0)/(s/sqrt(n))  # Test Statistic
t

# critical value at 0.05 significance level

alpha = 0.05
t.alpha = qt(1-alpha, df=n-1)
-t.alpha # critical value


# Alternative Solution

# Instead of using the critical value, we apply the pnorm function to compute 
# the lower tail p_value of the test statistic.
# As it runs out to be less than the 0.05 significance level, 
# we reject the null hypothesis that U>=1000.

pval = pt(t, df=n-1)
pval


# Another Solution

xabr = 9900 # Sample Mean
mu0 = 10000 # hypothesized value
s = 125 # population standard deviation
n = 30 # Sample size

sample = rt(30, df=n-1)
sample

t.test(sample, mu=9900, sd=sigma, alternative = "less")




#Upper Tail of population Mean :sigma Unknown----------------

# The null hypothesis of the upper tail test of the population mean can be 
# expressed as follows: U <= U0
# where U0 is a hypothesized upper bound of the true population mean U.

# Suppose the food label on a cooking bag states that there is at most
# 2 grams of saturated fat in a signal cookie. In a sample of 35 cookies,
# it is found that the mean amount of saturated fat per cookie is 2.1 grams.
# Assume that population standard deviation is 0.3 grams.
# At 0.05significance level, can we reject the claim on food label?

# null hypothesis: U<=2

xbar = 2.1 # Sample Mean
mu0 = 2 # hypothesized value
s = 0.3 # population standard deviation
n = 35 # Sample size
t = (xbar - mu0)/(s/sqrt(n))  # Test Statistic
t


# critical value at 0.05 significance level

alpha = 0.05
t.alpha = qt(1-alpha, df=n-1)
-t.alpha # critical value


# Alternative Solution

# Instead of using the critical value, we apply the pnorm function to compute 
# the lower tail p_value of the test statistic.
# As it runs out to be less than the 0.05 significance level, 
# we reject the null hypothesis that U<=2.

pval = pt(t,df=n-1, lower.tail = FALSE)
pval #upper tail p-value


# Another Solution

xabr = 2.1 # Sample Mean
mu0 = 2 # hypothesized value
sigma = 0.25 # population standard deviation
n = 35 # Sample size


#t.test(xbar,muo=10000,sd=sigma, alternative= "less")
# t test need whole sample
sample = rt(35, df=n-1)
sample

t.test(sample, mu=mu0, sd=sigma, alternative = "greater")





#Two-Tailed Tail Test of population Mean : Sigma Unknown----------

# The null hypothesis of the upper tail test of the population mean can be 
# expressed as follows: U = U0
# where U0 is a hypothesized upper bound of the true population mean U.

# Suppose the mean weight of the king penguins found in an Antarctic colony
# last year was 15.4 kg. In a sample of 35 penguins same time this year
# in the same colony, the mean penguin weight is 14.6kg.
# At 0.05 significance level, can we reject the null hypothesis that the mean
# penguin weight does not differ from last year?

# null hypothesis: U = 15.4

xbar = 14.6 # Sample Mean
mu0 = 15.4 # hypothesized value
s = 2.5 # population standard deviation
n = 35 # Sample size
z = (xbar - mu0)/(s/sqrt(n))  # Test Statistic
z

# critical value at 0.05 significance level

alpha = 0.05
t.half.alpha = qnorm(1-alpha/2)
c(-t.half.alpha, t.half.alpha) # critical value


# Alternative Solution

# Instead of using the critical value, we apply the pt function to compute 
# the two-tailed p_value of the test statistic. It doubles the lower tail
# p-value as the sample mean is less than the hypothesized value.
# Since it turns out to be greater than the 0.5 significance level, 
# we reject the null hypothesis that U=15.4.


pval = 2 * pt(t, df=n-1) # lower tail
pval #upper tail p-value


# Another Solution

xabr = 14.6 # Sample Mean
mu0 = 15.4 # hypothesized value


#Lower Tail Test of population proportion---------------------------------------

# The null hypothesis of the lower tail test about population proportion can be 
# expressed as follows: P>=P_0
# where P_0 is a hypothesized lower bound of the true population proportion P.

# Suppose 60% of citizens voted in last election.85 out of 148 people.
# in a telephone survey side that they voted in current election
# At 0.5 significance of voters in the population is above 60% this year?

# null hypothesis: P>=0.6

pbar = 84/148 # sample proportion
p0=.6  # Hypothesized value
n=148  # Sample size
z= (pbar-p0)/sqrt(p0*(1-p0)/n)  # test statistic
z 


# Critical value at 0.05 significance level

alpha = 0.05
z.alpha = qnorm(alpha)  # Critical Value
z.alpha

# Alternative Solution

# Instead using the critical value, we apply the pnorm function to compute the 
# lower tail p-value of the test statistic.
# As it turns out to be grater than the 0.05 significance level,
# we do not reject the null hypothesis that p>=0.6

pval = pnorm(z)
pval


# Another solution

prop.test(x=85,n=148,p=0.6,alternative = "less", correct = FALSE)




#Upper Tail Test of population proportion------

#Lower Tail Test of population proportion
# The null hypothesis of the lower tail test about population proportion can be 
# expressed as follows: P>=P_0
# where P_0 is a hypothesized lower bound of the true population proportion P.
  
# Suppose 60% of citizens voted in last election.85 out of 148 people.
# in a telephone survey side that they voted in current election
# At 0.5 significance of voters in the population is above 60% this year?
  
# null hypothesis: P>=0.6
  
pbar = 84/148 # sample proportion
p0=.6  # Hypothesized value
n=148  # Sample size
z= (pbar-p0)/sqrt(p0*(1-p0)/n)  # test statistic
z 


# Critical value at 0.05 significance level

alpha = 0.05
z.alpha = qnorm(1-alpha)  # Critical Value
-z.alpha

# Alternative Solution

# Instead using the critical value, we apply the pnorm function to compute the 
# lower tail p-value of the test statistic.
# As it turns out to be grater than the 0.05 significance level,
# we do not reject the null hypothesis that p>=0.6

pval = pnorm(z)
pval


# Another solution

prop.test(x=85,n=148,p=0.6,alternative = "less", correct = FALSE)


# Upper Tail Test of population proportion-----

# The null hypothesis of the lower tail test about population proportion can be 
# expressed as follows: P<=P0
# where P0 is a hypothesized lower bound of the true population proportion P.

# Suppose that 12% of apples harvested in an orchard last year was rotten.
# 30 out of 214 apples on a harvest sample this year turns out to be rotten.
# At 0.05 significance level, can we reject the null hypothesis that the
#  proportion of rotten apples in harvest stays below 12% this year?

# null hypothesis: p<=0.12

pbar = 30/214 # sample proportion
p0=.12  # Hypothesized value
n=214  # Sample size
z= (pbar-p0)/sqrt(p0*(1-p0)/n)  # test statistic
z 

# Critical value at 0.05 significance level

alpha = 0.05
z.alpha = qnorm(1-alpha)  # Critical Value
z.alpha

# Alternative Solution

# Instead using the critical value, we apply the pnorm function to compute the 
# lower tail p-value of the test statistic.
# As it turns out to be grater than the 0.05 significance level,
# we do not reject the null hypothesis that p<=0.12

pval = pnorm(z, lower.tail =  FALSE)
pval

# Another solution

prop.test(30,214,p=0.12,alternative = "greater", correct = FALSE)


# Two Tailed Test of population proportion-------

# The null hypothesis of the lower tail test about population proportion can be 
# expressed as follows: P<=P0
# where P0 is a hypothesized lower bound of the true population proportion P.

# Suppose a coin toss turns up 12 heads out of 20 trials.
# At 0.05 significance level, can one reject the null hypothesis that the coin
# toss is fair?

# null hypothesis: p=0.5

pbar = 12/20 # sample proportion
p0=0.05  # Hypothesized value
n=20  # Sample size
z= (pbar-p0)/sqrt(p0*(1-p0)/n)  # test statistic
z 

# Critical value at 0.05 significance level

alpha = 0.05
z.half.alpha = qnorm(z-alpha/2)
c(-z.half.alpha ,z.half.alpha)

# Alternative Solution

# Instead using the critical value, we apply the pnorm function to compute the 
# lower tail p-value of the test statistic.
# It doubles the upper tail p-value as the sample proportion is grater than the 
# hypothesized value.
# Since it turns out to be greater than the .05 significance level,
# we do not reject the null hypothesis that p=0.5.

pval = 2 * pnorm(z,lower.tail = FALSE)  # upper tail
pval  # two-tailed p-value

# Another Solution

prop.test(12,20,p=0.5, correct = FALSE)

# we apply the prop.test function to compute the p-value directly.
# The Yates continuity correction is disables for pedagogical reasons.




# Z - sigma konwn
# T - sigma unkown

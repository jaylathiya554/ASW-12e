#Population Mean: ??sigma Unknown----
#Q11----

#11. For a t distribution with 16 degrees of freedom, find the area, or probability, in each region.
#a. To the right of 2.120
#b. To the left of 1.337
#c. To the left of 1.746
#d. To the right of 2.583
#e. Between -2.120 and 2.120
#f. Between -1.746 and 1.746

#Q12-----

#12. Find the t value(s) for each of the following cases.
#a. Upper tail area of .025 with 12 degrees of freedom
#b. Lower tail area of .05 with 50 degrees of freedom
#c. Upper tail area of .01 with 30 degrees of freedom
#d. Where 90% of the area falls between these two t values with 25 degrees of freedom
#e. Where 95% of the area falls between these two t values with 45 degrees of freedom

install.packages("TeachingDemos")
library("TeachingDemos")

#(a)
ta = qt(1 - 0.025,df = 12)
ta

#(b)
tb = qt(1- 0.05, df = 50, lower.tail = F)
tb

#(c)
tc = qt(1- 0.01, df = 30)
tc

#(d)
td = qt(1 - (1-0.90)/2, df = 25)
td
c(-td,td)

#(e)
te = qt(1 - (1-0.95)/2, df = 45)
te
c(-te,te)


#Q13----

#13. The following sample data are from a normal population: 10, 8, 12, 15, 13, 11, 6, 5.
#a. What is the point estimate of the population mean?
#b. What is the point estimate of the population standard deviation?
#c. With 95% confidence, what is the margin of error for the estimation of the population
#mean?
#d. What is the 95% confidence interval for the population mean?

#(a)
n=8
a= 10+8+12+15+13+11+6+5
a

xbar = a/n
xbar

#(b)
s = c((10-10),(8-10),(12-10),(15-10),(13-10),(11-10),(6-10),(5-10))
s

ss = (s)**2
ss
sd = sum(ss)
sd

sde = sqrt(sd / n)
sde

#(c)
z=qnorm(1-((1 - 0.95)/2))
z

E=z*sde/sqrt(n) #Margin of Error
E

xbar+c(-E,E)

#(d)



#Q15-----

#15. Sales personnel for Skillings Distributors submit weekly reports listing the customer contacts
#made during the week. A sample of 65 weekly reports showed a sample mean of 19.5
#customer contacts per week. The sample standard deviation was 5.2. Provide 90% and 95%
#confidence intervals for the population mean number of weekly customer contacts for the
#sales personnel.

#(a)
n = 65
xbar = 19.5
sigma = 5.2

t = qt(1 - (1-0.90)/2, df = 64)
t

xtn=xbar-t*(sigma/sqrt(n))
xtp=xbar+t*(sigma/sqrt(n))

c(xtn,xtp)

#(b)
t = qt(1 - (1-0.95)/2, df = 64)
t

xtn=xbar-t*(sigma/sqrt(n))
xtp=xbar+t*(sigma/sqrt(n))

c(xtn,xtp)
#Q16----

#16. The mean number of hours of flying time for pilots at Continental Airlines is 49 hours per
#month (The Wall Street Journal, February 25, 2003). Assume that this mean was based on
#actual flying times for a sample of 100 Continental pilots and that the sample standard
#deviation was 8.5 hours.
#a. At 95% confidence, what is the margin of error?
#b. What is the 95% confidence interval estimate of the population mean flying time for
#the pilots?
#c. The mean number of hours of flying time for pilots at United Airlines is 36 hours
#per month. Use your results from part (b) to discuss differences between the flying
#times for the pilots at the two airlines. The Wall Street Journal reported United Airlines
#as having the highest labor cost among all airlines. Does the information in
#this exercise provide insight as to why United Airlines might expect higher labor
#costs?

n = 100
xbar = 49
s = 8.5

#(a)
t = qt(1 - (1-0.95)/2, df = 99)
t

me = t*(s/sqrt(n))
me

#(b)
xbar+c(-me,me)

#(c)
#Fewer hours and higher cost for United


#Q18-----

#18. Older people often have a hard time finding work. AARP reported on the number of weeks
#it takes a worker aged 55 plus to find a job. The data on number of weeks spent searching
#for a job contained in the file JobSearch are consistent with the AARP findings (AARP
#Bulletin, April 2008).
#a. Provide a point estimate of the population mean number of weeks it takes a worker aged
#55 plus to find a job.
#b. At 95% confidence, what is the margin of error?
#c. What is the 95% confidence interval estimate of the mean?
#d. Discuss the degree of skewness found in the sample data. What suggestion would you
#make for a repeat of this study?


#mu<=55
#mu0>55

n=55




#Q19-----

#19. The average cost per night of a hotel room in New York City is $273 (SmartMoney, March
#2009).Assume this estimate is based on a sample of 45 hotels and that the sample standard
#deviation is $65.
#a. With 95% confidence, what is the margin of error?
#b. What is the 95% confidence interval estimate of the population mean?
#c. Two years ago the average cost of a hotel room in New York City was $229. Discuss
#the change in cost over the two-year period.

xbar=273
s=45
sigma = 65

#(a)
z = qnorm(1 - (1-0.95)/2)
z

ME = (sigma/sqrt(s))
E = ME*z

#(b)
xbar+c(-E,E)

#(c)

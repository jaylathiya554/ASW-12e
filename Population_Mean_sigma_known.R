#Population Mean: ??sigma known----
#Q2-----

#2. A simple random sample of 50 items from a population with ??  6 resulted in a sample
#mean of 32.
#a. Provide a 90% confidence interval for the population mean.
#b. Provide a 95% confidence interval for the population mean.
#c. Provide a 99% confidence interval for the population mean.

n = 50
mu = 32
sigma = 6

se= sigma/sqrt(n)  # stranded error
se

z=qnorm((1 - 0.90)/2)  #95, 99
z

E=z*se #Margin of Error
E

mu+c(E,-E)

#Q4----

#4. A 95% confidence interval for a population mean was reported to be 152 to 160. If ??  15,
#what sample size was used in this study?

sigma = 15

E = (160-152)/2
E

z=qnorm(1-((1 - 0.95)/2))
z

n = ((z*sigma)/E)**2
n





#Q5----

#5. In an effort to estimate the mean amount spent per customer for dinner at a major Atlanta
#restaurant, data were collected for a sample of 49 customers. Assume a population standard
#deviation of $5.
#a. At 95% confidence, what is the margin of error?
#b. If the sample mean is $24.80, what is the 95% confidence interval for the population mean?

#(a) and (b)
s=49
mu=24.80
sigma = 5

SE = sigma/sqrt(s)
SE

z=qnorm(1-((1 - 0.95)/2))
z

E = z*SE
E

E*(sigma/sqrt(n))

mu+c(-E,E)

#Q6----

#6. Nielsen Media Research conducted a study of household television viewing times during
#the 8 p.m. to 11 p.m. time period. The data contained in the file named Nielsen are consistent
#with the findings reported (The World Almanac, 2003). Based upon past studies the
#population standard deviation is assumed known with ??  3.5 hours. Develop a 95% confidence
#interval estimate of the mean television viewing time per week during the 8 p.m. to
#11 p.m. time period.

n=300
sigma = 3.5
x = 2550

xbar = x/n
xbar


#Q8----

#8. The National Quality Research Center at the University of Michigan provides a quarterly
#measure of consumer opinions about products and services (The Wall Street Journal,
#February 18, 2003). A survey of 10 restaurants in the Fast Food/Pizza group showed a
#sample mean customer satisfaction index of 71. Past data indicate that the population standard
#deviation of the index has been relatively stable with ??  5.
#a. What assumption should the researcher be willing to make if a margin of error is desired?
#b. Using 95% confidence, what is the margin of error?
#c. What is the margin of error if 99% confidence is desired?

n = 10
xbar = 71
sigma = 5

#(a)

#(b)
stderr = sigma/sqrt(n) # stranded error
stderr

z=qnorm(1-0.025)
z

E = z*stderr # margin of error
E

#(c)
z=qnorm(1-((1-0.99)/2))
z

E = z*stderr
E

#Q10----

#10. Playbill magazine reported that the mean annual household income of its readers is
#$119,155 (Playbill, January 2006). Assume this estimate of the mean annual household income
#is based on a sample of 80 households, and based on past studies, the population standard
#deviation is known to be ??  $30,000.
#a. Develop a 90% confidence interval estimate of the population mean.
#b. Develop a 95% confidence interval estimate of the population mean.
#c. Develop a 99% confidence interval estimate of the population mean.
#d. Discuss what happens to the width of the confidence interval as the confidence level
#is increased. Does this result seem reasonable? Explain.


sigma = 30000
xbar = 119155
n = 80

#(a)
se= sigma/sqrt(n)  # stranded error
se

z=qnorm(1-((1 - 0.90)/2))
z

E=z*se #Margin of Error
E

xbar+c(-E,E)

#(b)
se= sigma/sqrt(n)  # stranded error
se

z=qnorm(1-((1 - 0.95)/2))
z

E=z*se #Margin of Error
E

xbar+c(-E,E)

#(c)
se= sigma/sqrt(n)  # stranded error
se

z=qnorm(1-((1 - 0.99)/2))
z

E=z*se #Margin of Error
E

xbar+c(-E,E)

#(d)
#Width increases as confidence level increases



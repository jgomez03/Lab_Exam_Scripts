### power practice 

## CORRELATION POWER

library(tidyverse)

# analysis plan A

#a) meta-analysis
# traditional power analysis
library(pwr)
pwr.r.test(r=.50,power=.80)
#need a sample size of 28 to reliably replicate the findings from the meta analysis 

#b) single published study
#need safeguard
psych::r.con(r=-.30, n=100)
# -0.4687942 -0.1100677
library(pwr)
pwr.r.test(r=-.11,power=.80)

#c) no estimate 
#traditional estimate 
library(pwr)
pwr.r.test(r=.07,power=.80)
#N = 1598

# analysis plan B

#a) meta-analysis
# traditional power analysis
## try out to different Ns to find smallest N that doesnt give CI difference of more than 50
pwr.r.test(r=.50,power=.80)

psych::r.con(r=.50,n=50)
psych::r.con(r=.50,n=37)

#b) single published study

psych::r.con(r=-.11,n=100)
psych::r.con(r=-.11,n=200)
psych::r.con(r=-.11,n=250)
psych::r.con(r=-.11,n=1000)
psych::r.con(r=-.11,n=1250)


#c) 
psych::r.con(r=.07,n=1598)
psych::r.con(r=.07,n=2000)
psych::r.con(r=.07,n=2050)
psych::r.con(r=.07,n=3000)


### REGRESSION POWER

# when you know the population R squared:
# need effect size as f squared and degrees of freedom for the predictors

my.f2 <- .20 / (1-.20)
print(my.f2)
#f squared = .25

#degress of freedom = u = number of predictors = 3

# calculate power

library(pwr)
pwr.f2.test(u=3, f2=.25, power = .80)

#calculate N using degrees of freedom error given by above code
# N = u + v + 1 = :
N = 3 + 44 + 1
print(N)
#N = 48

# when R squared = .25 need N of 48 to obtain a power of .80 in my study


# when you DON'T know the population R squared:
# safeguard power analysis 

#first, need to calculate the CI
# need MBESS package

library(MBESS)
ci.R2(R2=.20, p=3, N=100, Random.Predictors = FALSE)
#CI = [.05, .32]

# determine f squared using lowerbound CI
my.f2 <- .0596 / (1-.0596)
print(my.f2)
#f2 = .06338

# degrees of freedom - 3 predictors = u = 3

# calculate power
pwr.f2.test(u=3, f2=.06338, power = .80)

#calculate N
N = 3 + 172 + 1
print(N)
#N = 176

## POWER FOR MULTIPLE REGRESSION USING INCRIMENTAL PREDICTORS (AS OPPOSED TO OVERALL R2)

# determine df
# interested in the incremental prediction of one variable, so u = 1

#determine f2 using sr2 and R2
my.f2 <- .02/(1 - .20)
print(my.f2)
#f2=.025

#calculate power
pwr.f2.test(u=1, f2=0.025, power=.80)

#calculate N
# in this case, u now again = number of predictors
N = 3 + 314 + 1
print(N)
# N = 318
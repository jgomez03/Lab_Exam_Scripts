library(tidyverse)
library(apaTables)
library(predictionInterval)
library(pwr)


# Find sample size given meta-analysis of r=.50
pwr.r.test(r=.50, power=.80)



# Find sample size given single published study
psych::r.con(r=-.30, n=100)
pwr.r.test(r=-.07, power=.80)


# Find sample size given weak positive relationship
pwr.r.test(r=.16, power=.80)



# Find sample size given meta-analysis of r=.50
psych::r.con(r=.50, n=100)
psych::r.con(r=.50, n=50)
psych::r.con(r=.50, n=40)
psych::r.con(r=.50, n=37)


# Find sample size given single published study
psych::r.con(r=-.30, n=100)
psych::r.con(r=-.11, n=100)
psych::r.con(r=-.11, n=500)
psych::r.con(r=-.11, n=750)
psych::r.con(r=-.11, n=1000)
psych::r.con(r=-.11, n=1500)
psych::r.con(r=-.11, n=1250)


# Find sample size given weak positive relationship
psych::r.con(r=.07, n=500)
psych::r.con(r=.07, n=1000)
psych::r.con(r=.07, n=2000)
psych::r.con(r=.07, n=3000)
psych::r.con(r=.07, n=3200)
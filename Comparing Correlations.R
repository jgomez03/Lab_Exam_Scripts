library(tidyverse)
library(cocor)

#question 4
#is rating-raises different than rating-critical - how diff
#(j = rating, k = raises, h = critical)
#(r.jk = .59, r.jh = .16, r.kh = .38) n = 30
?cocor.dep.groups.overlap
cocor.dep.groups.overlap(.59, .16, .38, 30, alternative = "two.sided",
                         test = "all", alpha = 0.05, conf.level = 0.95, null.value = 0, 
                         data.name = NULL, var.labels = NULL, return.htest = FALSE)

#question5
#ratings-raises versus complaints-critical
#nonoverlapping
#(j = rating, k = raises, h = complaints, m = critical)
#(r.jk = .59, r.hm = .19, r.jh = .83, r.jm = .16, r.kh = .67, r.km = .38)  )
cocor.dep.groups.nonoverlap (.59, .19, .83, .16, .67, .38, 30, alternative = "two.sided",
                             test = "all", alpha = 0.05, conf.level = 0.95, null.value = 0, 
                             data.name = NULL, var.labels = NULL, return.htest = FALSE)

#question6
#Comparing indepedant correlations
#(r1.jk = .59, r2.hm = .03)
cocor.indep.groups(.59, .03, 30, 30000, alternative = "two.sided",
                   test = "all", alpha = 0.05, conf.level = 0.95, null.value = 0, 
                   data.name = NULL, var.labels = NULL, return.htest = FALSE)

#question1
#load data
bfi2 <- read_csv("bfi2.csv")
glimpse(bfi2)
library(apaTables)
apa.cor.table(bfi2)

library(cocor)
cocor(~A1+C1|E1+O1, data=as.data.frame(bfi2) )

#question2
#correlation within the same data set
cocor(~C1+A1|E1+A1, data=as.data.frame(bfi2) )
#saying, does C1 predict A1 better than E1 predicts A1?

##question 3
bfi2_men <- bfi2 %>% filter(gender==1) %>% select(-gender)
bfi2_women <- bfi2 %>% filter(gender==2) %>% select(-gender)
bfi2_men <- as.data.frame(bfi2_men)
bfi2_women <- as.data.frame(bfi2_women)

cocor(~A1+E1|A1+E1, data=list(bfi2_men, bfi2_women))
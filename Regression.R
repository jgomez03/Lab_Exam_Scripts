##Regression Lab Quiz

library(tidyverse)
library(apaTables)

###load data
my.data <- read_csv("reg_quiz2_data.csv")
glimpse(my.data)

### get initial descriptives
apa.cor.table(my.data)
##save table
apa.cor.table(my.data, filename="Table1_quiz8.doc", table.number=1)

##check for curve. relationship
psych::pairs.panels(as.data.frame(my.data)) #no concerns

###QUESTION 4

##PAS on its own
reg_pas <- lm(aSuc~PAS, data=my.data)
summary(reg_pas)

apa.reg.table(reg_pas)

##a.
##PAS and selfEsteem predicting aSuc
reg_a_PASSE <- lm(aSuc~PAS + selfEsteem, data=my.data)
summary(reg_a_PASSE)

apa.reg.table(reg_a_PASSE)
summary(reg_a_PASSE)

apa.reg.table(reg_a_PASSE, filename="Table2_quiz8.doc", table.number=2)

##b. 
##NAS and selfEsteem predicting aSuc #using hierarchical regression
head(my.data)
reg_b_NASse <- lm(aSuc ~ NAS + selfEsteem, data=my.data)
#run anova
anova(reg_b_NASse)
apa.reg.table(reg_b_NASse)

apa.reg.table(reg_b_NASse, filename="Table3_quiz8.doc", table.number=3)

##c. 
##block regression

block1 = lm(aSuc~NAS+PAS,data=my.data)

apa.reg.table(block1)

block2 = lm(aSuc~NAS+PAS+selfEsteem,data=my.data)

apa.reg.table(block1, block2)

apa.reg.table(block1, block2, filename="Table4_quiz8.doc", table.number=4)
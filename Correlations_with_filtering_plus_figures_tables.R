library(tidyverse)

#obtain data file
bfi_data <- psych::bfi


#tell R about categorical variables gender and education
categorical_variables <- select(bfi_data, gender)
categorical_variables$gender <- as.factor(categorical_variables$gender)
levels(categorical_variables$gender) <- list("Male"=1,"Female"=2)
gender <- categorical_variables$gender

categorical_variables <- select(bfi_data, education)
education <- categorical_variables$education
levels(categorical_variables$education) <- list("HS"=1,"finished HS"=2,"some college"=3,"college graduate"=4, "graduate degree"=5)
education <- categorical_variables$education

# create scale item
Agreeableness_items <- select (bfi_data, A1, A2, A3, A4, A5)
Extraversion_items <- select (bfi_data, E1, E2, E3, E4, E5)
Neuroticism_items <- select (bfi_data, N1, N2, N3, N4, N5)

age <- select(bfi_data, age)

reverse key items ## (N+1) - E1 etc. 
#flip reverse key items Agreeablesness
Agreeableness_items <- mutate(Agreeableness_items,A1=7-A1)

#View(Agreeableness_items)

#flip reverse key items Extraversion 
Extraversion_items <- mutate(Extraversion_items,E1=7-E1)
Extraversion_items <- mutate(Extraversion_items,E2=7-E1)

#obtain scaled scores 
Agreeableness <- psych::alpha(as.data.frame(Agreeableness_items) ,check.keys=FALSE)$scores
Extraversion <- psych::alpha(as.data.frame(Extraversion_items) ,check.keys=FALSE)$scores
Neuroticism <- psych::alpha(as.data.frame(Neuroticism_items), check.keys=FALSE)$scores

#combine into analytic data
analytic_data <- cbind(categorical_variables,age,Agreeableness,Extraversion,Neuroticism)
analytic_data

#save data
write_csv(analytic_data,path="analytic_data.csv")

#filter
analytic_data_nogender <- analytic_data %>% select(Agreeableness,Extraversion,Neuroticism,education,age)

#create a correlation table
#correlation table with no gender
library(apaTables)
analytic_data_nogender
apa.cor.table(analytic_data_nogender, filename="Table1.doc", table.number=1)

#menover40
analytic_data_menover40 <- filter(analytic_data, gender=="Male")
analytic_data_menover40 <- filter(analytic_data, age>=40)
View(analytic_data_menover40)

#correlation table men over 40
analytic_data_menover40
apa.cor.table(analytic_data_menover40, filename="Table2.doc", table.number=2)

#create scatterplot 
my.agreeablness.extra.men.40 <- qplot(Agreeableness,Extraversion,data=analytic_data_menover40)
my.agreeablness.extra.men.40 <- qplot(x=Agreeableness,y=Extraversion,data=analytic_data_menover40)
my.agreeablness.extra.men.40 <- my.scatter + geom_smooth(method = "lm", se = FALSE, color='black')
my.agreeablness.extra.men.40 <- my.scatter + labs(title="Relation between agreeableness and extraversion for men over 40",
                                x="Agreeableness",y="Extraversion")
my.agreeablness.extra.men.40 + theme_classic()
my.agreeablness.extra.men.40 + theme(axis.line.x = element_line(colour = 'black',size=0.5, linetype='solid'),
                                 axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))



print(my.agreeablness.extra.men.40)
cor.test(x=analytic_data_menover40$Agreeableness,y=analytic_data_menover40$Extraversion)
ggsave("Figure1.pdf")
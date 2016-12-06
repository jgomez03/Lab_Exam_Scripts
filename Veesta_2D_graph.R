library(tidyverse)
library(apaTables)

#read a table 

my_data <- read_csv("lectureData6060.csv")

glimpse(my_data)

#process the data to just select columns we need and drop missing values 

analytic.data <- my_data %>% select(Exam, Anxiety, Preparation)

analytic.data <- na.omit(analytic.data)

glimpse(analytic.data)
apa.cor.table(analytic.data)

my.regression <- lm(Exam ~ Anxiety + Preparation, data=analytic.data, na.action = na.exclude)

summary(my.regression)

apa.reg.table(my.regression)

##### MAKING A 2D GRAPH 

#pick predictor for x axis 


sd.x <- sd(analytic.data$Anxiety, na.rm=TRUE)

#graph from range of 2SD +- 

x.axis.range <- seq(-2*sd.x, 2*sd.x, by=.25*sd.x)

#we want two lines - one for high and one for low preparation (moderator)

sd.z <- sd(analytic.data$Preparation, na.rm=TRUE)

z.line.hi= 1*sd.z
z.line.lo= -1*sd.z

#create predicted values for each line - PART 1 

#+1SD Line
predictor.x.range.line.hi <- expand.grid(Anxiety=x.axis.range, Preparation=z.line.hi)
y.values.at.plus.1SD.z <- predict(my.regression, newdata=predictor.x.range.line.hi)

#-1SD Line
predictor.x.range.line.lo <- expand.grid(Anxiety=x.axis.range, Preparation=z.line.lo)
y.values.at.minus.1SD.z <- predict(my.regression,newdata=predictor.x.range.line.lo)

#Put the information describing the lines into a data frame
line.data <- data.frame(x.axis.range, y.values.at.plus.1SD.z, y.values.at.minus.1SD.z)

#make the graph - PAR 2 

library(ggplot2)

#set default (x,y) variables
my.plot <- ggplot(line.data, aes(x=x.axis.range, y=y.values.at.plus.1SD.z))

#make +1 SD Z line (because it is the one in the aes statement above)
my.plot <- my.plot + geom_line(color="black",linetype="dotted",size=1.5)

#make the -1 SD Z line
my.plot <- my.plot + geom_line(aes(x=x.axis.range,y=y.values.at.minus.1SD.z),
                               color="black",linetype="solid",size=1.5)

#set APA part of graph below
my.plot <- my.plot + theme_classic()

#add lines 

my.plot <- my.plot + theme(axis.line.x = element_line(colour='black',size=0.5, linetype='solid'),
                           axis.line.y = element_line(colour='black',size=0.5, linetype='solid'))

print(my.plot)

#label the lines 

my.plot <- my.plot+annotate("text", x= - 1, y = 68.5, label = "+1 SD Preparation")
my.plot <- my.plot+annotate("text", x= - 1, y = 43.5, label = "-1 SD Preparation")

#label 
my.plot <- my.plot + labs(x="Anxiety", y="Exam Grades")

#?annotate

print(my.plot)

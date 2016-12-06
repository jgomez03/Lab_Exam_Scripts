library(tidyverse)
library(ggplot2)
library(apaTables)

my.data <- read_csv("mmr_practice_data.csv")

glimpse(my.data)

#keep complete cases only 

analytic.data <- na.omit(analytic.data)


#run regression

my.regression <- lm(exam ~ anxiety + preparation, data=analytic.data, na.action = na.exclude)

summary(my.regression)

apa.reg.table(my.regression)



##MAKE A 2D REGRESSION GRAPH 

#indicate range of scores on x axis 

sd.x <- sd(analytic.data$anxiety, na.rm=TRUE)
x.axis.range <- seq(-50*sd.x, 50*sd.x, by=.25*sd.x)

sd.z <- sd(analytic.data$preparation, na.rm=TRUE)
z.line.hi=1*sd.z
z.line.lo=-1*sd.z

predictor.x.range.line.hi <- expand.grid(anxiety=x.axis.range, preparation=z.line.hi)
y.values.at.plus1sd.z <- predict(my.regression,newdata=predictor.x.range.line.hi)

predictor.x.range.line.lo <- expand.grid(anxiety=x.axis.range, preparation=z.line.lo)
y.values.at.minus1sd.z <- predict(my.regression,newdata=predictor.x.range.line.lo)

line.data <- data.frame(x.axis.range, y.values.at.plus1sd.z, y.values.at.minus1sd.z)

###############

#set default (x,y) variables
my.plot <- ggplot(line.data, aes(x=x.axis.range, y=y.values.at.plus1sd.z))

#make +1 SD Z line (because it is the one in the aes statement above)
my.plot <- my.plot + geom_line(color="black",linetype="dotted",size=1.5)

#make the -1 SD Z line
my.plot <- my.plot + geom_line(aes(x=x.axis.range,y=y.values.at.minus1sd.z),
                               color="black",linetype="solid",size=1.5)


#set APA part of graph below
my.plot <- my.plot + theme_classic()

#add lines 

my.plot <- my.plot + theme(axis.line.x = element_line(colour='black',size=0.5, linetype='solid'),
                           axis.line.y = element_line(colour='black',size=0.5, linetype='solid'))

#adjust the axis 
my.plot <- my.plot + coord_cartesian(xlim=c(1,4),ylim=c(1,6))

my.plot <- my.plot + scale_x_continuous(breaks=seq(1,4,by=1)) 
my.plot <- my.plot + scale_y_continuous(breaks=seq(1,6,by=1))

print(my.plot)

#label the lines 

my.plot <- my.plot+annotate("text", x= 1.5, y = 5.5, label = "+1 SD Positive Affect")
my.plot <- my.plot+annotate("text", x= 1.5, y = 3.5, label = "-1 SD Positive Affect")
#?annotate

#label the axis
my.plot <- my.plot + labs(x="anxiety", y="exam grade")


print(my.plot)

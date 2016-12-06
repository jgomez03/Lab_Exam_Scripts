library(tidyverse)
library(ggplot2)
library(apaTables)

###load data
my.data <- read_csv("mmr_practice_data.csv")
apa.cor.table(my.data, filename="Table 1_APA.doc", table.number=1)

glimpse(my.data)

my.data %>% select(exam, anxiety, preparation)
na.omit(my.data)

#center variables ###run the REGRESSION

##add column with mean center anxiety
my.data <- my.data %>% mutate(x.centered=as.numeric(scale(anxiety,center=T,scale=F)) )

##add column with mean center preparation
my.data <- my.data %>% mutate(z.centered=as.numeric( scale(preparation,center=T,scale=F)) )

###run the REGRESSION
my.regression <- lm(exam ~ x.centered + z.centered, data=my.data, na.action = na.exclude)

summary(my.regression)
apa.reg.table(my.regression, filename="Table 2_APA.doc", table.number=2)


##explore interaction
#high prep

sd.z <- sd(my.data$z.centered, na.rm=TRUE)
my.data <- my.data %>% mutate(z.centered.at.plus.1SD = z.centered - sd.z)
simple.slope.plus.1SD <- lm(exam ~ x.centered + z.centered.at.plus.1SD,
                            data=my.data, na.action=na.exclude) 

#low prep
my.data <- my.data %>% mutate(z.centered.at.minus.1SD=z.centered + sd.z)
simple.slope.minus.1SD <- lm(exam ~ x.centered + z.centered.at.minus.1SD,
                             data=my.data,na.action=na.exclude)



###Graphing
#pick anxiety to be x
sd.x <- sd(my.data$x.centered,na.rm=TRUE)


##making a 2D graph (part 1)


#choose x axis range
x.axis.range <-seq(-2*sd.x,2*sd.x,by=.25*sd.x)

#values of z that indicate high/low preparation
sd.z<-sd(my.data$z.centered, na.rm=TRUE)
z.line.hi= 1*sd.z
z.line.lo=-1*sd.z

#create predicted values for each line
#+1SD line
predictor.x.range.line.hi <- expand.grid(x.centered=x.axis.range, z.centered=z.line.hi)
y.values.at.plus.1SD.z <- predict(my.regression,newdata=predictor.x.range.line.hi)

#-1SD line
predictor.x.range.line.lo <- expand.grid(x.centered=x.axis.range, z.centered=z.line.lo)
y.values.at.minus.1SD.z <- predict(my.regression,newdata=predictor.x.range.line.lo)

#pull into data frame
line.data <- data.frame(x.axis.range, y.values.at.plus.1SD.z, y.values.at.minus.1SD.z)

##making a 2D graph (part 2)

#library(ggplot2)
#set default variables
my.plot<- ggplot(line.data, aes(x=x.axis.range, y=y.values.at.plus.1SD.z))

#make +1 SD Z line 
my.plot <- my.plot + geom_line(color="black",linetype="dotted",size=1.5)

#make -1 SD Z line 
my.plot <- my.plot + geom_line(aes(x=x.axis.range, y=y.values.at.minus.1SD.z),
                               color="black",linetype="solid",size=1.5)

#set graph as apa
my.plot <- my.plot + theme_classic()

#adjust the axis 
my.plot <- my.plot + coord_cartesian(xlim=c(-2,2),ylim=c(0,100))
print(my.plot)

#label the lines
my.plot <- my.plot + labs(x="Anxiety (mean centered)", y="Exam Grade")
my.plot <- my.plot+annotate("text", x = -1, y = 68.5, label = "+1 SD Preparation")
my.plot <- my.plot+annotate("text", x = -1, y = 43.5, label = "-1 SD Preparation")
print(my.plot)

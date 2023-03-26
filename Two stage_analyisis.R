setwd("C:/Users/Asus/Desktop/MESIO - LDA/2022-2023/02_Two Stage model")
library(lattice)


#Read database
GrowthGV<-read.table("growthgv.txt",header=T)
head(GrowthGV)
summary(GrowthGV)

#Change the data type of the variables child  group 
GrowthGV$child<-as.factor(GrowthGV$child)
GrowthGV$group<-as.factor(GrowthGV$group)

#Transformations
#Move the age measurement to the origen
GrowthGV$age<-GrowthGV$age-6
summary(GrowthGV)

Plot1<-xyplot(height ~ age, groups = child,
              data = GrowthGV,
              type = "l" ,xlab="Years from Study Start",ylab="Height")


#Set output to PDF and set output path and filename
pdf("Spaghetti.pdf",width=12)
Plot1
dev.off()

#Grouped data in library nlme
library(nlme)
GrowthGV2<-groupedData(height~age|child,GrowthGV,outer=~group,labels=list(y="Height of the child"),units=list(y="(cm)"))

#Grouped data-Graphical representation
pdf("GroupedPlot.pdf")
plot(GrowthGV2)
dev.off()

pdf("GroupedPlot2.pdf")
plot(GrowthGV2,outer=T)
dev.off()

meanHeightbyID<-tapply(GrowthGV$height,as.factor(GrowthGV$child),mean,na.rm=T)


plot(GrowthGV2[GrowthGV2$child==15,])
plot(by(GrowthGV2$height,GrowthGV2$age,mean),type="l",lty=1,lwd=1)

#Boxplot
meanHeightbyAge<-tapply(GrowthGV$height,as.factor(GrowthGV$age),mean,na.rm=T)
boxplot(GrowthGV$height~as.factor(GrowthGV$age), main= 'Boxplot height per Age',xlab="Age",ylab='Height (cm)')

#Extra correlation analysis
GrowthGVWide <- reshape(data=GrowthGV, 
                    direction="wide", 
                    v.names=c("height"), 
                    times=0:4, 
                    timevar="age", 
                    idvar="child")
head(GrowthGVWide)


dataCorr <- GrowthGVWide[, c(3:7)]
library("PerformanceAnalytics")
chart.Correlation(dataCorr, histogram=TRUE)
cov(dataCorr)

#Global linear model
GrowthGV.lm<-lm(height~age,GrowthGV)
summary(GrowthGV.lm)

GrowthGV.lm2<-lm(height~age+group,GrowthGV)
summary(GrowthGV.lm2)



#This is the 1st step in the 2-steps model described by Molenberghs and Verbeke.

modlist1<-lmList(height~age|group, GrowthGV2, na.action=na.pass)

modlist1

pdf("intervals.pdf")
plot(intervals(modlist1))
dev.off()


#2nd step of the two stage model

#### EXERCISE
# We need to create a data set with the data of each individual in a # row containing:
# The individual identification
# The intercept, and the slope coefficient
# Baseline data (outer) for our individuals

attributes(modlist1)


beta0<- coef(modlist1)[,1]

beta1<- coef(modlist1)[,2]





coef(modlist1)
bbdd<-data.frame(Id=as.numeric(attributes(modlist1)$names),
                 beta0=coef(modlist1)[,1],beta1=coef(modlist1)[,2])
bbdd <- bbdd[order(bbdd[,1]),]
bbdd <- cbind(bbdd,group=GrowthGVWide$group)


#Boxplot
meanbeta0bygroup<-tapply(bbdd$beta0,as.factor(bbdd$group),mean,na.rm=T)
meanbeta1bygroup<-tapply(bbdd$beta1,as.factor(bbdd$group),mean,na.rm=T)


#Let's check if they are quite related ?inverse correlation?

with(bbdd,cor(beta0,beta1))


library(ggplot2)
ggplot(bbdd, aes(x=beta0, y=beta1)) + geom_point()
ggplot(bbdd, aes(x=beta0, y=beta1, color=group)) + geom_point()


install.packages('dplyr', repos = 'https://cloud.r-project.org')

library(dplyr)
bbdd %>%
  group_by(group) %>%
  summarize(cor=cor(beta0, beta1))


# Fit a model for the intercept
modbeta0<-lm(beta0~group,bbdd)
summary(modbeta0)
confint(modbeta0)

### Fit a model for the slope
# All the variables should be included
modbeta1<-lm(beta1~group,bbdd)
summary(modbeta1)
confint(modbeta1)


#### END OF  EXERCISE


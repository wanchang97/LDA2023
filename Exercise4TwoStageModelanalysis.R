library(readxl)
trenal <- read_excel("Trenal.XLS") # summary(trenal)
trenal= trenal[,-18] #remove a noninformative column const
# Continuous or discrete variables
trenal$id = as.factor(trenal$id)
trenal$j = as.factor(trenal$j)
#trenal$time = as.factor(trenal$time)
trenal$male = as.factor(trenal$male)
trenal$cardio = as.factor(trenal$cardio)
trenal$reject = as.factor(trenal$reject)
# Change the name of respons
colnames(trenal)[19] <- "HC"
trenal.long = trenal[,13:20] # long table form

# Remove j
trenal.long = trenal.long[,-6]
trenal.long.unique <- trenal.long[match( unique(trenal.long$id), trenal.long$id),]
trenal.long.noNA <- na.omit(trenal.long)

# Wide table form
trenal.wide = as.data.frame(subset(trenal,trenal$j=="1"))[,1:18] # 1160 x 18

# Two stage Model analysis
# 1st stage A model for each individual estimating the intercept and time was adjusted
lm.firststage = lm(HC ~ time+age+male+reject+cardio ,trenal.long)
summary(lm.firststage)
library(nlme)
trenal.group.age<-groupedData(HC~time|id,trenal.long,outer=~age,labels=list(y="HC level"),units=list(y="(%)"))
trenal.group.male<-groupedData(HC~time|id,trenal.long,outer=~male,labels=list(y="HC level"),units=list(y="(%)"))
trenal.group.reject<-groupedData(HC~time|id,trenal.long,outer=~reject,labels=list(y="HC level"),units=list(y="(%)"))
trenal.group.cardio<-groupedData(HC~time|id,trenal.long,outer=~cardio,labels=list(y="HC level"),units=list(y="(%)"))

list.male<-lmList(HC~time|id, 
                 trenal.group.male, na.action=na.pass)
#View(modlist1)
attributes(list.male)

pdf("intervals.pdf")
plot(intervals(list.male))
dev.off()

beta0<- coef(modlist1)[,1]
beta1<- coef(modlist1)[,2]

coef(modlist1)
bbdd<-data.frame(Id=as.numeric(attributes(modlist1)$id),
                 beta0=coef(modlist1)[,1],beta1=coef(modlist1)[,2])
bbdd <- bbdd[order(bbdd[,1]),]
bbdd <- cbind(bbdd,group=trenal.long$id)

#Boxplot
meanbeta0bygroup<-tapply(bbdd$beta0,as.factor(bbdd$group),mean,na.rm=T)
meanbeta1bygroup<-tapply(bbdd$beta1,as.factor(bbdd$group),mean,na.rm=T)

#Let's check if they are quite related ?inverse correlation?

with(bbdd,cor(beta0,beta1))

library(ggplot2)
ggplot(bbdd, aes(x=beta0, y=beta1)) + geom_point()
ggplot(bbdd, aes(x=beta0, y=beta1, color=id)) + geom_point()

#plot(intervals(modlist1)) ?  can not plot Error in UseMethod("intervals") : 
#no applicable method for 'intervals' applied to an object of class "c('lmList4', 'list', 'vector')"


modlist2<-lmList(HC~time+age|id, 
                 trenal.long, na.action=na.pass)
modlist2

modlist2.2<-lmList(HC~age|id, 
                 trenal.long, na.action=na.pass)
modlist2.2
# every individual has only one age to perform the operation




# 2nd stage the estimated intercept and time were considered the output of a regression 
# using variables 
# A different measurement could have some random effect on intercept and the slope of time
# A different measurement have no affect on the slope of age, male, reject and cardio

library(nlme)




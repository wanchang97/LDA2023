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
set.seed(1)
selected <- sample(1:length(unique(trenal.long.noNA$id)),30,replace=T)
trenal.long.noNA.selected = trenal.long.noNA[(trenal.long.noNA$id %in% c(selected)),]
# Wide table form
trenal.wide = as.data.frame(subset(trenal,trenal$j=="1"))[,1:18] # 1160 x 18

# Two stage Model analysis
# 1st stage A model for each individual estimating the intercept and time was adjusted
lm.firststage = lm(HC ~ time+age+male+reject+cardio ,trenal.long)
summary(lm.firststage)
library(nlme)

trenal.long.noNA.groupbyage <- groupedData(HC ~ time|id,trenal.long.noNA,outer=~age,labels=list(x = "time",y="HC level",units=list(y="(cm)")))
trenal.long.noNA.groupbymale <- groupedData(HC ~ time|id,trenal.long.noNA,outer=~male,labels=list(x = "time",y="HC level",units=list(y="(cm)")))
trenal.long.noNA.groupbyreject <- groupedData(HC ~ time|id,trenal.long.noNA,outer=~reject,labels=list(x = "time",y="HC level",units=list(y="(cm)")))
trenal.long.noNA.groupbycardio <- groupedData(HC ~ time|id,trenal.long.noNA,outer=~cardio,labels=list(x = "time",y="HC level",units=list(y="(cm)")))

res.list.groupbyage <- lmList(HC ~ time |id, data=trenal.long.noNA.groupbyage)
b = lapply(res.list.groupbyage,coef)
V = lapply(res.list.groupbyage,vcov)
#trenal.long.noNA.select.groupbyagemalereject <- groupedData(HC ~time|id,trenal.long.noNA.selected,outer=~age+male+reject,labels=list("HC level", units = list(y="(%)")))
#plot(trenal.long.noNA.select.groupbyagemalereject) 
#formula(trenal.long.noNA.select.groupbyagemalereject)
#gsummary(trenal.long.noNA.select.groupbyagemalereject)
#fm1 <- lme(trenal.long.noNA.select.groupbyagemalereject)

trenal.long.noNA.select.groupbymale <- groupedData(HC ~time|id,trenal.long.noNA.selected,outer=~male,labels=list("HC level", units = list(y="(%)")))
pdf("GroupedPlot/GroupbyMale.pdf")
plot(trenal.long.noNA.select.groupbymale)                                                                                                                                  
dev.off()

trenal.long.noNA.select.groupbycardio <- groupedData(HC ~time|id,trenal.long.noNA.selected,outer=~cardio,labels=list("HC level", units = list(y="(%)")))
pdf("GroupedPlot/GroupbyCardio.pdf")
plot(trenal.long.noNA.select.groupbycardio)                                                                                                                                  
dev.off()


trenal.long.noNA.select.groupbyreject <- groupedData(HC ~time|id,trenal.long.noNA.selected,outer=~reject,labels=list("HC level", units = list(y="(%)")))
pdf("GroupedPlot/GroupbyReject.pdf")
plot(trenal.long.noNA.select.groupbyreject)                                                                                                                                  
dev.off()


trenal.long.noNA.select.groupbyage <- groupedData(HC ~time|id,trenal.long.noNA.selected,outer=~age,labels=list("HC level", units = list(y="(%)")))
pdf("GroupedPlot/GroupbyAge.pdf")
plot(trenal.long.noNA.select.groupbyage)                                                                                                                                  
dev.off()


res.male <- lme(HC~time,random=~1|id,data=trenal.long.noNA)
plot(augPred(res.male, level = 0:1, length.out = 2))

res.age <- lme(HC ~ time,random = ~age|id,data=trenal.long.noNA)
plot(augPred(res.age, level = 0:1, length.out = 2))


res.list.age.selected <- lmList(HC ~ age | id, data=trenal.long.noNA.selected)
plot(augPred(res.list.age.selected),grid=TRUE)


res.list.age <- lmList(HC ~ age | id, data=trenal.long.noNA)
plot(augPred(res.list.age),grid=TRUE)


















trenal.group.age<-groupedData(HC~time|id,trenal.long,outer=~age,labels=list(y="HC level"),units=list(y="(%)"))
trenal.group.male<-groupedData(HC~time|id,trenal.long,outer=~male,labels=list(y="HC level"),units=list(y="(%)"))
trenal.group.reject<-groupedData(HC~time|id,trenal.long,outer=~reject,labels=list(y="HC level"),units=list(y="(%)"))
trenal.group.cardio<-groupedData(HC~time|id,trenal.long,outer=~cardio,labels=list(y="HC level"),units=list(y="(%)"))

list.male<-lmList(HC~time|id, 
                 trenal.group.male, na.action=na.pass)

#View(list.male)
attributes(list.male)

pdf("intervals.pdf")
plot(intervals(list.male))
dev.off()

beta0<- coef(list.male)[,1]
beta1<- coef(list.male)[,2]

coef(list.male)
bbdd<-data.frame(Id=as.numeric(attributes(list.male)$id),
                 beta0=coef(list.male)[,1],beta1=coef(list.male)[,2])
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




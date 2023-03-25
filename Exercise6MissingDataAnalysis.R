# This is my question on long table and wide table transform:
library(readxl)
trenal <- read_excel("Trenal.XLS") # summary(trenal)
trenal= trenal[,-18] #remove a noninformative column const

# Continuous or discrete variables
trenal$id = as.factor(trenal$id)
trenal$j = as.factor(trenal$j)
trenal$time = as.factor(trenal$time)
trenal$male = as.factor(trenal$male)
trenal$cardio = as.factor(trenal$cardio)
trenal$reject = as.factor(trenal$reject)

# Change the name of respons
colnames(trenal)[19] <- "HC"

trenal.long = trenal[,13:20] # long table form

# Remove j
trenal.long = trenal.long[,-6]
trenal.long.unique <- trenal.long[match( unique(trenal.long$id), trenal.long$id),]
trenal.long.noNA <- na.omit(trenal.long)# reordered

# Wide table form?
trenal.wide = as.data.frame(subset(trenal,trenal$j=="1"))[,1:18] # 1160 x 18

# Analyse the NA values descriptively
## First to collect how many NAs are in HC0, Hc0.5, Hc1, ..., Hc 10
Hc.NA = numeric(12)
for (i in c(1:12)) {
  Hc.NA[i] = sum(is.na(trenal.wide[,i]))
}
# 1   0   1  87 205 314 418 508 595 672 749 812
# The number of missing data / the ideal case have all meassurements for everyone
missingdata.percentage = Hc.NA/1160
plot(missingdata.percentage,xaxt="n",xlab ="j",ylab="Missing data%")
axis(side=1,at=c(1,2,3,4,5,6,7,8,9,10,11,12),labels=colnames(trenal.wide)[1:12])
# Conclusion could be at the first three measurements, there are almost full data
# More people tends to miss the measurements when time increases

##Second  We can extract all NA data from the long table to analyse their construction
trenal.long.NA = trenal.long[is.na(trenal.long$HC),]
#t = unique(trenal.long.NA$id) # 821 individuals
trenal.long.NA.unique <- trenal.long.NA[match( unique(trenal.long.NA$id), trenal.long.NA$id),]
summary(trenal.long.NA.unique)

## Conclusion, For the missing data, we can see that 
png(file="MissingValueAnalysis.png",
    width=600, height=1200)
par(mfrow=c(4,2))
## age 
hist(trenal.long.unique$age,title="Age distribution in original data")
hist(trenal.long.NA.unique$age,col="red",title="Age distribution in missing data")

## male
plot(trenal.long.unique$male)
title(main="Gender distribution in original data")
plot(trenal.long.NA.unique$male,col="red")
title(main="Gender distribution in missing data")

## cardio
plot(trenal.long.unique$cardio)
title(main="Cardio distribution in original data")
plot(trenal.long.NA.unique$cardio,col="red")
title(main="Cardio distribution in missing data")

## reject
#hist(as.numeric(trenal.long.NA.unique$reject),freq=FALSE,main="Histogram Plot for reject",xaxt="n")
#axis(side=1,at=c(0,1),labels=c("not reject","reject"))
plot(trenal.long.unique$reject)
title(main="Reject distribution in original data")
plot(trenal.long.NA.unique$reject,col="red") 
title(main="Reject distribution in missing data")
dev.off()



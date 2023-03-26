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

# Multivariate Linear Model analysis
lm1 <- lm(HC ~ time, trenal.long)
summary(lm1)

lm2 <- lm(HC ~ time + age, trenal.long)
summary(lm2)

lm3 <- lm(HC ~ time + age + male,trenal.long)
summary(lm3)

lm4 <- lm(HC ~ time + age + male + reject,trenal.long)
summary(lm4)

lm5 <- lm(HC ~ time + age + male + reject + cardio,trenal.long)
summary(lm5)

anova(lm2,lm3,lm4,lm5)

# Variables must keep are: intercept, time, age, male
# if p value is 0.01, it is better to keep reject
# if p value is 0.05, it is better to keep  cardio





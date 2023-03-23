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
trenal.long.noNA <- na.omit(trenal.long)# reordered

# Wide table form?
trenal.wide0 = trenal[,1:17] # directly taken from the raw data, where the id, age,cardio,reject are all not correct.

trenal.wide1 <- reshape(trenal.long,
                       direction="wide",
                       timevar="time",
                       idvar = "id",
                       v.names="HC",
                       sep="_")
# gives me a dataframe with dimension 1160 x 6 where the HC level is apparently not meaningful
trenal.wide2 <- reshape(trenal.long.noNA,
                        direction="wide",
                        timevar="time",
                        idvar = "id",
                        v.names="HC",
                        sep="_")# It gives me error


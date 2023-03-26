# Learn two stage model for longitudinal data analysis
library(nlme)
head(Orthodont)
?Orthodont
summary(Orthodont)
Orthodont.unique <- Orthodont[match( unique(Orthodont$Subject), Orthodont$Subject),]
# For easier interpretation of the results to be shown below, we cna center the age varibale with
Orthodont$age <- Orthodont$age-min(Orthodont$age)
# In this way the model intercepts will reflect the average distance at age 8
# Mixed-Effects Model Approach
# Allowing the intercepts and slopes to (randomly) vary across subjects
# (also sometimes called a random intercepts and slopes model)
res1 <- lme(distance ~age, random=~age|Subject,data =Orthodont)
summary(res1)
# Therefore, the estimated average distance at age 8 is b0 = 22.04 mm (Std.Error SE = .420)
# For each year, the distance is estimated to increase on average by b1 = 0.66 mm (SE = .071)
# However, there is variability in the intercepts and slopes, as reflected by their estimated std(SD(b0i)=1.887 and SD(b1i)=0.226, respectively)
# Also, intercepts and slopes appear to be somewhat correlated (\rho = .21)
# Finally, residual variability remains ( reflecting deviations of the measurements from teh subject-specific regression lines)
# as given by the residual standard deviation of sigma =1.31
data.M01 = Orthodont[c(1:4),]
lm.M01= lm(distance ~age,data.M01)
summary(lm.M01)

data.M16 = Orthodont[Orthodont$Subject=="M16",]
lm.M16= lm(distance ~age,data.M16)
summary(lm.M16)

## Two-stage Approach
# Now let us use a two-stage approach to analyse these data.
# First the linear regression model is fitted to the data for each person separately (i.e., based on the four observations per individual):
fm1 <- lmList(distance ~ age | Subject, Orthodont)
# standardized residuals versus fitted values by gender
plot(fm1, resid(., type = "pool") ~ fitted(.) | Sex, abline = 0, id = 0.05)
# box-plots of residuals by Subject
plot(fm1, Subject ~ resid(.))
# observed versus fitted values by Subject
plot(fm1, distance ~ fitted(.) | Subject, abline = c(0,1))

res.list <- lmList(distance ~ age|Subject,data=Orthodont)
# standardized residuals vs fitted values by gender
plot(res.list,resid(.,type="pool")~fitted(.)|male,abline=0,id=0.05)
# We can examine the individual profiles and fitted regression lines for each individual with
plot(augPred(res.list),grid=TRUE)

# From that object, we can extract the estimated model coefficients(intercepts and slopes) and the corresponding variance-covariance matrices with 
b = lapply(res.list,coef)  # 54 (27*2) x 1
V = lapply(res.list,vcov)  # 54 x 54

# A dummy variable including the estimate type( alternating intercept and slope) and a subject id variabel are also needed, which can be created with
estm = rep(c("intercept","slope"),length(b))
subj = rep(names(b),each=2)

# Next we create one long vector with the model coefficients and the corresponding block-diagonal variance covariance matrix with ( the metafor package needs to be loded for the bldiag() function):
library(metafor)
b <- unlist(b) # flatten lists
V <- bldiag(V) # construct block diagonal matrix
# Finally we conduct a multivariate meta-analysis with the model coefficients (since we have two correlated coefficients per subject). 
# The V matrix contains the variance and covariances of the sampling errors.
# We also allow for heterogeneity in the true outcomes (i.e., coefficients) 
# and allow them to be correlated (by using an unstructured variance covariance matrix for the true outcomes)
# The model can be fitted with
# Meta-Analysis via multivariate 
res2 = rma.mv(b ~ estm - 1,V,random = ~estm|subj,struct = "UN") # meta-analysis via multivariate linear mixed effects models
res2

res2.1 = rma.mv(b ~ estm,V,random = ~estm|subj,struct = "UN") # meta-analysis via multivariate linear mixed effects models
res2.1

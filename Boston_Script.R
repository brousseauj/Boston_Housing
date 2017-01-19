
library(MASS)


# Read in Data ------------------------------------------------------------
df = Boston
# Boston is contained in the MASS package.


# Explore Data ------------------------------------------------------------

summary(df)
        

library(corrplot)


df_cor = cor(df)
corrplot.mixed(df_cor)

# We can see some features that are highly correlated with eachother. 

library(ggplot2)
qplot(x = medv,data=df,geom ='histogram')
#Slight right skew in the data, to balance, we can take log(medv)
qplot(x=log(medv),data=df,geom='histogram')
#better



# Models ------------------------------------------------------------------

model1 = lm(log(medv)~.,data=df)
summary(model1)
par(mfrow=c(2,2))
plot(model1)


#Let's try and pick the best model.
library(MASS)
step = stepAIC(model1,direction = 'both')
step$anova
step_model= lm(log(medv) ~ crim + zn + chas + nox + rm + dis + rad + tax + ptratio + 
                 black + lstat,data=df)

summary(step_model)
anova(step_model,model1)
#We were able to remove some predictors that arent that important 
# and enhance our model, but it is not statiscally significant to remove them

vif(step_model)
sqrt(vif(step_model))>2


#Tests for multi-collineratity among predictors
# is >5 remove from model

#Rad is =6.7, so we need to remove this. 

model2 = lm(log(medv) ~ crim + zn + chas + nox + rm + dis +
  tax + ptratio + black + lstat,data=df)
summary(model2)
plot(model2)


vif(model2)
sqrt(vif(model2))>2




#Assumption 2 - mean of residuals ~ 0
mean(model2$residuals)
plot(model2$residuals)
abline(h=0,col='red')
#Assumption 3 - Equal Variance

par(mfrow=c(2,2))
plot(model2)

## outliers? 
outlierTest(model2)

#let's try to remove point 413,372,369,406

df2=df[-c(413,372,369,406),,drop=T]
row.names(df2)=1:nrow(df2)

model3 = lm(log(medv) ~ crim + zn + chas + nox + rm + dis +
              tax + ptratio + black + lstat,data=df2)
par(mfrow=c(2,2))
plot(model3)
summary(model3)

vif(model3)
sqrt(vif(model3))>2
#Model 3 increased the R2 by .05 compared to model 2

model3_step=stepAIC(model3,df2)
model3_step$anov

summary(model3_step)
par(mfrow=c(2,2))
plot(model3_step)


outlierTest(model3_step)

# need to remove some points from this model. 
df3=df2[-c(371,400,373),,drop=T]
row.names(df3)=1:nrow(df3)
model4 = lm(log(medv) ~ crim + zn + chas + nox + rm + dis +
                 tax + ptratio + black + lstat,data=df3)
summary(model4)
par(mfrow=c(2,2))
plot(model4)

#now we ahve a model with no extreme outliers. Let's start to check the assumptions of the model


# Model 4 - outliers ------------------------------------------------------
library(car)
outlierTest(model4)
leveragePlots(model4)


# model 4 - influentional observations -------------------------------------

#added variable plots
avPlots(model4)

#Cooks D Plot
cutoff_value = 4/((nrow(df4)-length(model4$coefficients)-2))

plot(model4, which = 4, cook.levels = cutoff_value)
# some points influecing still 

influencePlot(model4, id.method = 'identify',
              main = 'Influence Plot',
              sub="Circle size is proportial to Cook's Distance")

# model 4 - non-normality -------------------------------------------------

qqPlot(model4)
#still some minor outliers

library(MASS)
sresid = studres(model4)
hist(sresid,freq = F,main='Distribution of Residuals')
xfit = seq(min(sresid),max(sresid),length=40)
yfit=dnorm(xfit)
lines(xfit,yfit)


# model 4 - non-constant error variance -----------------------------------

ncvTest(model4)
spreadLevelPlot(model4)


# model 4 - multi-collinearity --------------------------------------------

vif(model4)
sqrt(vif(model4))


# model 4 - nonlinearity --------------------------------------------------

crPlots(model4)


# model 4 - non-independence of errors ------------------------------------

durbinWatsonTest(model4)


library(gvlma)
gvmodel = gvlma(model4)
summary(gvmodel)


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
plot(model1)


# Diagonistics and Improving the model ------------------------------------
library(car)
vif(model1)

#Let's try and pick the best model.
step = stepAIC(model1,direction = 'both')
step$anova
step_model= lm(log(medv) ~ crim + zn + chas + nox + rm + dis + rad + tax + ptratio + 
                 black + lstat,data=df)
anova(step_model,model1)
#We were able to remove some predictors that arent that important 
# and enhance our model, but it is not statiscally significant to remove them

vif(step_model)
#Tests for multi-collineratity among predictors
# is >5 remove from model

#Rad is =6.7, so we need to remove this. 

model2 = lm(log(medv) ~ crim + zn + chas + nox + rm + dis +
  tax + ptratio + black + lstat,data=df)
summary(model2)

vif(model2)

#Assumption 2 - mean of residuals ~ 0
mean(model2$residuals)

#Assumption 3 - Equal Variance

par(mfrow=c(2,2))
plot(model2)

## outliers? 

plot(model2)

#let's try to remove point 413,372,369,406

df2=df[-c(413,372,369,406),]

model3 = lm(log(medv) ~ crim + zn + chas + nox + rm + dis +
              tax + ptratio + black + lstat,data=df2)
plot(model3)
summary(model2)
summary(model3)
#Model 3 increased the R2 by .05 compared to model 2

lev = hat(model.matrix(model3))
plot(lev)
#still a very high leveage point. 
df2[lev>.2,]

#381 and 419 are above .2 
#Let's look at the cook's distance


cooks=cooks.distance(model3)
plot(cooks)
points(c(381,419),cooks[c(381,419)],col='red')

#Clearly those points are not outliers or influential. All the data is good now.

#with all the outliers removed, we can incease our model efficeny by stepping

summary(model3)
model3_step=stepAIC(model3,df2)
model3_step$anov
plot(model3_step)
anova(model3,model3_step)
# different enough models. 

final_model

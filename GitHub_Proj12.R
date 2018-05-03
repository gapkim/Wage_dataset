##############################
### Initial Setup

knitr::opts_chunk$set(comment=NA, echo=FALSE, warning=FALSE, message=FALSE,
                      fig.align="center")
options(digits=4)
rm(list=ls())

library(ISLR)
data(Wage)

#table(Wage$region)

Wage = Wage[, -6]

#sum(is.na(Wage))
#str(Wage)

library(ggplot2)
library(reshape2)

Wage.cat = Wage[,c(3:9,10)]
Wage.cat.melt = melt(Wage.cat[,-8], id.vars = "logwage")

ggplot(data=Wage.cat.melt) +
   geom_boxplot(aes(x=value, y=logwage)) +
   facet_wrap(~variable, scale="free") +
   labs(x="Categorical variable")

library(gridExtra)

p1 = ggplot(data=Wage) +
   geom_point(aes(x=age, y=logwage, color=health_ins)) +
   scale_x_continuous(breaks=seq(20,80, by=10))

p2 = ggplot(data=Wage) +
   geom_point(aes(x=age, y=logwage, color=education)) +
   scale_x_continuous(breaks=seq(20,80, by=10))

grid.arrange(p1, p2, ncol=1)

attach(Wage)
agelims = range(age)
age.grid = seq(from=agelims[1], to=agelims[2])

smooth.age = smooth.spline(x=age, y=logwage, cv=FALSE)
cat("Optimal DF of the smoothing spline for 'age': ", smooth.age$df)

smooth.year = smooth.spline(x=year, y=logwage, cv=FALSE)
cat("Optimal DF of the smoothing spline for 'year': ", smooth.year$df)

library(gam)

gam.m1 = gam(logwage ~ s(age, df=7.567), data=Wage)
gam.m2 = gam(logwage ~ s(age, df=7.567) + s(year, df=2.804), data=Wage)
gam.m3 = gam(logwage ~ s(age, df=7.567) + s(year, df=2.804) + education, data=Wage)
gam.m4 = gam(logwage ~ s(age, df=7.567) + s(year, df=2.804) + education + health_ins, data=Wage)
gam.m5 = gam(logwage ~ s(age, df=7.567) + s(year, df=2.804) + education + health_ins +
                maritl, data=Wage)
gam.m6 = gam(logwage ~ s(age, df=7.567) + s(year, df=2.804) + education + health_ins +
                maritl + health, data=Wage)
gam.m7 = gam(logwage ~ s(age, df=7.567) + s(year, df=2.804) + education + health_ins +
                maritl + health + jobclass, data=Wage)
gam.m8 = gam(logwage ~ s(age, df=7.567) + s(year, df=2.804) + education + health_ins +
                maritl + health + jobclass + race , data=Wage)

anova(gam.m1, gam.m2, gam.m3, gam.m4, gam.m5, gam.m6, gam.m7, gam.m8, test="F")

par(mfrow = c(3,3))
plot(gam.m7, se=T, col="blue")

ggplot() +
   geom_point(aes(x=gam.m7$fitted.values, y=gam.m7$residuals, color=as.factor(I(wage>250)))) +
   labs(x="Fitted values", y="Residuals", color="'wage' > 250")

library(gridExtra)

dd1 = ggplot() +
   geom_jitter(aes(x=Wage$age , y=gam.m7$residuals), width=0.5) +
   labs(x="age", y="Residuals")

dd2 = ggplot() +
   geom_jitter(aes(x=Wage$year , y=gam.m7$residuals), width=0.5) +
   labs(x="year", y="Residuals")

grid.arrange(dd1, dd2, ncol=2)
dd3 = ggplot() +
   geom_qq(aes(sample=gam.m7$residuals)) +
   labs(x="Theoretical quantiles", y="Sample quantiles") +
   ggtitle("Q-Q plot of residuals")

dd4 = ggplot() +
   geom_histogram(aes(x=gam.m7$residuals), bins=40) +
   labs(x="Residuals", y="Count") +
   ggtitle("Histogram of residuals")

grid.arrange(dd3, dd4, ncol=2)


ggplot() +
   geom_point(aes(x=Wage$logwage, y=gam.m7$fitted.values)) +
   labs(x="logwage", y="Fitted values")

library(htmlTable)

rname = c("year","age","education","health_ins","maritl","health","jobclass","race")
AIC = c(gam.m1$aic, gam.m2$aic, gam.m3$aic, gam.m4$aic, gam.m5$aic, gam.m6$aic,
        gam.m7$aic, gam.m8$aic)
res.deviance = c(gam.m1$deviance, gam.m2$deviance, gam.m3$deviance, gam.m4$deviance,
                 gam.m5$deviance, gam.m6$deviance, gam.m7$deviance, gam.m8$deviance)

feature.m = matrix(rep("No", 8*8), nrow=8)
for(i in c(1:8)) {
   feature.m[i,i:8] = "Yes"
}

rownames(feature.m) = rname
colnames(feature.m) = c("Gam1","Gam2","Gam3","Gam4","Gam5","Gam6","Gam7**","Gam8")
feature.m = rbind(feature.m, round(AIC,1), round(res.deviance,1))

rownames(feature.m)[9:10] = c("AIC", "Res_deviance")

htmlTable(feature.m,
          caption="Table 1: The performance of GAMs used in predicting 'logwage'",
          rgroup = c("Predictor", "Metric"),
          n.rgroup = c(8,2),
          tfoot = paste("** Selected best model"),
          css.cell = "width:80px;")

gam.b1 = gam(I(wage>250) ~ education, family=binomial)
gam.b2 = gam(I(wage>250) ~ education + maritl, data=Wage, family=binomial)
gam.b3 = gam(I(wage>250) ~ education + maritl + jobclass, data=Wage, family=binomial)
gam.b4 = gam(I(wage>250) ~ education + maritl + jobclass + s(age, df=7.567), data=Wage, family=binomial)
gam.b5 = gam(I(wage>250) ~ education + maritl + jobclass + s(age, df=7.567) + health, 
             data=Wage, family=binomial)
gam.b6 = gam(I(wage>250) ~ education + maritl + jobclass + s(age, df=7.567) + health +
                health_ins, data=Wage, family=binomial)
gam.b7 = gam(I(wage>250) ~ education + maritl + jobclass + s(age, df=7.567) + health +
                health_ins + race, data=Wage, family=binomial)
gam.b8 = gam(I(wage>250) ~ education + maritl + jobclass + s(age, df=7.567) + health +
                health_ins + race + s(year, df=2.804), data=Wage, family=binomial)

anova(gam.b1, gam.b2, gam.b3, gam.b4, gam.b5, gam.b6, gam.b7, gam.b8, test="F")

rname = c("education","maritl","jobclass","age","health","health_ins","race","year")

AIC = c(gam.b1$aic, gam.b2$aic, gam.b3$aic, gam.b4$aic, gam.b5$aic, gam.b6$aic,
        gam.b7$aic, gam.b8$aic)
res.deviance = c(gam.b1$deviance, gam.b2$deviance, gam.b3$deviance, gam.b4$deviance,
                 gam.b5$deviance, gam.b6$deviance, gam.b7$deviance, gam.b8$deviance)

feature.m = matrix(rep("No", 8*8), nrow=8)
for(i in c(1:8)) {
   feature.m[i,i:8] = "Yes"
}

rownames(feature.m) = rname
colnames(feature.m) = c("GamB1","GamB2","GamB3","GamB4","GamB5**","GamB6","GamB7","GamB8")
feature.m = rbind(feature.m, round(AIC,1), round(res.deviance,1))

rownames(feature.m)[9:10] = c("AIC", "Res_deviance")

htmlTable(feature.m,
          caption = "Table 2: The performance of GAMs used in predicting binary 'wage' response",
          rgroup = c("Predictor", "Metric"),
          n.rgroup = c(8,2),
          tfoot = paste("** Selected best model"),
          css.cell = "width:80px;")

par(mfrow = c(2,3))
plot(gam.b5, se=T, col="blue")
check1 = sum((Wage$education == "1. < HS Grad") & (Wage$wage > 250))
check2 = sum((Wage$maritl == "3. Widowed") & (Wage$wage > 250))
check3 = sum((Wage$maritl == "4. Divorced") & (Wage$wage > 250))
check4 = sum((Wage$maritl == "5. Separated") & (Wage$wage > 250))

no.HS = which(Wage$education != "1. < HS Grad")
never.married = which(Wage$maritl == "1. Never Married")
married = which(Wage$maritl == "2. Married")
marital = union(never.married, married)

id = intersect(no.HS, marital)
numb.id = length(id)

Wage.rev = Wage[id,]

gam.b5.rev = gam(I(wage>250) ~ education + maritl + jobclass + s(age, df=7.567) + health, 
             data=Wage.rev, family=binomial)

par(mfrow = c(2,3))
plot(gam.b5.rev, se=T, col="blue")
preds = predict(gam.b5.rev, newdata=data.frame(education="5. Advanced Degree", maritl="2. Married", jobclass="2. Information", age=50, health="2. >=Very Good"), type="response")

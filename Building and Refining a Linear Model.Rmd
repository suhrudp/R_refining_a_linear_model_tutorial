# BUILDING AND REFINING A NON-LINEAR MODEL

## **LOAD LIBRARIES**

```{r}
library(ggpubr)
library(nlme)
library(lspline)
library(splines)
library(mgcv)
library(visreg)
library(effects)
```

## **ATTACH DATA**

```{r}
df <- Theoph
attach(df)
View(df)
?Theoph
```

## VISUALIZING DATA

```{r}
ggscatter(data=df, y="conc", x="Time")
```

## BUILDING LINEAR & NON-LINEAR MODELS & ASSESSING EACH FIT

```{r}
fit1 <- lme(data=df, conc ~ Time + Dose + Wt, random = ~1|Subject)
summary(fit1)
visreg(fit1, "Time")
fit2 <- lme(data=df, conc ~ lspline(Time, knots=c(1)) + Dose + Wt, random = ~1|Subject)
summary(fit2)
visreg(fit2, "Time")
fit3 <- lme(data=df, conc ~ lspline(Time, knots=c(2,3,5,7,9,12)) + Dose + Wt, random = ~1|Subject)
summary(fit3)
visreg(fit3, "Time")
fit4 <- lme(data=df, conc ~ lspline(Time, knots=c(1,3,5,7,9,12)) + Dose + Wt, random = ~1|Subject)
summary(fit4)
visreg(fit4, "Time")
fit5 <- lme(data=df, conc ~ ns(Time, knots=c(1,2,5,12)) + Dose + Wt, random = ~1|Subject)
summary(fit5)
visreg(fit5, "Time")
fit5eff <- predictorEffect(fit4, predictor="Time")
plot(fit5eff,rug=F)
fitgam <- gam(conc~s(Time),data=df)
summary(fitgam)
visreg(fitgam)
ggplot(data=df, aes(x=Time, y=predict(fit5), group=Dose, color=Dose)) + geom_smooth(method="gam") + geom_point()
```

## 

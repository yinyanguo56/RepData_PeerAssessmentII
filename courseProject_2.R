library(datasets)

summary(ToothGrowth)

## explore  tooth growth by supp
boxplot(len~supp,data=ToothGrowth, main="len supp Data",
        xlab="supp", ylab="len")

## hypothesis tests and difference interval


## assume equal variance
OJ<- ToothGrowth[ToothGrowth$supp=="OJ",1]
VC<- ToothGrowth[ToothGrowth$supp=="VC",1]
t.test(OJ,VC, var.equal=TRUE, paired=FALSE)

## assume variance are not equal
t.test(ToothGrowth$len~ToothGrowth$supp)

## conclusion:

## explore  tooth growth by dose
boxplot(len~dose,data=ToothGrowth, main="len dose Data",
        xlab="dose", ylab="len")
tapply(ToothGrowth$len, ToothGrowth$dose, mean)

## ANOVA test
Tooth.df =ToothGrowth
Tooth.df$Dosegroup = factor(Tooth.df$dose)
Tooth.mod1 = lm(len ~ Dosegroup, data = Tooth.df)
anova(Tooth.mod1)
summary(Tooth.mod1)

## compare with dose(1) vs dose (2)
mat<- cbind(rep(1/3, 3), "1 vs 2"=c(0,1,-1),"0.5 vs 1"=c(-1,1,0))
mymat <- solve(t(mat))
my.contrast <- mymat[,2:3]
contrasts(Tooth.df$Dosegroup) <- my.contrast
summary.lm(aov(len ~ Dosegroup, data = Tooth.df))

#library(knitr)
#knit2html("courseProject_II.Rmd")



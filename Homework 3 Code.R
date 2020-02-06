
#Calling necessary packages
```{r Getting packages}
library(tidyverse)
library(car)
library(gvlma)
```

#1 
```{r 1}
#Reading in the KIllarney data
lakes <- read_csv("Killarney.csv", col_types = cols(Diversity = "d", Status = "f", Lake = "c"))
lakes
```

#2
```{r 2}
#Generating a Status~Diversity boxplot
a <- ggplot(lakes, aes(x = Status, y = Diversity, fill = Status))
lakesboxplot <- a+geom_boxplot(show.legend = FALSE)+theme_classic()+ylab('Species Diversity')
lakesboxplot
```

There appears to be a difference betweeen acidic and neutral lakes, and likely also a difference between acidic and recovered lakes. There may be a difference between neutral and recovered lakes, but since the boxes overlap, further testing is necessary to determine whether there is in fact a significant difference between the two.

#3
```{r 3}
#Anova among lake types for species diversity
Diversity <- lakes$Diversity
Status <- lakes$Status
lakedivfit <- aov(Diversity~Status)
lakedivfit
```

#4
H0: There is no difference in species diversity among each of the three lake types

#5
```{r 5}
summary(lakedivfit)
```
p-value = 5.43e-06 
Test statistic: F


#6
Since p < 0.05, we reject H0. Species diversity differs among the three lake types.

#7
```{r 7}
TukeyHSD(aov(Diversity~Status))
```

#8
As suspected, there are significant differences between Recovered-Acidic and Neutral-Acidic.

#9
```{r 9}
#Getting residuals#
ldfres <- lakedivfit$residuals
ldfres
#taking a look at the residuals#
scatter.smooth(ldfres)
hist(ldfres)
#Testing for residual normality#
shapiro.test(ldfres)
```

Since p < 0.05, we reject the null hypothesis. The residuals are not normally distributed and these data not not meet the assumptions of ANOVA.

```{r 9 levene}
leveneTest(Diversity, Status)
```

Since p > 0.05, we fail to reject the null hypothsis. The data are homoscedastic.

#10
#a
```{r 10a}
esoph <- read_csv("esoph.csv")
esoph

ncases <- esoph$ncases
Alcohol <- esoph$Alcohol
Tobacco <- esoph$Tobacco

ATanova <- aov(ncases~Alcohol*Tobacco)
ATanova
summary(ATanova)
```
#b
The given p-values for the influence of alcohol, tobacco, and the interaction between the two on esophageal cancer are < 0.05. We fail to reject H0; there is no evidence that these factors alone have an impact on the development of esophageal cancer.

#11
#a
```{r 11a}
Age <- esoph$Age

TAanova <- aov(ncases~Age*Tobacco)
TAanova
summary(TAanova)
```
#b
TukeyHSD(aov(TAanova))

#c
The ANOVA results indicate taht p < 0.05 for the effects of age and tobacco consumption individually on the development of esophageal cancer, but no significant interaction term. The Tukey post-hoc test identified significant differences among the most disparate tobacco classes, and numerous significant differences in the effect of age among the 55-64 and 65-74 classes and the other classes. Collectively, these results indicate that middle-aged people with high tobacco consumption are at greater risk than younger individuals with low tobacco consumption. 

#12
#a
```{r 12a}
AAanova <- aov(ncases~Age*Alcohol)
AAanova
summary(AAanova)
```

#b
```{r 12b}
interaction.plot(Age, Alcohol, ncases)
```

#c
The interaction plot indicates that cases of esophageal cancer are most prevalent in individuals between the ages of 45-74 whose alcohol consumption is moderate. The significance of this interaction is supported by the ANOVA results which indicate a significant interaction between alcohol consumption and age. The frequency of esophageal cancer cannot be explained by either variable alone. Regardless of alcohol consumption, very old and very young individuals have a lower frequency of esophageal cancer than intermediate age classes. 

#13

#a
```{r 13a}
b <- ggplot(mtcars, aes(wt, mpg))
wtmpg_scatter <- b + geom_point()+theme_classic()+xlab('Vehicle Weight (tons)')+ylab('Fuel Efficiency (miles/gallon)')
```

#b
```{r 13b}
wt <- mtcars$wt
mpg <- mtcars$mpg

wtmpg_glm <- glm(mpg~wt)
summary.lm(wtmpg_glm)
qt(0.05, 30)
```
Since |tobs| > |tcrit|, we reject H0. There is evidence for a significant relationship between weight and gas mileage. In other words, weight does have an influence on fuel efficiency.


#c
```{r 13c}
#adding a fit line
bluefit <- wtmpg_scatter <- b + geom_point()+theme_classic()+xlab('Vehicle Weight (tons)')+ylab('Fuel Efficiency (miles/gallon)')+geom_smooth(method='lm')
bluefit
```

#d
p-value: 1.29e-10
Since p<0.05, we reject H0. Gas mileage is significantly related to car weight.


#14
#a
```{r 14a}
#making objects for each column#
hp <- mtcars$hp
qsec <- mtcars$qsec
#creating the ggplot object and plotting the scatterplot#
c <- ggplot(mtcars, aes(hp, qsec))
qsechp_scatter <- c + geom_point()+theme_classic()+xlab('Horsepower')+ylab('Quarter Mile Time (s)')
qsechp_scatter
```

#b
```{r 14b}
qsechp_glm <- glm(qsec~hp)
summary.lm(qsechp_glm)
```

#c
```{r 14c}
qsechpline_scatter <- c + geom_point()+theme_classic()+xlab('Horsepower')+ylab('Quarter Mile Time (s)')+geom_smooth(method = lm)
qsechpline_scatter
```
Equation: Quater Mile Time = 20.556 - 0.0.18(Horsepower)

#d
20.566 - 0.018*300


#e
p-value: 5.77e-06
Since p < 0.05, we reject H0. There is a significant relationship between horsepower and quarter mile time.

#f
```{r 14f}
assumptions_qsechp <-gvlma(lm(qsechp_glm))
assumptions_qsechp
```
This data meets all regression assumptions except Kurtosis.

#15
#a
```{r 15a}
#These data are the forearm length measurements of captured bats and the height(m) at which they were captured. If wing length is a predictor of flying strength, then I predict that individuals with larger wings will be caught a greater height.
HF <- read_csv("HeightForearm.csv")
```

#15b
```{r 15b}
d <- ggplot(HF, aes(Forearm, Height))
HF_scatter <- d + geom_point()+theme_classic()+xlab('Forearm Length (mm)')+ylab('Capture Height (m)')+geom_smooth(method = lm)
HF_scatter
```

#15c
```{r 15c}
HF_glm <- glm(HF$Height~HF$Forearm)
summary.lm(HF_glm)
sqrt(0.0007458)
```
The correlation coefficient is 0.02730934.

#d
p-value: 0.792
Since p >>> 0.05, we fail to reject the null hypothesis. There is no evidence for a relationship between forearm (wing) length and capture height.

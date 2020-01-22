setwd("\\Users\\Asvin\\Documents\\RData\\STAD37")
lifeex <- read.table("lifeex.txt",header = TRUE,  dec = ".")
#names(lifeex) <- c("Country", "Life Expectancy of a 0 Year Old Man", "Life Expectancy of a 25 Year Old Man", "Life Expectancy of a 50 Year Old Man", "Life Expectancy of a 75 Year Old Man", "Life Expectancy of a 0 Year Old Woman", "Life Expectancy of a 25 Year Old Woman", "Life Expectancy of a 50 Year Old Woman", "Life Expectancy of a 75 Year Old Woman")
names(lifeex) <- c("Country", "0m", "25m", "50m", "75m", "0w", "25w", "50w", "75w")
R <- cor(lifeex[,-1])
R
model.mle1 <- factanal(lifeex[,-1], factors = 1, rotation = "varimax")
model.mle1
model.mle2 <- factanal(lifeex[,-1], factors = 2, rotation = "varimax")
model.mle2
model.mle3 <- factanal(lifeex[,-1], factors = 3, rotation = "varimax")
model.mle3
# factor 3 fits well
# factor 1 measures life expectancy at birth
# life expectancy for older women
# life expenctancy for older men
nr.mle3 <- factanal(lifeex[,-1], factors = 3, rotation = "none")
nr.mle3
# calculate communalities, specific variances and residuls for no rotation maximum likelihood 3 factors
L3 <- nr.mle3$load 
# Communalities
h2 <- apply(L3^2, 1, sum)
h2
# Specific Variance
Psi <- nr.mle3$unique
Psi
# Residuals
R-L3%*%t(L3)-diag(Psi)
# calculate communalities, specific variances and residuls for varimax maximum likelihood 3 factors
LV3 <- model.mle3$load 
# Communalities
h2 <- apply(LV3^2, 1, sum)
h2
# Specific Variance
Psi <- model.mle3$unique 
Psi
# Residuals
R-LV3%*%t(LV3)-diag(Psi)
# perform factor analyis using principal component method with varimax rotation and 3 common factors
installed.packages("psych")
library(psych)

fit.pc1 <- principal(lifeex[,-1], nfactors=3, rotate="varimax", method="regression")
fit.pc1
# perform factor analysis for 3 factors using the regression method
model.reg3 <- factanal(lifeex[,-1], factors = 3, rotation = "varimax", score = "regression")
model.reg3
# get estimated scores for the factor model using regression method
model.reg3$scores
# sort countries by values of first factor scores
regscfac1 <- model.reg3$scores[,1]

test1=data.frame(lifeex[ ,1], regscfac1)
test1[order(test1[ ,2]), ]
# sort countries by values of second factor scores
regscfac2 <- model.reg3$scores[,2]

test2=data.frame(lifeex[ ,1], regscfac2)
test2[order(test2[ ,2]), ]
# sort countries by values of third factor scores
regscfac3 <- model.reg3$scores[,3]

test3=data.frame(lifeex[ ,1], regscfac3)
test3[order(test3[ ,2]), ]
# scatterplot of factors 1 and 2 in the regression factor analysis model
plot(model.reg3$scores[,1:2], pch=20, col="blue")
text(model.reg3$scores[ ,1], model.reg3$scores[ ,2], model.reg3[ ,1], cex=0.5, pos=3)
# qq plot for 1st factor
qqnorm(model.reg3$scores[,1])
# qq plot for 2nd factor
qqnorm(model.reg3$scores[,2])
# qq plot for 3rd factor
qqnorm(model.reg3$scores[,3])

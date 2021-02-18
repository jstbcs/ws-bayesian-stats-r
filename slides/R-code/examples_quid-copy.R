# load libraries
library(devtools)
install_github("lukasklima/quid")
library(quid)

# check what arguments the function takes
args(quid:::constraintBF)

## Stroop
# inspect the data
str(stroop)
?stroop

# perform analysis: Stroop
resStroop <- quid:::constraintBF(formula = rtS ~ ID*cond,
                                 data = stroop,
                                 whichRandom = "ID",
                                 ID = "ID",
                                 whichConstraint = c(cond = "2 > 1"),
                                 rscaleEffects = c("ID" = 1, "cond" = 1/6, "ID:cond" = 1/10))

resStroop

# Plot the estimates

means <- tapply(stroop$rtS, list(stroop$cond, stroop$ID), mean)
obs.eff <- means[2, ] - means[1, ]
est.eff <- resStroop@individualEffects$cond_2 - resStroop@individualEffects$cond_1
ord.id <- order(obs.eff)

par(cex = 1.5, mar = c(3,3,1,1), mgp = c(2, .7, 0))
plot(obs.eff[ord.id], pch = 20,col = adjustcolor("slateblue", .7)
     , ylab = "Stroop Effect", xlab = "Participant")
abline(h = 0)
points(est.eff[ord.id], pch = 20, col = adjustcolor("firebrick", .7))

## LD5
# inspect data
str(ld5)
?ld5

# analysis
resLD5 <- quid:::constraintBF(formula = rt ~ sub * distance + side,
                               data = ld5,
                               whichRandom = c("sub"),
                               ID = "sub",
                               whichConstraint = c(distance = "1 > 2", distance = "2>3"),
                               rscaleEffects = c("sub" = 1,
                                                 "side" = 1/6,
                                                 "distance" = 1/6,
                                                 "sub:distance" = 1/10))

resLD5

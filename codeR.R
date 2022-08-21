#--------------------------------------
# EXAMPLE 1: NUMBER OF VISITS TO DOCTOR
# -------------------------------------

# GWRM fit with a stepwise algorithm
install.packages("GWRM")
install.packages("COUNT")
library(GWRM)
library(COUNT)
data(badhealth)
badhealth.gw0 <- gw(numvisit ~ 1, data = badhealth)
summary(badhealth.gw0)
badhealth.finalgw <- step(badhealth.gw0, scope = ~ badh + age, data = badhealth)
summary(badhealth.finalgw)

# Poisson fit
badhealth.pois <- glm(numvisit ~ badh + age, family = poisson, data = badhealth)
summary(badhealth.pois)

# NegbinII fit
install.packages("MASS")
library(MASS)
badhealth.nb <- glm.nb(numvisit ~ badh + age, data = badhealth)
summary(badhealth.nb)

# Predictions by GWRM model
badh.0 <- subset(badhealth, badh == 0)
predictions.finalgw.0 <- predict(badhealth.finalgw, newdata = badh.0)
predictions.finalgw.0[1,]

badh.1 <- subset(badhealth, badh == 1)
predictions.finalgw.1 <- predict(badhealth.finalgw, newdata = badh.1)
predictions.finalgw.1[1,]

# Partition of variance: without covariate age
partvar(badhealth.finalgw)
partvar.finalgw.0 <- partvar(badhealth.finalgw, newdata = badh.0)
partvar.finalgw.0$Prop.Variance[1,]

partvar.finalgw.1 <- partvar(badhealth.finalgw, newdata = badh.1)
partvar.finalgw.1$Prop.Variance[1,]

# Partition of variance: with covariate age
badhealth.gw <- gw(numvisit ~ badh + age, data = badhealth)# Fitting the GWRM model with covariate age
summary(badhealth.gw)
partvar(badhealth.gw)

badh.0 <- with(badh.0, badh.0[order(age, decreasing=FALSE), ])# Sorting data by age
partvar(badhealth.gw, newdata = unique(badh.0[,2:3]))

badh.1 <- with(badh.1, badh.1[order(age, decreasing=FALSE), ])# Sorting data by age
partvar(badhealth.gw, newdata = unique(badh.1[,2:3]))

# Simulated envelope of residuals
set.seed(123)# To reproduce the same plot of the paper
res <- residuals(badhealth.finalgw, type = "deviance", envelope = TRUE, parallel = FALSE, rep = 99) #set.seed() is not appropriate with parallel computing
env <- data.frame(res$residuals, apply(res$sim.residuals[,-1], 1, min), apply(res$sim.residuals[,-1], 1, max))
sum((env[,1] < env[,2])|(env[,1] > env[,3]))# Points outside the envelope


#----------------------------------
# EXAMPLE 2: NUMBER OF GOALS SCORED
# ---------------------------------

# Fits
Spain <- read.table("Spain.txt")
library(GWRM)
library(MASS)
gw.fits <- list()
nb.fits <- list()
for (i in 1:10){
  gw.fits[[i]] <- gw(Goals ~ Position + offset(log(Matches)), data = Spain[Spain$Season == levels(Spain$Season)[i], ])
  nb.fits[[i]] <- glm.nb(Goals ~ Position + offset(log(Matches)), data = Spain[Spain$Season == levels(Spain$Season)[i], ])
}

# Goodness of fit comparison
print(data.frame(Season = levels(Spain$Season), k = sapply(gw.fits, function(data) coef(data)[4]), ro = sapply(gw.fits, function(data) coef(data)[5]), AIC.gw = sapply(gw.fits, function(data) AIC(data)), AIC.nb = sapply(nb.fits, function(data) AIC(data)), Improvement = - sapply(gw.fits, function(data) AIC(data)) + sapply(nb.fits, function(data) AIC(data))))

# Boxplot of GWRM parameter estimates in the nine fitted seasons
boxplot(t(sapply(gw.fits, function(l) l$coefficients))[, c(1,3,2,4,5)], sd = TRUE, names = c("Intercept", "Position:Midfielder", "Position:Forward", expression(hat(k)), expression(hat(rho))), eyes = FALSE)

# Simulated envelope in the first season
set.seed(123)# To reproduce the same plot of the paper
res <- residuals(gw.fits[[1]], parallel = FALSE, envelope = TRUE, rep = 99) #set.seed() is not appropriate with parallel computing
env <- data.frame(res$residuals, apply(res$sim.residuals[,-1], 1, min), apply(res$sim.residuals[,-1], 1, max))
sum((env[,1] < env[,2])|(env[,1] > env[,3]))# Points outside the envelope

# Partition of variance along the ten fitted seasons. Boxplots
forward.levels <- data.frame(Position = c("Forward"), Matches = 1:38)
midfielder.levels <- data.frame(Position = c("Midfielder"), Matches = 1:38)
defender.levels <- data.frame(Position = c("Defender"), Matches = 1:38)
forward.partvar <- list()
midfielder.partvar <- list()
defender.partvar <- list()
for (i in 1:10){
  forward.partvar[[i]] <- partvar(gw.fits[[i]], newdata = forward.levels)
  midfielder.partvar[[i]] <- partvar(gw.fits[[i]], newdata = midfielder.levels)
  defender.partvar[[i]] <- partvar(gw.fits[[i]], newdata = defender.levels)
}
forward.rand <- t(data.frame(lapply(forward.partvar, function(l) l$Prop.Variance.Components$Randomness)))
fr.box <- boxplot(forward.rand, sd = TRUE, eyes = FALSE, ylim = c(0,1), xlab = "Number of matches played", ylab = "Randomness (%)", main = "Forward")
lines(fr.box$stats[3,], col = 2, lwd = 2)
forward.liab <- t(data.frame(lapply(forward.partvar, function(l) l$Prop.Variance.Components$Liability)))
fl.box <- boxplot(forward.liab, sd = TRUE, eyes = FALSE, ylim = c(0,1), xlab = "Number of matches played", ylab = "Liability (%)", main = "Forward")
lines(fl.box$stats[3,], col = 2, lwd = 2)
forward.pron <- t(data.frame(lapply(forward.partvar, function(l) l$Prop.Variance.Components$Proneness)))
fp.box <- boxplot(forward.pron, sd = TRUE, eyes = FALSE, ylim = c(0,1), xlab = "Number of matches played", ylab = "Proneness (%)", main = "Forward")
lines(fp.box$stats[3,], col = 2, lwd = 2)
midfielder.rand <- t(data.frame(lapply(midfielder.partvar, function(l) l$Prop.Variance.Components$Randomness)))
mr.box <- boxplot(midfielder.rand, sd = TRUE, eyes = FALSE, ylim = c(0,1), xlab = "Number of matches played", ylab = "Randomness (%)", main = "Midfielder")
lines(mr.box$stats[3,], col = 2, lwd = 2)
midfielder.liab <- t(data.frame(lapply(midfielder.partvar, function(l) l$Prop.Variance.Components$Liability)))
ml.box <- boxplot(midfielder.liab, sd = TRUE, eyes = FALSE, ylim = c(0,1), xlab = "Number of matches played", ylab = "Liability (%)", main = "Midfielder")
lines(ml.box$stats[3,], col = 2, lwd = 2)
midfielder.pron <- t(data.frame(lapply(midfielder.partvar, function(l) l$Prop.Variance.Components$Proneness)))
mp.box <- boxplot(midfielder.pron, sd = TRUE, eyes = FALSE, ylim = c(0,1), xlab = "Number of matches played", ylab = "Proneness (%)", main = "Midfielder")
lines(mp.box$stats[3,], col = 2, lwd = 2)
defender.rand <- t(data.frame(lapply(defender.partvar, function(l) l$Prop.Variance.Components$Randomness)))
dr.box <- boxplot(defender.rand, sd = TRUE, eyes = FALSE, ylim = c(0,1), xlab = "Number of matches played", ylab = "Randomness (%)", main = "Defender")
lines(dr.box$stats[3,], col = 2, lwd = 2)
defender.liab <- t(data.frame(lapply(defender.partvar, function(l) l$Prop.Variance.Components$Liability)))
dl.box <- boxplot(defender.liab, sd = TRUE, eyes = FALSE, ylim = c(0,1), xlab = "Number of matches played", ylab = "Liability (%)", main = "Defender")
lines(dl.box$stats[3,], col = 2, lwd = 2)
defender.pron <- t(data.frame(lapply(defender.partvar, function(l) l$Prop.Variance.Components$Proneness)))
dp.box <- boxplot(defender.pron, sd = TRUE, eyes = FALSE, ylim = c(0,1), xlab = "Number of matches played", ylab = "Proneness (%)", main = "Defender")
lines(dp.box$stats[3,], col = 2, lwd = 2)

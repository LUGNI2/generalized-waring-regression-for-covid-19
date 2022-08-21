library(GWRM)
library(COUNT)
data(badhealth)
badhealth.gw0 = gw(numvisit~1, data = badhealth)
badhealth.finalgw = step(badhealth.gw0,scope =~badh + age,
                         data = badhealth)

badhealth.pois <- glm(numvisit~ badh + age, family = poisson,
                      data = badhealth)
summary(badhealth.pois)
library(MASS)
badhealth.nb <- glm.nb(numvisit~ badh + age, data = badhealth)
summary(badhealth.nb)


badh.0=subset(badhealth,badh == 0)
badh.1=subset(badhealth,badh == 1)
predictions.finalgw.0 =predict(badhealth.finalgw,newdata = badh.0)
predictions.finalgw.0[1,]
predictions.finalgw.1=predict(badhealth.finalgw,newdata = badh.1)
predictions.finalgw.1[1,]

pv=partvar(badhealth.finalgw)
pv$Prop.Variance.Components
pv$Variance.Components

partvar.finalgw.0 <- partvar(badhealth.finalgw, newdata = badh.0)
partvar.finalgw.0$Prop.Variance[1,]

partvar.finalgw.1 <- partvar(badhealth.finalgw, newdata = badh.1)
partvar.finalgw.1$Prop.Variance[1,]

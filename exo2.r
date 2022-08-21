tailpop=c(92396,97768,113745,154790,142764,73961,59999)
retour =c(512,437,559,619,494,245,239)
rigueur=c(4520,4279,4233,3935,4814,4034,4536)
df =data.frame(tailpop,retour,rigueur)
poisson=glm(retour~rigueur+offset(log(tailpop)), family=poisson,data =df)
            
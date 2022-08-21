
pr = glm(cas ~contact+importe+communautaire+offset(log(tests)),
          family = poisson, data = dt_train)

nbr = glm.nb(cas ~contact+importe+communautaire+offset(log(tests)),
             data = dt_train)

gwr = gw(cas ~contact+importe+communautaire, offset = log(tests), data=dt_train)
gwr = gw(cas ~contact+importe+communautaire+offset(log(tests)), data=dt_train)

pr_final = step(pr, direction ="backward", trace = T, k=2)
nb_final =step(nbr, direction ="backward", trace = T, k=2)
gwr_final =step(gwr,direction ="backward", trace = T, k=2)

#Fitting the models---
prm = glm(cas ~1+offset(log(tests)), family = poisson, data = dt_train)
nbrm = glm.nb(cas~1+offset(log(tests)), data = dt_train)
gwrm = gw(cas~1+offset(log(tests)),data=dt_train)

#Variable selection  with aic criterion ----
scope = ~contact+importe+communautaire

prm_final <- step(prm, scope = scope)
nbrm_final<- step(nbrm, scope = scope)
gwrm_final<- step(gwrm, scope = scope)

pv = partvar.gwrm$Prop.Variance.Components
pv$day = seq(1,dim(pv)[1],by=1)

ggplot(pv,aes(x = day)) + 
  geom_line(aes(y = Randomness),color = "darkred",size=1,linetype="solid") +
  ylab("Source of variabiity")+
  labs(fill = "Randomness")+
  geom_line(aes(y = Liability,),color = "darkblue",size=1,linetype="solid")+
  labs(fill = "Liability")+
  geom_line(aes(y = Proneness),color = "black",size=1,linetype="solid")+
  labs(fill = "Proneness")



ggplot(data=pv)+
  geom_line(mapping=aes(y=Randomness,x= day, linetype="Randomness"),size=1,color='blue') +
  geom_line(mapping=aes(y=Liability,x= day, linetype="Liability"),size=1,color='red')+
  geom_line(mapping=aes(y=Proneness,x= day, linetype="Proneness"),size=1,color="black") +
  scale_linetype_manual(values = c(
    'Randomness' = 'solid',
    'Liability' = 'twodash',
    'Proneness' = 'dotted')) +
  labs(linetype = 'Source of variability')

ggplot(data=pv)+
  ylab("Source of variabiity")+
  geom_line(mapping=aes(y=Randomness,x= day, linetype="randomness"),size=1) +
  geom_line(mapping=aes(y=Liability,x= day, linetype="liability"),size=1)+
  geom_line(mapping=aes(y=Proneness,x= day, linetype="proneness"),size=1) +
  scale_linetype_manual(values = c(
    'randomness' = 'solid',
    'liability' = 'twodash',
    'proneness' = 'dotted')) +
  labs(linetype = 'Source of variability')
 


ggplot(data=pv)+
  geom_line(mapping=aes(y=Randomness,x= day, color="Randomness"),size=1,linetype="solid") +
  geom_line(mapping=aes(y=Liability,x= day, color="Liability"),size=1,linetype="longdash")+
  geom_line(mapping=aes(y=Proneness,x= day, color="Proneness"),size=1,linetype="dotted") +
  scale_color_manual(values = c(
    'Randomness' = 'darkred',
    'Liability' = 'darkblue',
    'Proneness' = 'black')) +
  scale_linetype_manual(values=
    c("solid","longdash","dotted")
                        )+
    labs(color = 'Source')



nb=glm.nb(cas ~contact+importe+communautaire+log(tests)
          +offset(log(tests)), data = dt_train)
poi =glm(cas ~contact+importe+communautaire+log(tests)+offset(log(tests)),
         family = poisson, data = dt_train)
gwr = gw(cas ~contact+importe+communautaire+(log(tests)),offset = log(tests), data=dt_train)

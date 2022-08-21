#Preparation des donnees

rm(list = ls())
library(tidyverse)
library(MASS)
library(prettyR)
library(GWRM)
library(caret)
library(gridExtra)
library(cowplot)

Sys.setlocale("LC_TIME","English")
df = read.csv2("confirmes.csv",sep = ",")
df$date = as.Date(df$date,format = "%Y-%m-%d")
describe(df,num.desc = c("mean","median","var","sd","valid.n"),xname = NA,horizontal = FALSE)

reverse = function(x){
  x = x[seq(length(x),1)]
  return(x)
}

reversed = function(dat){
  dt = dat
  for (i in 1:ncol(dat)) {
    dt[,i] = reverse(dat[,i])
  }
  return(dt)
}

# modification the data  ----

modif = function(dat, delay){
  
  d1 = dat[c("date", "tests", "cas", "contact", "importe", 
             "communautaire","total","gueri",	"mort",	"evacue",
             "grave")]
  
  sdim = nrow(d1) - delay
  d2 = tail(dat, n = sdim)
  d2 = d2[c("cas", "contact", "importe", "communautaire","total"
            ,"gueri","mort",	"evacue",	"grave")]
  d1 = d1[c("date","tests","cas")]
  
  d1 = head(d1, n = sdim)  
  d1 = cbind(d1,d2)
  names(d1)[4] = "cas_p"
  return(d1)
}

d1 = as.Date("2020-05-11")
#toupper(format(as.Date(date),format="%b%d%Y"))

df$ratio = df$cas/df$tests
#visualisation of the model 
total_plot = ggplot(df) + 
  geom_line(data = df, aes(x = date, y = total), size = 1, color = "red") +
  xlab('Date') + 
  ylab('Cumulative number of cases') 

cas_plot = ggplot(df) + 
geom_line(data = df, aes(x = date, y = cas) , size = 1,color = "blue") +
  xlab('Date') + 
  ylab('New daily cases') 

grid.arrange(cas_plot, total_plot, nrow = 2)

dz = na.omit(df)  
ratio_plot = ggplot(dz) + 
  geom_line(data = dz, aes(x = date, y = ratio) , size = 1,color = "black") +
  xlab('Date') + 
  ylab('daily cases / daily screen tests') 

#imputation
df$cas[which(df$date == d1)] = 95
#deletion of incompelete information NA
df  = df[complete.cases(df),]
#deletion of 14th first days
df = head(df,dim(df)[1] - 14)

dt = modif(df, delay = 3)
dt = reversed(dt)

#deletion of incompelete information NA
#df =df[complete.cases(df),]
#df = reversed(df)

#df = df[-70,]

#split the data on training and test data


ind = sample(2, nrow(dt), replace = T, prob = c(0.9,0.1))
dt_train = dt[ind == 1,]  # training = 80%
dt_test  = dt[ind == 2,]  #validation = 20%

#testingSet_size = round(0.1*dim(dt)[1])
#trainingSet_size = dim(dt)[1]-testingSet_size
#dt_test  = head(dt,testingSet_size)  #validation = 20%
#dt_train = tail(dt,trainingSet_size)  # training = 80%

# Fitting the models

prm = glm(cas ~contact+importe+communautaire+offset(log(tests)),
         family = poisson, data = dt_train)

nbrm = glm.nb(cas ~contact+importe+communautaire+offset(log(tests)),
             data = dt_train)

gwrm = gw(cas~contact + importe + communautaire, offset = log(tests), data = dt_train)

predictPRM  = predict(prm, newdata = dt_test,type = "response")
predictNBRM = predict(nbrm, newdata = dt_test,type = "response")
predictGWRM = predict(gwrm, newdata = dt_test)
names(predictGWRM) = "GWRM"
dt_test = cbind(dt_test, PRM = predictPRM, NBRM = predictNBRM,
              GWRM = predictGWRM)
dt_test[,c("date","cas","GWRM","PRM","NBRM")]

plotPrediction = ggplot(data = dt_test) +
  xlab("Date") +
  ylab("Daily new cases") +
  geom_line(mapping = aes(x = date,y = PRM, color = "PRM"),size = 1) +
  geom_line(mapping = aes(x = date,y = NBRM, color = "NRM"),size = 1) +
  geom_line(mapping = aes(x = date,y = GWRM,color = "GWRM"),size = 1) +
  geom_line(mapping = aes(x = date,y = cas,color = "Observed"),size = 1) +
  scale_color_manual(values = c(
    'PRM' = 'blue',
    'NRM' = 'green',
    'GWRM' = 'red',
    'Observed' = 'black')) +
  labs(color = 'Daily new cases')
plotPrediction





comparaison = data.frame(AIC = c(AIC(prm), AIC(nbrm), AIC(gwrm)),
                BIC = c(BIC(prm), BIC(nbrm), BIC(gwrm)),
      logLik = c(logLik(prm), logLik(nbrm), logLik(gwrm))
)

#Validation
deviance = residuals(gwrm, data = dt_test, type = "deviance", envelope = TRUE,
title="Simulated Envelope of deviance residuals",rep = 99)
pearson=residuals(gwrm, type = "pearson", envelope = TRUE, 
title="Simulated Envelope of Pearson residuals", rep = 99)
response= residuals(gwrm, type = "response", envelope = TRUE,
title="Simulated Envelope of Response residuals", rep = 99)
library(png)
imagedeviance=readPNG("deviance.png")
imagepearson = readPNG("pearson.png")
imageresponse = readPNG("response.png")
library(grid)
grid.arrange(rasterGrob(imagedeviance),rasterGrob(imagepearson),
             rasterGrob(imageresponse), ncol=3)

#visualisation of the model 
ggplot(dt_test,aes(x = date, y = cas )) + 
  geom_point(color = "darkblue", size = 3, alpha = 0.3) +
  geom_line(data = dt_test, aes(x = date, y= GWRM), size=1,color="red") 


#propotion of the variance
dt_test =df
partvar.gwrm = partvar(gwrm,dt_test)
pv = partvar.gwrm$Prop.Variance.Components
#pv$day = seq(1,dim(pv)[1],by=1)
pv$date =dt_test$date
pv$cas =dt_test$cas
pv$tests = dt_test$tests
pv$contact = dt_test$contact
pv$communautaire =dt_test$communautaire
pv$importe=dt_test$importe
pv$total = dt_test$total

ggplot(data=pv)+
  xlab("New daily COVID-19 cases")+
  ylab("Proportion of variance for each component")+
  geom_line(mapping=aes(y=Randomness,x= dt_test$cas, color="randomness"),size=1) +
  geom_line(mapping=aes(y=Liability,x= dt_test$cas, color="liability"),size=1)+
  geom_line(mapping=aes(y=Proneness,x= dt_test$cas, color="proneness"),size=1) +
  scale_color_manual(values = c(
    'randomness' = 'blue',
    'liability' = 'red',
    'proneness' = 'black')) +
  labs(color = 'Source of variability')

ggplot(data=pv)+
  xlab("Date")+
  ylab("Proportion of variance for each component")+
  geom_line(mapping=aes(y=Randomness,x= dt_test$date, color="randomness"),size=1) +
  geom_line(mapping=aes(y=Liability,x= dt_test$date, color="liability"),size=1)+
  geom_line(mapping=aes(y=Proneness,x= dt_test$date, color="proneness"),size=1) +
  scale_color_manual(values = c(
    'randomness' = 'blue',
    'liability' = 'red',
    'proneness' = 'black')) +
  labs(color = 'Source of variability')

#propotion of the variance -- tests
gptests =ggplot(data=pv)+
  xlab("Daily number of screening tests")+
  ylab("Source of variability")+
  geom_line(mapping=aes(y=Randomness,x= tests, color="randomness"),size=1) +
  geom_line(mapping=aes(y=Liability,x= tests, color="liability"),size=1)+
  geom_line(mapping=aes(y=Proneness,x= tests, color="proneness"),size=1) +
  scale_color_manual(values = c(
    'randomness' = 'blue',
    'liability' = 'red',
    'proneness' = 'black')) +
  labs(color= 'Source of variability')

#propotion of the variance --  contact
gpcontact=ggplot(data=pv)+
  xlab("Daily number of contact cases")+
  ylab("Source of variability")+
  geom_line(mapping=aes(y=Randomness,x= contact, color="randomness"),size=1) +
  geom_line(mapping=aes(y=Liability,x= contact, color="liability"),size=1)+
  geom_line(mapping=aes(y=Proneness,x= contact, color="proneness"),size=1) +
  scale_color_manual(values = c(
    'randomness' = 'blue',
    'liability' = 'red',
    'proneness' = 'black')) +
  labs(color = 'Source of variability')

#propotion of the variance --  communautaire
gpcommu= ggplot(data=pv)+
  xlab("Daily number of community cases")+
  ylab("Source of variability")+
  geom_line(mapping=aes(y=Randomness,x=communautaire, color="randomness"),size=1) +
  geom_line(mapping=aes(y=Liability,x=communautaire, color="liability"),size=1)+
  geom_line(mapping=aes(y=Proneness,x=communautaire, color="proneness"),size=1) +
  scale_color_manual(values = c(
    'randomness' = 'blue',
    'liability' = 'red',
    'proneness' = 'black')) +
  labs(color = 'Source of variability')

#propotion of the variance --  importe
gpimporte=ggplot(data=pv)+
  xlab("Daily number of imported cases")+
  ylab("Source of variability")+
  geom_line(mapping=aes(y=Randomness,x=importe, color="randomness"),size=1) +
  geom_line(mapping=aes(y=Liability,x=importe, color="liability"),size=1)+
  geom_line(mapping=aes(y=Proneness,x=importe, color="proneness"),size=1) +
  scale_color_manual(values = c(
    'randomness' = 'blue',
    'liability' = 'red',
    'proneness' = 'black')) +
  labs(color = 'Source of variability')

grid.arrange(gptests,gpcontact,nrow=2)
grid.arrange(gpcommu ,gpimporte, nrow=2)

#propotion of the variance --  total
ggplot(data=pv)+
  xlab("Total cases")+
  ylab("Source of variabiity")+
  geom_line(mapping=aes(y=Randomness,x=total, linetype="randomness"),size=1) +
  geom_line(mapping=aes(y=Liability,x=total, linetype="liability"),size=1)+
  geom_line(mapping=aes(y=Proneness,x=total, linetype="proneness"),size=1) +
  scale_linetype_manual(values = c(
    'randomness' = 'solid',
    'liability' = 'twodash',
    'proneness' = 'dotted')) +
  labs(linetype = 'Source of variability')

## Models comparisaon using Bootstrap
set.seed(123)
folds <- createFolds(1:nrow(dt))
predictions.prm <- NULL
predictions.nbrm <- NULL
predictions.gwrm <- NULL

for (foldid in 1:length(folds))
{
  fold = folds[[foldid]] # Select the current fold
  TrainingData = dt[-fold,] # Subset data to everything except our one fold
  ValidationData = dt[fold,] # Subset to our fold
  
  #Poisson Regression model
  #pr.model = glm(cas ~ contact + communautaire + importe + offset(log(tests)), family = poisson, data = TrainingData) 
  pr.model = prm
  predictions.prm = c(predictions.prm, predict(pr.model, ValidationData))
  
  #Negative binomial regression model
  #nbr.model <- glm.nb(cas ~ contact + communautaire + importe + offset(log(tests)), data = TrainingData)
  nbr.model = nbrm
  predictions.nbrm = c(predictions.nbrm, predict(nbr.model, ValidationData))
  
  #Generalized waring regression model
  #gwr.model = gw(cas ~ contact + communautaire + importe + offset(log(tests)),data = TrainingData)
  gwrm.model = gwrm
  predictions.gwrm = c(predictions.gwrm, predict(gwrm.model, ValidationData)[["fit"]])
}

any(duplicated(names(predictions.prm))) # All unique, proof that we did not miss anything
dt$predictions.prm = predictions.prm[order(unlist(folds))] # Optional: save predictions as another column. Convenient

# Statistics
RMSE.prm = sqrt(mean((dt$predictions.prm - dt$cas)^2)) # Around 1
MAE.prm = mean(abs(dt$predictions.prm - dt$cas))
ME.prm = mean(dt$predictions.prm - dt$cas)

any(duplicated(names(predictions.nbrm))) # All unique, proof that we did not miss anything
dt$predictions.nbrm = predictions.nbrm[order(unlist(folds))] # Optional: save predictions as another column. Convenient

# Statistics
RMSE.nbrm = sqrt(mean((dt$predictions.nbrm - dt$cas)^2)) # Around 1
MAE.nbrm = mean(abs(dt$predictions.nbrm - dt$cas))
ME.nbrm = mean(dt$predictions.nbrm - dt$cas)

any(duplicated(names(predictions.gwrm))) # All unique, proof that we did not miss anything
dt$predictions.gwrm = predictions.gwrm[order(unlist(folds))] # Optional: save predictions as another column. Convenient

# Statistics
RMSE.gwrm = sqrt(mean((dt$predictions.gwrm - dt$cas)^2)) # Around 1
MAE.gwrm = mean(abs(dt$predictions.gwrm - dt$cas))
ME.gwrm = mean(dt$predictions.gwrm - dt$cas)


dx = data.frame(RMSE=c(RMSE.gwrm, RMSE.nbrm, RMSE.prm),
           MAE=c(MAE.gwrm, MAE.nbrm, MAE.prm),
           ME=c(ME.gwrm, ME.nbrm, ME.prm)
           )






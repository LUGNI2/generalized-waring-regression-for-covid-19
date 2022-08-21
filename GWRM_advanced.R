#Preparation des donnees

rm(list = ls())
library(tidyverse)
library(MASS)
library(prettyR)
library(GWRM)
library(caret)
library(gridExtra)
library(cowplot)

df = read.csv2("confirmes.csv",sep = ",")
df$date = as.Date(df$date,format="%Y-%m-%d")
describe(df,num.desc=c("mean","median","var","sd","valid.n"),xname=NA,horizontal=FALSE)

reverse = function(x){
  x = x[seq(length(x),1)]
  return(x)
}

reversed = function(dat){
  dt = dat
  for(i in 1:ncol(dat)){
    dt[,i] = reverse(dat[,i])
  }
  return(dt)
}

# modification the data  ----

modif = function(dat, delay){
  
  d1 = dat[c("date", "tests", "cas", "contact", "importe", 
             "communautaire","total","gueri",	"mort",	"evacue",
             "grave")]
  
  sdim =nrow(d1)-delay
  d2 = tail (dat, n = sdim)
  d2 = d2[c("cas", "contact", "importe", "communautaire","total"
            ,"gueri","mort",	"evacue",	"grave")]
  d1 = d1[c("date","tests","cas")]
  
  d1 = head(d1, n = sdim)  
  d1 = cbind(d1,d2)
  names(d1)[4] = "cas_p"
  return(d1)
}

d1 = as.Date("2020-05-11")

#visualisation of the model 
total_plot = ggplot(df)+ 
  geom_line(data=df, aes(x = date, y = total), size= 1, color="red")+
  xlab('Date') + 
  ylab('Cumulative number of cases') 

cas_plot = ggplot(df)+ 
  geom_line(data = df, aes(x = date, y= cas) , size=1,color="darkblue")+
  xlab('Date') + 
  ylab('Daily new cases') 
grid.arrange(cas_plot, total_plot, ncol=2)



#imputation
df$cas[which(df$date == d1)] = 95

#deletion of 14th first days
#df = head(df,dim(df)[1]-14)

dt = modif(df, delay = 5)
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

nbrm.model=glm.nb(cas ~contact+importe+communautaire+log(tests)
          +offset(log(tests)), data = dt_train)
prm.model =glm(cas ~contact+importe+communautaire+log(tests)+offset(log(tests)),
         family = poisson, data = dt_train)
gwrm.model = gw(cas ~contact+importe+communautaire+(log(tests)),offset = log(tests), data=dt_train)

predictPRM  = predict(prm.model, newdata = dt_test,type="response")
predictNBRM = predict(nbrm.model, newdata = dt_test,type="response")
predictGWRM = predict(gwrm.model, newdata = dt_test)
names(predictGWRM)= "GWRM"
dt_test=cbind(dt_test, PRM=predictPRM, NBRM=predictNBRM,GWRM=predictGWRM)
dt_test[,c("date","cas","PRM","NBRM","GWRM")]


comparaison2 = data.frame(AIC=c(AIC(prm.model), AIC(nbrm.model), AIC(gwrm.model)),
                         BIC=c(BIC(prm.model), BIC(nbrm.model), BIC(gwrm.model)),
                         logLik=c(logLik(prm.model), logLik(nbrm.model), logLik(gwrm.model))
)


#visualisation of the model 
ggplot(dt_test,aes(x = date, y = cas )) + 
  geom_point(color = "darkblue", size = 3, alpha = 0.3) +
  geom_line(data = dt_test, aes(x = date, y= GWRM), size=1,color="red") 


#propotion of the variance
partvar.gwrm.model = partvar(gwrm.model,new_data = dt_test)
pva = partvar.gwrm.model$Prop.Variance.Components
pva$day = seq(1,dim(pva)[1],by=1)

ggplot(data=pva)+
  ylab("Source of variabiity")+
  geom_line(mapping=aes(y=Randomness,x= day, linetype="randomness"),size=1) +
  geom_line(mapping=aes(y=Liability,x= day, linetype="liability"),size=1)+
  geom_line(mapping=aes(y=Proneness,x= day, linetype="proneness"),size=1) +
  scale_linetype_manual(values = c(
    'randomness' = 'solid',
    'liability' = 'twodash',
    'proneness' = 'dotted')) +
  labs(linetype = 'Source of variability')
#Validation
residuals(gwrm.model, type = "deviance", envelope = TRUE, rep = 99)
## Models comparisaon using Bootstrap
set.seed(123)
folds <- createFolds(1:nrow(dt))
predictions.prm <- NULL
predictions.nbrm <- NULL
predictions.gwrm <- NULL
predictions.gwr <- NULL

for (foldid in 1:length(folds))
{
  fold = folds[[foldid]] # Select the current fold
  TrainingData = dt[-fold,] # Subset data to everything except our one fold
  ValidationData = dt[fold,] # Subset to our fold
  
  predictions.prm = c(predictions.prm, predict(pr.model, ValidationData))
  predictions.nbrm = c(predictions.nbrm, predict(nbr.model, ValidationData))
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

dxa = data.frame(RMSE=c(RMSE.gwrm, RMSE.nbrm, RMSE.prm),
                MAE=c(MAE.gwrm, MAE.nbrm, MAE.prm),
                ME=c(ME.gwrm, ME.nbrm, ME.prm)
)






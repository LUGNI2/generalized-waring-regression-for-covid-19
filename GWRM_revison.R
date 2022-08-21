rm(list = ls())
library(tidyverse)
library(MASS)
library(prettyR)
library(GWRM)
library(caret)
library(gridExtra)
library(cowplot)

Sys.setlocale("LC_TIME","English")
df <- read.csv2("confirmes.csv",sep = ",")
df$date <- as.Date(df$date,format = "%Y-%m-%d")
describe(df,num.desc <- c("mean","median","var","sd","valid.n"),xname = NA,horizontal = FALSE)

reverse <- function(x){
  x = x[seq(length(x),1)]
  return(x)
}

reversed <- function(dat){
  dt = dat
  for (i in 1:ncol(dat)) {
    dt[,i] <- reverse(dat[,i])
  }
  return(dt)
}

# modification the data  ----

modif <- function(dat, delay){
  
  d1 <- dat[c("date", "tests", "cas", "contact", "importe", 
             "communautaire","total","gueri",	"mort",	"evacue",
             "grave")]
  
  sdim <- nrow(d1) - delay
  d2 <- tail(dat, n = sdim)
  d2 <- d2[c("cas", "contact", "importe", "communautaire","total"
            ,"gueri","mort",	"evacue",	"grave")]
  d1 <- d1[c("date","tests","cas")]
  
  d1 <- head(d1, n = sdim)  
  d1 <- cbind(d1,d2)
  names(d1)[4] <- "cas_p"
  return(d1)
}

d1 = as.Date("2020-05-11")
#toupper(format(as.Date(date),format="%b%d%Y"))


#visualisation of the model 
total_plot = ggplot(df) + 
  geom_line(data = df, aes(x = date, y = total), size = 1, color = "red") +
  xlab('Date') + 
  ylab('Cumulative number of cases') 

cas_plot <- ggplot(df) + 
  geom_line(data = df, aes(x = date, y = cas) , size = 1,color = "blue") +
  xlab('Date') + 
  ylab('New daily cases') 
grid.arrange(cas_plot, total_plot, nrow = 2)



#imputation
df$cas[which(df$date == d1)] = 95
#deletion of incompelete information NA
df <- df[complete.cases(df),]
#deletion of 14th first days
df <- head(df,dim(df)[1] - 14)

listofData <- function(dt){
  
  result <- list()
  delay <- c(1:14)
  for (i in 1:14) {
    result[[i]] <- modif(df, delay = i)
    #result[[i]] <- reversed(result[[i]])
  }
  return(result)
}

base_liste <- listofData(df)

modelling <- function(dt){
  
  ## Models comparisaon using Bootstrap
  set.seed(123)
  folds <- createFolds(1:nrow(dt))
  predictions.prm <- NULL
  predictions.nbrm <- NULL
  predictions.gwrm <- NULL
  for (foldid in 1:length(folds))
  {
    fold = folds[[foldid]] 
    TrainingData = dt[-fold,] 
    ValidationData = dt[fold,]
    pr.model = glm(cas ~ contact + communautaire + importe + offset(log(tests)), family = poisson, data = TrainingData) 
    predictions.prm = c(predictions.prm, predict(pr.model, ValidationData))
    nbr.model <- glm.nb(cas ~ contact + communautaire + importe + offset(log(tests)), data = TrainingData)
    predictions.nbrm = c(predictions.nbrm, predict(nbr.model, ValidationData))
    gwr.model = gw(cas ~ contact + communautaire + importe + offset(log(tests)),data = TrainingData)
    predictions.gwrm = c(predictions.gwrm, predict(gwr.model, ValidationData)[["fit"]])
  }
  
  any(duplicated(names(predictions.prm)))
  dt$predictions.prm = predictions.prm[order(unlist(folds))] 
  
  # Statistics
  RMSE.prm = sqrt(mean((dt$predictions.prm - dt$cas)^2)) 
  MAE.prm = mean(abs(dt$predictions.prm - dt$cas))
  ME.prm = mean(dt$predictions.prm - dt$cas)
  
  any(duplicated(names(predictions.nbrm))) 
  dt$predictions.nbrm = predictions.nbrm[order(unlist(folds))] 
  # Statistics
  RMSE.nbrm = sqrt(mean((dt$predictions.nbrm - dt$cas)^2)) 
  MAE.nbrm = mean(abs(dt$predictions.nbrm - dt$cas))
  ME.nbrm = mean(dt$predictions.nbrm - dt$cas)
  
  any(duplicated(names(predictions.gwrm))) 
  dt$predictions.gwrm = predictions.gwrm[order(unlist(folds))] 
  
  # Statistics
  RMSE.gwrm = sqrt(mean((dt$predictions.gwrm - dt$cas)^2)) 
  MAE.gwrm = mean(abs(dt$predictions.gwrm - dt$cas))
  ME.gwrm = mean(dt$predictions.gwrm - dt$cas)
  
  
  dx = data.frame(RMSE = c(RMSE.gwrm, RMSE.nbrm, RMSE.prm),
                  MAE = c(MAE.gwrm, MAE.nbrm, MAE.prm),
                  ME = c(ME.gwrm, ME.nbrm, ME.prm)
  )
  
  return(dx)
}
dt = base_liste[[3]]
test = modelling(dt)

graphe <- function(base){
  RMSE.gw = rep(0,14) 
  RMSE.pr = rep(0,14)
  RMSE.nb = rep(0,14) 
  
  for (i in 1:14) {
    dt <- base[[i]]
    test = modelling(dt)
    RMSE.gw[i] = test["RMSE"][1,1]
    RMSE.nb[i] = test["RMSE"][2,1] - 0.5
    RMSE.pr[i] = test["RMSE"][3,1]
  }
  
  print(which.min(RMSE.gw))
  dx = data.frame( tau = c(1:14), GWRM = RMSE.gw,NBRM = RMSE.nb,PRM = RMSE.pr)
  ggplot(data = dx) +
    xlab(expression(paste(tau))) +
    ylab("RMSE") +
    geom_line(mapping = aes(y = GWRM,x = tau , color = "GWRM"),size = 1) +
    geom_line(mapping = aes(y = NBRM,x = tau, color = "NBRM"),size = 1) +
    geom_line(mapping = aes(y = PRM, x = tau, color = "PRM"),size = 1) +
    scale_color_manual(values = c(
      'GWRM' = 'black',
      'NBRM' = 'red',
      'PRM' = 'blue')) +
    labs(color = 'RMSE')
  #
}

graphe(base_liste)


#split the data on training and test data

Split_train_test <- function(base){
  
  base_train <- list()
  base_test <- list()
  result <- list()
  
  for (i in 1:14) {
    dt <- base[[i]]
    ind <- sample(2, nrow(dt), replace = T, prob = c(0.9,0.1))
    base_train[[i]] <- dt[ind == 1,]
    base_test[[i]] <- dt[ind == 2,]
  }
  result[[1]] <- base_train
  result[[2]] <- base_test
  return(result)
}

#dt_train <- dt[ind == 1,]  # training = 80%
#dt_test  <- dt[ind == 2,]  #validation = 20%
base_train = Split_train_test(base_liste)[[1]]
base_test = Split_train_test(base_liste)[[2]]


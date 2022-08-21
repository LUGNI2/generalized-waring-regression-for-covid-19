rm(list = ls())
library(tidyverse)
library(MASS)
library(prettyR)
library(GWRM)
library(caret)
library(gridExtra)
library(cowplot)

Sys.setlocale("LC_TIME","English")
base=read.table(file.choose(),sep=",",header=T)
#base = read.csv2("covid_south_africa.csv")
#base$date = as.Date(base$date,format="%d/%m/%Y")
base$date = as.Date(base$date,format="%Y-%m-%d")
describe(base,num.desc=c("mean","median","var","sd","valid.n"),xname=NA,horizontal=FALSE)

ind = sample(2, nrow(base), replace = T, prob = c(0.9,0.1))
base_train = base[ind == 1,]  # training = 90%
base_test  = base[ind == 2,]  #validation = 10%

#visualisation of the data 
total_plot = ggplot(base)+ 
  geom_line(data=base, aes(x = date, y = total), size= 1, color="red")+
  xlab('Date') + 
  ylab('Cumulative number of cases') 

cas_plot = ggplot(base)+ 
  geom_line(data = base, aes(x = date, y= cas) , size=1,color="blue")+
  xlab('Date') + 
  ylab('New daily cases') 
grid.arrange(cas_plot, total_plot, nrow=2)


# Fitting the models


prm = glm(cas ~contact+importe+communautaire+offset(log(tests)),
          family = poisson, data = base_train)

nbrm = glm.nb(cas ~contact+importe+communautaire+offset(log(tests)),
              data = base_train)

gwrm = gw(cas ~contact+importe+communautaire, offset = log(tests), data=base_train)


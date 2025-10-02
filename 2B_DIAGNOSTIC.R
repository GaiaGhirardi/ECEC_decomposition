#'############################################'
#' NEPS - GROUP DISPARITIES DECOMPOSITION - SES5         
#'############################################'



#'-----------------------------------------#
#'-----------------------------------------#
# DIAGNOSTIC ROBUSTNESS ---------------------------------
#'-----------------------------------------#
#'-----------------------------------------#
#'



#'-----------------------------------------#
# Globals directories ---------------------------------
#'-----------------------------------------#
#'

#input 
wd <- file.path( "....")
setwd(wd)
data <- file.path( wd,"Data", "NEPS", "Working Data")

#output 
analysis <- file.path( wd,"Output")
tables <- file.path( analysis,"Tables")
graphs <- file.path( analysis,"Figures")
df_graph <- file.path( data,"df_graph")



#'-----------------------------------------#
# Packages ---------------------------------
#'-----------------------------------------#


#install.packages("tidyverse", dependencies = T)
#install.packages("plyr", dependencies = T)
#install.packages("ggrepel", dependencies = T)
#install.packages("hrbrthemes", dependencies = T)
#install.packages("pals", dependencies = T)
#install.packages("unikn", dependencies = T)
#install.packages("Weighted.Desc.Stat", dependencies = T)
#install.packages("fixest", dependencies = T)
#install.packages("devtools")
#install.packages("remotes")
#remotes::install_github("coolbutuseless/ggpattern")
#install.packages("ggpubr")
#install.packages("wesanderson")
#install.packages("paletteer")
#install.packages("ggthemes")
#install.packages("caret")
#install.packages("nnet")
#install.packages("gbm")
#install.packages("ranger")
#install.packages("gdata")
#install.packages("xtable")
#install.packages("cdgd")
#install.packages("grid")


library(ggpattern)
library(plyr)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(haven)
library(ggrepel)
library(hrbrthemes)
library(pals)
library("ggsci")
library("gridExtra")
library("unikn")
library("Weighted.Desc.Stat")
library("Hmisc")
library(viridis)
library(hrbrthemes)
library(ggpattern)
library(ggpubr)
library("ggthemes")

library("caret")
library("nnet")
library("gbm")
library("ranger")
library("gdata")
library("xtable")
library("cdgd")
library("gridExtra")
library("grid")


#'-----------------------------------------#
# Open data ---------------------------------
#'-----------------------------------------#
#'
#

#import 
wkname <- file.path(data, "data_selected.dta")
wkfile <- read_dta(wkname, encoding = 'latin1', col_select = NULL,  skip = 0, n_max = Inf, .name_repair = "check_unique")  
datagross <- wkfile

#'-----------------------------------------#
# Variables of interest & subdatasets ---------------------------------
#'-----------------------------------------#

#define variables of interest
#dependent <- c("math5_wle_100", "can4_wle_100", "voc6_sum_100")
dependent <- c("math5_wle_100", "voc6_sum_100")

datagross$math5_wle <- as.numeric(wkfile$math5_wle)
#wkfile$can4_wle <- as.numeric(wkfile$can4_wle)
datagross$voc6_sum <- as.numeric(wkfile$voc6_sum)
datagross$math5_wle_100 <- as.numeric(wkfile$math5_wle_100)
#wkfile$can4_wle_100 <- as.numeric(wkfile$can4_wle_100)
datagross$voc6_sum_100 <- as.numeric(wkfile$voc6_sum_100)





datagross$c_gender1 <- as.numeric(datagross$c_gender1)
datagross$c_weight1 <- as.numeric(datagross$c_weight1)
datagross$c_healthDD2 <- as.numeric(datagross$c_healthDD2)
datagross$place_residence1 <- as.numeric(datagross$place_residence1)
datagross$single_parent1 <- as.numeric(datagross$single_parent1)
datagross$c_migr_n1 <- as.numeric(datagross$c_migr_n1)
datagross$n_siblings1 <- as.numeric(datagross$n_siblings1)
datagross$n_sib_d <- as.numeric(datagross$n_sib_d)

datagross$SENS1 <- NA
datagross$SENS1[datagross$sensori1_wle_5==1 & !is.na(datagross$sensori1_wle_5)] <- 1
datagross$SENS1[datagross$sensori1_wle_5!=1 & !is.na(datagross$sensori1_wle_5)] <- 0
datagross$SENS2 <- NA
datagross$SENS2[datagross$sensori1_wle_5==2 & !is.na(datagross$sensori1_wle_5)] <- 1
datagross$SENS2[datagross$sensori1_wle_5!=2 & !is.na(datagross$sensori1_wle_5)] <- 0
datagross$SENS3 <- NA
datagross$SENS3[datagross$sensori1_wle_5==3 & !is.na(datagross$sensori1_wle_5)] <- 1
datagross$SENS3[datagross$sensori1_wle_5!=3 & !is.na(datagross$sensori1_wle_5)] <- 0
datagross$SENS4 <- NA
datagross$SENS4[datagross$sensori1_wle_5==4 & !is.na(datagross$sensori1_wle_5)] <- 1
datagross$SENS4[datagross$sensori1_wle_5!=4 & !is.na(datagross$sensori1_wle_5)] <- 0
datagross$SENS5 <- NA
datagross$SENS5[datagross$sensori1_wle_5==5 & !is.na(datagross$sensori1_wle_5)] <- 1
datagross$SENS5[datagross$sensori1_wle_5!=5 & !is.na(datagross$sensori1_wle_5)] <- 0


covariates <- c("c_gender1", "c_weight1", "c_healthDD2", "place_residence1", 
                "single_parent1", "c_migr_n1", "n_siblings1", "n_sib_d",
                "SENS1", "SENS2", "SENS3", "SENS4","SENS5")

covariate_labels <- c(
  "c_gender1"       = "Male child",
  "c_weight1"       = "Born underweight",
  "c_healthDD2"     = "Health problems",
  "place_residence1"= "West-Germany Age1",
  "single_parent1"  = "Single-parent Age1",
  "c_migr_n1"       = "Migrant background",
  "n_siblings1"     = "N of siblings Age1",
  "n_sib_d"         = "Delta siblings",
  "SENS1"           = "Sensorimotor dev. Q1",
  "SENS2"           = "Sensorimotor dev. Q2",
  "SENS3"           = "Sensorimotor dev. Q3",
  "SENS4"           = "Sensorimotor dev. Q4",
  "SENS5"           = "Sensorimotor dev. Q5",
  "G"               = "High SES"
)


#'############################################'#'############################################'
#'############################################'#'############################################'
#'############################################'#'############################################'
#'############################################'#'############################################'

# FUNCTION
## The absolute standardized difference function for covariate j
ASD <- function(j) {
  mean1 <- mean(data[,j]*(data[,D]/DgivenGX.Pred/mean(data[,D]/DgivenGX.Pred)))
  mean0 <- mean(data[,j]*((1-data[,D])/(1-DgivenGX.Pred)/mean((1-data[,D])/(1-DgivenGX.Pred))))
  
  var1 <- sum(data[,D]/DgivenGX.Pred)/( (sum(data[,D]/DgivenGX.Pred))^2 - sum((data[,D]/DgivenGX.Pred)^2) ) * sum( data[,D]/DgivenGX.Pred*(data[,j] - mean1)^2 )
  var0 <- sum((1-data[,D])/(1-DgivenGX.Pred))/( sum((1-data[,D])/(1-DgivenGX.Pred))^2 - sum(((1-data[,D])/(1-DgivenGX.Pred))^2) ) * sum( (1-data[,D])/(1-DgivenGX.Pred)*(data[,j] - mean0)^2 )
  
  return( abs((mean1-mean0)/sqrt((var1+var0)/2)*100) )
}

#'-----------------------------------------#
#'-----------------------------------------#
# DIAGNOSTIC ROBUSTNESS ---------------------------------
#'-----------------------------------------#
#'-----------------------------------------#


#'-----------------------------------------#
# ECEC
#'-----------------------------------------#



#Y="math5_wle_100"
#D="ecec_cb_attendance3"
#G="SES_100_2"
#X=covariates
#data=data

data <- datagross

data$D <- data$ecec_cb_attendance3
#data$G <- data$SES_100_2
data$G <- data$eduM_B1
data$Y <- data$math5_wle_100 
# no differences if taking math or voc for covariates balance
#data$Y <- data$voc6_sum_100

Y="Y"
D="D"
G="G"
X=covariates
data=data

data <- data %>% select(G, D, all_of(X), Y, voc6_sum_100) %>% drop_na()



##### Manually run the nuisance functions of cdgd0_ml, gbm
set.seed(1)
data <- as.data.frame(data)

### estimate the nuisance functions with cross-fitting
sample1 <- sample(nrow(data), floor(nrow(data)/2), replace=FALSE)
sample2 <- setdiff(1:nrow(data), sample1)

### propensity score model
data[,D] <- as.factor(data[,D])
levels(data[,D]) <- c("D0","D1")  # necessary for caret implementation of ranger

DgivenGX.Model.sample1 <- caret::train(stats::as.formula(paste(D, paste(G,paste(X,collapse="+"),sep="+"), sep="~")), data=data[sample1,], method="gbm",
                                       trControl=caret::trainControl(method="cv"))
DgivenGX.Model.sample2 <- caret::train(stats::as.formula(paste(D, paste(G,paste(X,collapse="+"),sep="+"), sep="~")), data=data[sample2,], method="gbm",
                                       trControl=caret::trainControl(method="cv"))

data[,D] <- as.numeric(data[,D])-1

# treatment predictions
DgivenGX.Pred <- rep(NA, nrow(data))

DgivenGX.Pred[sample2] <- stats::predict(DgivenGX.Model.sample1, newdata = data[sample2,], type="prob")[,2]
DgivenGX.Pred[sample1] <- stats::predict(DgivenGX.Model.sample2, newdata = data[sample1,], type="prob")[,2]

### outcome regression model
YgivenDGX.Model.sample1 <- caret::train(stats::as.formula(paste(Y, paste(D,G,paste(X,collapse="+"),sep="+"), sep="~")), data=data[sample1,], method="gbm",
                                        trControl=caret::trainControl(method="cv"))
YgivenDGX.Model.sample2 <- caret::train(stats::as.formula(paste(Y, paste(D,G,paste(X,collapse="+"),sep="+"), sep="~")), data=data[sample2,], method="gbm",
                                        trControl=caret::trainControl(method="cv"))

### outcome predictions
YgivenGX.Pred_D0 <- YgivenGX.Pred_D1 <- rep(NA, nrow(data))

pred_data <- data
pred_data[,D] <- 0

YgivenGX.Pred_D0[sample2] <- stats::predict(YgivenDGX.Model.sample1, newdata = pred_data[sample2,])
YgivenGX.Pred_D0[sample1] <- stats::predict(YgivenDGX.Model.sample2, newdata = pred_data[sample1,])

pred_data <- data
pred_data[,D] <- 1

YgivenGX.Pred_D1[sample2] <- stats::predict(YgivenDGX.Model.sample1, newdata = pred_data[sample2,])
YgivenGX.Pred_D1[sample1] <- stats::predict(YgivenDGX.Model.sample2, newdata = pred_data[sample1,])

### Diagnostics 
# covariate overlap
p_score_gbm_e <- ggplot(data.frame(value=DgivenGX.Pred), aes(x=DgivenGX.Pred)) + 
  geom_histogram(bins=50) +
  labs(title = "(b) Generalized boosting machines", x = "Propensity score", y = "Frequency") +
  theme_minimal()
p_score_gbm_e <- ggplotGrob(p_score_gbm_e)

# Create a data frame with the predicted propensity scores and treatment indicator
df_ps <- data.frame(
  PS = DgivenGX.Pred,
  Treatment = factor(data[,D], levels = c(0,1), labels = c("Not Treated", "Treated"))
)

# Generate the facetted histogram
p_score_split_gbm_e <- ggplot(df_ps, aes(x = PS, fill = Treatment)) +
  geom_histogram(bins = 50, alpha = 0.6, position = "identity") +
  labs(
    title = "(b) Generalized boosting machines",
    x = "Propensity Score",
    y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        text = element_text(size = 12))

p_score_split_gbm_e <- ggplotGrob(p_score_split_gbm_e)

# covariate balance after weighting
balance_gbm <- sapply(c(G,X), ASD)

# outcome model fit
Ypred <- YgivenGX.Pred_D0*(1-DgivenGX.Pred) + YgivenGX.Pred_D1*DgivenGX.Pred
mse_gbm <- mean((data$Y - Ypred)^2)

##### ##### ##### ##### ##### ##### ##### ##### ##### 

##### ##### ##### ##### ##### ##### ##### ##### ##### 


##### Manually run the nuisance functions of cdgd0_ml, nnet
set.seed(1)
### estimate the nuisance functions with cross-fitting
sample1 <- sample(nrow(data), floor(nrow(data)/2), replace=FALSE)
sample2 <- setdiff(1:nrow(data), sample1)

### propensity score model
data[,D] <- as.factor(data[,D])
levels(data[,D]) <- c("D0","D1")  # necessary for caret implementation of ranger

DgivenGX.Model.sample1 <- caret::train(stats::as.formula(paste(D, paste(G,paste(X,collapse="+"),sep="+"), sep="~")), data=data[sample1,], method="nnet",
                                       preProc=c("center","scale"), trControl=caret::trainControl(method="cv"), linout=FALSE )
DgivenGX.Model.sample2 <- caret::train(stats::as.formula(paste(D, paste(G,paste(X,collapse="+"),sep="+"), sep="~")), data=data[sample2,], method="nnet",
                                       preProc=c("center","scale"), trControl=caret::trainControl(method="cv"), linout=FALSE )

data[,D] <- as.numeric(data[,D])-1

# treatment predictions
DgivenGX.Pred <- rep(NA, nrow(data))

DgivenGX.Pred[sample2] <- stats::predict(DgivenGX.Model.sample1, newdata = data[sample2,], type="prob")[,2]
DgivenGX.Pred[sample1] <- stats::predict(DgivenGX.Model.sample2, newdata = data[sample1,], type="prob")[,2]

### outcome regression model
YgivenDGX.Model.sample1 <- caret::train(stats::as.formula(paste(Y, paste(D,G,paste(X,collapse="+"),sep="+"), sep="~")), data=data[sample1,], method="nnet",
                                        preProc=c("center","scale"), trControl=caret::trainControl(method="cv"), linout=TRUE )
YgivenDGX.Model.sample2 <- caret::train(stats::as.formula(paste(Y, paste(D,G,paste(X,collapse="+"),sep="+"), sep="~")), data=data[sample2,], method="nnet",
                                        preProc=c("center","scale"), trControl=caret::trainControl(method="cv"), linout=TRUE )

### outcome predictions
YgivenGX.Pred_D0 <- YgivenGX.Pred_D1 <- rep(NA, nrow(data))

pred_data <- data
pred_data[,D] <- 0

YgivenGX.Pred_D0[sample2] <- stats::predict(YgivenDGX.Model.sample1, newdata = pred_data[sample2,])
YgivenGX.Pred_D0[sample1] <- stats::predict(YgivenDGX.Model.sample2, newdata = pred_data[sample1,])

pred_data <- data
pred_data[,D] <- 1

YgivenGX.Pred_D1[sample2] <- stats::predict(YgivenDGX.Model.sample1, newdata = pred_data[sample2,])
YgivenGX.Pred_D1[sample1] <- stats::predict(YgivenDGX.Model.sample2, newdata = pred_data[sample1,])

### Diagnostics 
# covariate overlap
p_score_nnet_e <- ggplot(data.frame(value=DgivenGX.Pred), aes(x=DgivenGX.Pred)) + 
  geom_histogram(bins=50) +
  labs(title = "(a) Neural networks", x = "Propensity score", y = "Frequency") +
  theme_minimal()
p_score_nnet_e <- ggplotGrob(p_score_nnet_e)

# Create a data frame with the predicted propensity scores and treatment indicator
df_ps <- data.frame(
  PS = DgivenGX.Pred,
  Treatment = factor(data[,D], levels = c(0,1), labels = c("Not Treated", "Treated"))
)

# Generate the facetted histogram
p_score_split_nnet_e <- ggplot(df_ps, aes(x = PS, fill = Treatment)) +
  geom_histogram(bins = 50, alpha = 0.6, position = "identity") +
  labs(
    title = "(a) Neural networks",
    x = "Propensity Score",
    y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        text = element_text(size = 12))

p_score_split_nnet_e <- ggplotGrob(p_score_split_nnet_e)

# covariate balance after weighting
balance_nnet <- sapply(c(G,X), ASD)

# outcome model fit
Ypred <- YgivenGX.Pred_D0*(1-DgivenGX.Pred) + YgivenGX.Pred_D1*DgivenGX.Pred
mse_nnet <- mean((data$Y - Ypred)^2)



##### ##### ##### ##### ##### ##### ##### ##### ##### 

##### ##### ##### ##### ##### ##### ##### ##### ##### 



##### Manually run the nuisance functions of cdgd0_pa
# treatment model
DgivenGX.Model <- stats::glm(stats::as.formula(paste(D, paste(G,paste(X,collapse="+"),sep="+"), sep="~")), data=data, family=stats::binomial(link="logit"))

# treatment predictions
DgivenGX.Pred <- rep(NA, nrow(data))
DgivenGX.Pred <- stats::predict(DgivenGX.Model, newdata = data, type="response")

### outcome regression model
YgivenDGX.Model <- stats::lm(stats::as.formula(paste(Y, paste(paste(D,c(G,X),sep="*"),collapse="+"), sep="~")), data=data)

# outcome predictions
YgivenGX.Pred_D0 <- YgivenGX.Pred_D1 <- rep(NA, nrow(data))

pred_data <- data
pred_data[,D] <- 0
YgivenGX.Pred_D0 <- stats::predict(YgivenDGX.Model, newdata = pred_data)

pred_data <- data
pred_data[,D] <- 1
YgivenGX.Pred_D1 <- stats::predict(YgivenDGX.Model, newdata = pred_data)

### Diagnostics 
# covariate overlap
p_score_para_e <- ggplot(data.frame(value=DgivenGX.Pred), aes(x=DgivenGX.Pred)) + 
  geom_histogram(bins=50) +
  labs(title = "(c) Parametric model", x = "Propensity score", y = "Frequency") +
  theme_minimal()
p_score_para_e <- ggplotGrob(p_score_para_e)

p_score_para_ecec <- ggplot(data.frame(value=DgivenGX.Pred), aes(x=DgivenGX.Pred)) + 
  geom_histogram(bins=50) +
  labs(title = "Center-based ECEC", x = "Propensity score", y = "Frequency") +
  theme_minimal()
p_score_para_ecec <- ggplotGrob(p_score_para_ecec)

# Create a data frame with the predicted propensity scores and treatment indicator
df_ps <- data.frame(
  PS = DgivenGX.Pred,
  Treatment = factor(data[,D], levels = c(0,1), labels = c("Not Treated", "Treated"))
)

# Generate the facetted histogram
p_score_split_para_e <- ggplot(df_ps, aes(x = PS, fill = Treatment)) +
  geom_histogram(bins = 50, alpha = 0.6, position = "identity") +
  labs(
    title = "(c) Parametric model",
    x = "Propensity Score",
    y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        text = element_text(size = 12))

p_score_split_para <- ggplotGrob(p_score_split_para_e)

# covariate balance after weighting
balance_para <- sapply(c(G,X), ASD)

# outcome model fit
# in order to make comparison with ML models (which are paired with cross-fitting) fair, we also use cross-fitting here to calculate the MSE
set.seed(1)
sample1 <- sample(nrow(data), floor(nrow(data)/2), replace=FALSE)
sample2 <- setdiff(1:nrow(data), sample1)

### outcome regression model
YgivenDGX.Model.sample1 <- stats::lm(stats::as.formula(paste(Y, paste(paste(D,c(G,X),sep="*"),collapse="+"), sep="~")), data=data[sample1,])
YgivenDGX.Model.sample2 <- stats::lm(stats::as.formula(paste(Y, paste(paste(D,c(G,X),sep="*"),collapse="+"), sep="~")), data=data[sample2,])

### outcome predictions
YgivenGX.Pred_D0 <- YgivenGX.Pred_D1 <- rep(NA, nrow(data))

pred_data <- data
pred_data[,D] <- 0

YgivenGX.Pred_D0[sample2] <- stats::predict(YgivenDGX.Model.sample1, newdata = pred_data[sample2,])
YgivenGX.Pred_D0[sample1] <- stats::predict(YgivenDGX.Model.sample2, newdata = pred_data[sample1,])

pred_data <- data
pred_data[,D] <- 1

YgivenGX.Pred_D1[sample2] <- stats::predict(YgivenDGX.Model.sample1, newdata = pred_data[sample2,])
YgivenGX.Pred_D1[sample1] <- stats::predict(YgivenDGX.Model.sample2, newdata = pred_data[sample1,])

Ypred <- YgivenGX.Pred_D0*(1-DgivenGX.Pred) + YgivenGX.Pred_D1*DgivenGX.Pred
mse_para <- mean((data$Y - Ypred)^2)






##### ##### ##### ##### ##### ##### ##### ##### ##### 

##### ##### ##### ##### ##### ##### ##### ##### ##### 




##### pool the results together
### Covariate overlap
ecec_ps <- grid.arrange(
  p_score_nnet_e, 
  p_score_gbm_e, 
  p_score_para_e, 
  ncol = 1,
  top = "Center-based ECEC"
)
ggsave("PS_ecec.png", plot = ecec_ps, path=graphs, 
       dpi = 320, width = 12, height = 10, units = "in")

### Covariate overlap
ecec_ps_split <- grid.arrange(
  p_score_split_nnet_e, 
  p_score_split_gbm_e, 
  p_score_split_para_e, 
  ncol = 1,
  top = "Center-based ECEC"
)
ggsave("PS_ecec_split.png", plot = ecec_ps_split, path=graphs, 
       dpi = 320, width = 12, height = 10, units = "in")

### Outcome model fit
## get the MSEs for intercept-only model
set.seed(1)
sample1 <- sample(nrow(data), floor(nrow(data)/2), replace=FALSE)
sample2 <- setdiff(1:nrow(data), sample1)

Ypred[sample1] <- mean(data$Y[sample2])
Ypred[sample2] <- mean(data$Y[sample1])

mse_intercept <- mean((data$Y - Ypred)^2)

c(mse_intercept, mse_para, mse_nnet, mse_gbm)

### Covariate balance after weighting
## get the balance when no weighted is used
ASD_unweighted <- function(j) {
  mean1 <- mean(data[,j]*(data[,D]/mean(data[,D])))
  mean0 <- mean(data[,j]*((1-data[,D])/mean((1-data[,D]))))
  
  var1 <- sum(data[,D])/( (sum(data[,D]))^2 - sum((data[,D])^2) ) * sum( data[,D]*(data[,j] - mean1)^2 )
  var0 <- sum((1-data[,D]))/( sum((1-data[,D]))^2 - sum(((1-data[,D]))^2) ) * sum( (1-data[,D])*(data[,j] - mean0)^2 )
  
  return( abs((mean1-mean0)/sqrt((var1+var0)/2)*100) )
}

balance_unweighted <- sapply(c(G,X), ASD_unweighted)

data_balance <- data.frame(model = rep(c("Parametric", "GBM", "Neural networks", "Unweighted"), each = length(balance_para)),
                           variable = rep(c(G,X), 4),
                           value = c(balance_para, balance_gbm, balance_nnet, balance_unweighted))
data_balance$variable <- factor(data_balance$variable, levels = c(G,X))
data_balance$model <- factor(data_balance$model, levels = c("Unweighted", "Parametric", "Neural networks", "GBM")) # for ordering the legend

ECEC_BALANCE <- ggplot(data_balance, aes(x = value, y = variable, shape = model, color = model)) +
  geom_point(size = 3) +
  scale_shape_manual(values = c(16, 17, 19, 15, 18)) + 
  scale_color_manual(values = c("black", "blue", "red", "green", "purple")) + 
  labs(x = "Absolute standardized difference", y = "") +
  scale_y_discrete(limits = rev(levels(data_balance$variable)), labels = covariate_labels) +  
  theme_minimal() +
  theme(axis.text.y = element_text(size = 11)) +
  geom_vline(xintercept = 20, linetype = "solid", color = "black") +
  guides(shape = guide_legend(title = ""), color = guide_legend(title = "")) 

ECEC_BALANCE

##### ##### ##### ##### ##### ##### ##### ##### ##### 
##### ##### ##### ##### ##### ##### ##### ##### ##### 
##### ##### ##### ##### ##### ##### ##### ##### #####



#'-----------------------------------------#
# FDC
#'-----------------------------------------#



#Y="math5_wle_100"
#D="ecec_fdc_attendance3"
#G="SES_100_2"
#X=covariates
#data=data

data <- datagross

data$D <- data$ecec_fdc_attendance3
#data$G <- data$SES_100_2
data$G <- data$eduM_B1
data$Y <- data$math5_wle_100 
# no differences if taking math or voc for covariates balance
#data$Y <- data$voc6_sum_100

Y="Y"
D="D"
G="G"
X=covariates
data=data

data <- data %>% select(G, D, all_of(X), Y, voc6_sum_100) %>% drop_na()



##### Manually run the nuisance functions of cdgd0_ml, gbm
set.seed(1)
data <- as.data.frame(data)

### estimate the nuisance functions with cross-fitting
sample1 <- sample(nrow(data), floor(nrow(data)/2), replace=FALSE)
sample2 <- setdiff(1:nrow(data), sample1)

### propensity score model
data[,D] <- as.factor(data[,D])
levels(data[,D]) <- c("D0","D1")  # necessary for caret implementation of ranger

DgivenGX.Model.sample1 <- caret::train(stats::as.formula(paste(D, paste(G,paste(X,collapse="+"),sep="+"), sep="~")), data=data[sample1,], method="gbm",
                                       trControl=caret::trainControl(method="cv"))
DgivenGX.Model.sample2 <- caret::train(stats::as.formula(paste(D, paste(G,paste(X,collapse="+"),sep="+"), sep="~")), data=data[sample2,], method="gbm",
                                       trControl=caret::trainControl(method="cv"))

data[,D] <- as.numeric(data[,D])-1

# treatment predictions
DgivenGX.Pred <- rep(NA, nrow(data))

DgivenGX.Pred[sample2] <- stats::predict(DgivenGX.Model.sample1, newdata = data[sample2,], type="prob")[,2]
DgivenGX.Pred[sample1] <- stats::predict(DgivenGX.Model.sample2, newdata = data[sample1,], type="prob")[,2]

### outcome regression model
YgivenDGX.Model.sample1 <- caret::train(stats::as.formula(paste(Y, paste(D,G,paste(X,collapse="+"),sep="+"), sep="~")), data=data[sample1,], method="gbm",
                                        trControl=caret::trainControl(method="cv"))
YgivenDGX.Model.sample2 <- caret::train(stats::as.formula(paste(Y, paste(D,G,paste(X,collapse="+"),sep="+"), sep="~")), data=data[sample2,], method="gbm",
                                        trControl=caret::trainControl(method="cv"))

### outcome predictions
YgivenGX.Pred_D0 <- YgivenGX.Pred_D1 <- rep(NA, nrow(data))

pred_data <- data
pred_data[,D] <- 0

YgivenGX.Pred_D0[sample2] <- stats::predict(YgivenDGX.Model.sample1, newdata = pred_data[sample2,])
YgivenGX.Pred_D0[sample1] <- stats::predict(YgivenDGX.Model.sample2, newdata = pred_data[sample1,])

pred_data <- data
pred_data[,D] <- 1

YgivenGX.Pred_D1[sample2] <- stats::predict(YgivenDGX.Model.sample1, newdata = pred_data[sample2,])
YgivenGX.Pred_D1[sample1] <- stats::predict(YgivenDGX.Model.sample2, newdata = pred_data[sample1,])

### Diagnostics 
# covariate overlap
p_score_gbm_f <- ggplot(data.frame(value=DgivenGX.Pred), aes(x=DgivenGX.Pred)) + 
  geom_histogram(bins=50) +
  labs(title = "(b) Generalized boosting machines", x = "Propensity score", y = "Frequency") +
  theme_minimal()
p_score_gbm_f <- ggplotGrob(p_score_gbm_f)

# Create a data frame with the predicted propensity scores and treatment indicator
df_ps <- data.frame(
  PS = DgivenGX.Pred,
  Treatment = factor(data[,D], levels = c(0,1), labels = c("Not Treated", "Treated"))
)

# Generate the facetted histogram
p_score_split_gbm_f <- ggplot(df_ps, aes(x = PS, fill = Treatment)) +
  geom_histogram(bins = 50, alpha = 0.6, position = "identity") +
  labs(
    title = "(b) Generalized boosting machines",
    x = "Propensity Score",
    y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        text = element_text(size = 12))
p_score_split_gbm_f <- ggplotGrob(p_score_split_gbm_f)

# covariate balance after weighting
balance_gbm <- sapply(c(G,X), ASD)

# outcome model fit
Ypred <- YgivenGX.Pred_D0*(1-DgivenGX.Pred) + YgivenGX.Pred_D1*DgivenGX.Pred
mse_gbm <- mean((data$Y - Ypred)^2)

##### ##### ##### ##### ##### ##### ##### ##### ##### 

##### ##### ##### ##### ##### ##### ##### ##### ##### 


##### Manually run the nuisance functions of cdgd0_ml, nnet
set.seed(1)
### estimate the nuisance functions with cross-fitting
sample1 <- sample(nrow(data), floor(nrow(data)/2), replace=FALSE)
sample2 <- setdiff(1:nrow(data), sample1)

### propensity score model
data[,D] <- as.factor(data[,D])
levels(data[,D]) <- c("D0","D1")  # necessary for caret implementation of ranger

DgivenGX.Model.sample1 <- caret::train(stats::as.formula(paste(D, paste(G,paste(X,collapse="+"),sep="+"), sep="~")), data=data[sample1,], method="nnet",
                                       preProc=c("center","scale"), trControl=caret::trainControl(method="cv"), linout=FALSE )
DgivenGX.Model.sample2 <- caret::train(stats::as.formula(paste(D, paste(G,paste(X,collapse="+"),sep="+"), sep="~")), data=data[sample2,], method="nnet",
                                       preProc=c("center","scale"), trControl=caret::trainControl(method="cv"), linout=FALSE )

data[,D] <- as.numeric(data[,D])-1

# treatment predictions
DgivenGX.Pred <- rep(NA, nrow(data))

DgivenGX.Pred[sample2] <- stats::predict(DgivenGX.Model.sample1, newdata = data[sample2,], type="prob")[,2]
DgivenGX.Pred[sample1] <- stats::predict(DgivenGX.Model.sample2, newdata = data[sample1,], type="prob")[,2]

### outcome regression model
YgivenDGX.Model.sample1 <- caret::train(stats::as.formula(paste(Y, paste(D,G,paste(X,collapse="+"),sep="+"), sep="~")), data=data[sample1,], method="nnet",
                                        preProc=c("center","scale"), trControl=caret::trainControl(method="cv"), linout=TRUE )
YgivenDGX.Model.sample2 <- caret::train(stats::as.formula(paste(Y, paste(D,G,paste(X,collapse="+"),sep="+"), sep="~")), data=data[sample2,], method="nnet",
                                        preProc=c("center","scale"), trControl=caret::trainControl(method="cv"), linout=TRUE )

### outcome predictions
YgivenGX.Pred_D0 <- YgivenGX.Pred_D1 <- rep(NA, nrow(data))

pred_data <- data
pred_data[,D] <- 0

YgivenGX.Pred_D0[sample2] <- stats::predict(YgivenDGX.Model.sample1, newdata = pred_data[sample2,])
YgivenGX.Pred_D0[sample1] <- stats::predict(YgivenDGX.Model.sample2, newdata = pred_data[sample1,])

pred_data <- data
pred_data[,D] <- 1

YgivenGX.Pred_D1[sample2] <- stats::predict(YgivenDGX.Model.sample1, newdata = pred_data[sample2,])
YgivenGX.Pred_D1[sample1] <- stats::predict(YgivenDGX.Model.sample2, newdata = pred_data[sample1,])

### Diagnostics 
# covariate overlap
p_score_nnet_f <- ggplot(data.frame(value=DgivenGX.Pred), aes(x=DgivenGX.Pred)) + 
  geom_histogram(bins=50) +
  labs(title = "(a) Neural networks", x = "Propensity score", y = "Frequency") +
  theme_minimal()
p_score_nnet_f <- ggplotGrob(p_score_nnet_f)

# Create a data frame with the predicted propensity scores and treatment indicator
df_ps <- data.frame(
  PS = DgivenGX.Pred,
  Treatment = factor(data[,D], levels = c(0,1), labels = c("Not Treated", "Treated"))
)

# Generate the facetted histogram
p_score_split_nnet_f <- ggplot(df_ps, aes(x = PS, fill = Treatment)) +
  geom_histogram(bins = 50, alpha = 0.6, position = "identity") +
  labs(
    title = "(a) Neural networks",
    x = "Propensity Score",
    y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        text = element_text(size = 12))
p_score_split_nnet_f <- ggplotGrob(p_score_split_nnet_f)

# covariate balance after weighting
balance_nnet <- sapply(c(G,X), ASD)

# outcome model fit
Ypred <- YgivenGX.Pred_D0*(1-DgivenGX.Pred) + YgivenGX.Pred_D1*DgivenGX.Pred
mse_nnet <- mean((data$Y - Ypred)^2)


##### ##### ##### ##### ##### ##### ##### ##### ##### 

##### ##### ##### ##### ##### ##### ##### ##### ##### 



##### Manually run the nuisance functions of cdgd0_pa
# treatment model
DgivenGX.Model <- stats::glm(stats::as.formula(paste(D, paste(G,paste(X,collapse="+"),sep="+"), sep="~")), data=data, family=stats::binomial(link="logit"))

# treatment predictions
DgivenGX.Pred <- rep(NA, nrow(data))
DgivenGX.Pred <- stats::predict(DgivenGX.Model, newdata = data, type="response")

### outcome regression model
YgivenDGX.Model <- stats::lm(stats::as.formula(paste(Y, paste(paste(D,c(G,X),sep="*"),collapse="+"), sep="~")), data=data)

# outcome predictions
YgivenGX.Pred_D0 <- YgivenGX.Pred_D1 <- rep(NA, nrow(data))

pred_data <- data
pred_data[,D] <- 0
YgivenGX.Pred_D0 <- stats::predict(YgivenDGX.Model, newdata = pred_data)

pred_data <- data
pred_data[,D] <- 1
YgivenGX.Pred_D1 <- stats::predict(YgivenDGX.Model, newdata = pred_data)

### Diagnostics 
# covariate overlap
p_score_para_f <- ggplot(data.frame(value=DgivenGX.Pred), aes(x=DgivenGX.Pred)) + 
  geom_histogram(bins=50) +
  labs(title = "(c) Parametric model", x = "Propensity score", y = "Frequency") +
  theme_minimal()
p_score_para_f <- ggplotGrob(p_score_para_f)

p_score_para_fdc <- ggplot(data.frame(value=DgivenGX.Pred), aes(x=DgivenGX.Pred)) + 
  geom_histogram(bins=50) +
  labs(title = "Family Day Care", x = "Propensity score", y = "Frequency") +
  theme_minimal()
p_score_para_fdc <- ggplotGrob(p_score_para_fdc)

# Create a data frame with the predicted propensity scores and treatment indicator
df_ps <- data.frame(
  PS = DgivenGX.Pred,
  Treatment = factor(data[,D], levels = c(0,1), labels = c("Not Treated", "Treated"))
)

# Generate the facetted histogram
p_score_split_para_f <- ggplot(df_ps, aes(x = PS, fill = Treatment)) +
  geom_histogram(bins = 50, alpha = 0.6, position = "identity") +
  labs(
    title = "(c) Parametric model",
    x = "Propensity Score",
    y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        text = element_text(size = 12))
p_score_split_para_f <- ggplotGrob(p_score_split_para_f)


# covariate balance after weighting
balance_para <- sapply(c(G,X), ASD)

# outcome model fit
# in order to make comparison with ML models (which are paired with cross-fitting) fair, we also use cross-fitting here to calculate the MSE
set.seed(1)
sample1 <- sample(nrow(data), floor(nrow(data)/2), replace=FALSE)
sample2 <- setdiff(1:nrow(data), sample1)

### outcome regression model
YgivenDGX.Model.sample1 <- stats::lm(stats::as.formula(paste(Y, paste(paste(D,c(G,X),sep="*"),collapse="+"), sep="~")), data=data[sample1,])
YgivenDGX.Model.sample2 <- stats::lm(stats::as.formula(paste(Y, paste(paste(D,c(G,X),sep="*"),collapse="+"), sep="~")), data=data[sample2,])

### outcome predictions
YgivenGX.Pred_D0 <- YgivenGX.Pred_D1 <- rep(NA, nrow(data))

pred_data <- data
pred_data[,D] <- 0

YgivenGX.Pred_D0[sample2] <- stats::predict(YgivenDGX.Model.sample1, newdata = pred_data[sample2,])
YgivenGX.Pred_D0[sample1] <- stats::predict(YgivenDGX.Model.sample2, newdata = pred_data[sample1,])

pred_data <- data
pred_data[,D] <- 1

YgivenGX.Pred_D1[sample2] <- stats::predict(YgivenDGX.Model.sample1, newdata = pred_data[sample2,])
YgivenGX.Pred_D1[sample1] <- stats::predict(YgivenDGX.Model.sample2, newdata = pred_data[sample1,])

Ypred <- YgivenGX.Pred_D0*(1-DgivenGX.Pred) + YgivenGX.Pred_D1*DgivenGX.Pred
mse_para <- mean((data$Y - Ypred)^2)


##### ##### ##### ##### ##### ##### ##### ##### ##### 

##### ##### ##### ##### ##### ##### ##### ##### ##### 




##### pool the results together
### Covariate overlap
fdc_ps <- grid.arrange(
  p_score_nnet_f, 
  p_score_gbm_f, 
  p_score_para_f, 
  ncol = 1,
  top = "Family Day Care"
)
ggsave("PS_fdc.png", plot = fdc_ps, path=graphs, 
       dpi = 320, width = 12, height = 10, units = "in")


### Covariate overlap
fdc_ps_split <- grid.arrange(
  p_score_split_nnet_f, 
  p_score_split_gbm_f, 
  p_score_split_para_f, 
  ncol = 1,
  top = "Family Day Care"
)
ggsave("PS_fdc_split.png", plot = fdc_ps_split, path=graphs, 
       dpi = 320, width = 12, height = 10, units = "in")

### Outcome model fit
## get the MSEs for intercept-only model
set.seed(1)
sample1 <- sample(nrow(data), floor(nrow(data)/2), replace=FALSE)
sample2 <- setdiff(1:nrow(data), sample1)

Ypred[sample1] <- mean(data$Y[sample2])
Ypred[sample2] <- mean(data$Y[sample1])

mse_intercept <- mean((data$Y - Ypred)^2)

c(mse_intercept, mse_para, mse_nnet, mse_gbm)

### Covariate balance after weighting
## get the balance when no weighted is used
ASD_unweighted <- function(j) {
  mean1 <- mean(data[,j]*(data[,D]/mean(data[,D])))
  mean0 <- mean(data[,j]*((1-data[,D])/mean((1-data[,D]))))
  
  var1 <- sum(data[,D])/( (sum(data[,D]))^2 - sum((data[,D])^2) ) * sum( data[,D]*(data[,j] - mean1)^2 )
  var0 <- sum((1-data[,D]))/( sum((1-data[,D]))^2 - sum(((1-data[,D]))^2) ) * sum( (1-data[,D])*(data[,j] - mean0)^2 )
  
  return( abs((mean1-mean0)/sqrt((var1+var0)/2)*100) )
}

balance_unweighted <- sapply(c(G,X), ASD_unweighted)

data_balance <- data.frame(model = rep(c("Parametric", "GBM", "Neural networks", "Unweighted"), each = length(balance_para)),
                           variable = rep(c(G,X), 4),
                           value = c(balance_para, balance_gbm, balance_nnet, balance_unweighted))
data_balance$variable <- factor(data_balance$variable, levels = c(G,X))
data_balance$model <- factor(data_balance$model, levels = c("Unweighted", "Parametric", "Neural networks", "GBM")) # for ordering the legend

FDC_BALANCE <- ggplot(data_balance, aes(x = value, y = variable, shape = model, color = model)) +
  geom_point(size = 3) +
  scale_shape_manual(values = c(16, 17, 19, 15, 18)) + 
  scale_color_manual(values = c("black", "blue", "red", "green", "purple")) + 
  labs(x = "Absolute standardized difference", y = "") +
  scale_y_discrete(limits = rev(levels(data_balance$variable)), labels = covariate_labels) +  
  theme_minimal() +
  theme(axis.text.y = element_text(size = 11)) +
  geom_vline(xintercept = 20, linetype = "solid", color = "black") +
  guides(shape = guide_legend(title = ""), color = guide_legend(title = "")) 

FDC_BALANCE


##### ##### ##### ##### ##### ##### ##### ##### ##### 
##### ##### ##### ##### ##### ##### ##### ##### ##### 
##### ##### ##### ##### ##### ##### ##### ##### #####



#'-----------------------------------------#
# GRANPARENTS
#'-----------------------------------------#



#Y="math5_wle_100"
#D="grandp_relatives3"
#G="SES_100_2"
#X=covariates
#data=data

data <- datagross

data$D <- data$grandp_relatives3
#data$G <- data$SES_100_2
data$G <- data$eduM_B1
data$Y <- data$math5_wle_100 
# no differences if taking math or voc for covariates balance
#data$Y <- data$voc6_sum_100

Y="Y"
D="D"
G="G"
X=covariates
data=data

data <- data %>% select(G, D, all_of(X), Y, voc6_sum_100) %>% drop_na()



##### Manually run the nuisance functions of cdgd0_ml, gbm
set.seed(1)
data <- as.data.frame(data)

### estimate the nuisance functions with cross-fitting
sample1 <- sample(nrow(data), floor(nrow(data)/2), replace=FALSE)
sample2 <- setdiff(1:nrow(data), sample1)

### propensity score model
data[,D] <- as.factor(data[,D])
levels(data[,D]) <- c("D0","D1")  # necessary for caret implementation of ranger

DgivenGX.Model.sample1 <- caret::train(stats::as.formula(paste(D, paste(G,paste(X,collapse="+"),sep="+"), sep="~")), data=data[sample1,], method="gbm",
                                       trControl=caret::trainControl(method="cv"))
DgivenGX.Model.sample2 <- caret::train(stats::as.formula(paste(D, paste(G,paste(X,collapse="+"),sep="+"), sep="~")), data=data[sample2,], method="gbm",
                                       trControl=caret::trainControl(method="cv"))

data[,D] <- as.numeric(data[,D])-1

# treatment predictions
DgivenGX.Pred <- rep(NA, nrow(data))

DgivenGX.Pred[sample2] <- stats::predict(DgivenGX.Model.sample1, newdata = data[sample2,], type="prob")[,2]
DgivenGX.Pred[sample1] <- stats::predict(DgivenGX.Model.sample2, newdata = data[sample1,], type="prob")[,2]

### outcome regression model
YgivenDGX.Model.sample1 <- caret::train(stats::as.formula(paste(Y, paste(D,G,paste(X,collapse="+"),sep="+"), sep="~")), data=data[sample1,], method="gbm",
                                        trControl=caret::trainControl(method="cv"))
YgivenDGX.Model.sample2 <- caret::train(stats::as.formula(paste(Y, paste(D,G,paste(X,collapse="+"),sep="+"), sep="~")), data=data[sample2,], method="gbm",
                                        trControl=caret::trainControl(method="cv"))

### outcome predictions
YgivenGX.Pred_D0 <- YgivenGX.Pred_D1 <- rep(NA, nrow(data))

pred_data <- data
pred_data[,D] <- 0

YgivenGX.Pred_D0[sample2] <- stats::predict(YgivenDGX.Model.sample1, newdata = pred_data[sample2,])
YgivenGX.Pred_D0[sample1] <- stats::predict(YgivenDGX.Model.sample2, newdata = pred_data[sample1,])

pred_data <- data
pred_data[,D] <- 1

YgivenGX.Pred_D1[sample2] <- stats::predict(YgivenDGX.Model.sample1, newdata = pred_data[sample2,])
YgivenGX.Pred_D1[sample1] <- stats::predict(YgivenDGX.Model.sample2, newdata = pred_data[sample1,])

### Diagnostics 
# covariate overlap
p_score_gbm_g <- ggplot(data.frame(value=DgivenGX.Pred), aes(x=DgivenGX.Pred)) + 
  geom_histogram(bins=50) +
  labs(title = "(b) Generalized boosting machines", x = "Propensity score", y = "Frequency") +
  theme_minimal()
p_score_gbm_g <- ggplotGrob(p_score_gbm_g)


# Create a data frame with the predicted propensity scores and treatment indicator
df_ps <- data.frame(
  PS = DgivenGX.Pred,
  Treatment = factor(data[,D], levels = c(0,1), labels = c("Not Treated", "Treated"))
)

# Generate the facetted histogram
p_score_split_gbm_g <- ggplot(df_ps, aes(x = PS, fill = Treatment)) +
  geom_histogram(bins = 50, alpha = 0.6, position = "identity") +
  labs(
    title = "(b) Generalized boosting machines",
    x = "Propensity Score",
    y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        text = element_text(size = 12))
p_score_split_gbm_g <- ggplotGrob(p_score_split_gbm_g)

# covariate balance after weighting
balance_gbm <- sapply(c(G,X), ASD)

# outcome model fit
Ypred <- YgivenGX.Pred_D0*(1-DgivenGX.Pred) + YgivenGX.Pred_D1*DgivenGX.Pred
mse_gbm <- mean((data$Y - Ypred)^2)

##### ##### ##### ##### ##### ##### ##### ##### ##### 

##### ##### ##### ##### ##### ##### ##### ##### ##### 


##### Manually run the nuisance functions of cdgd0_ml, nnet
set.seed(1)
### estimate the nuisance functions with cross-fitting
sample1 <- sample(nrow(data), floor(nrow(data)/2), replace=FALSE)
sample2 <- setdiff(1:nrow(data), sample1)

### propensity score model
data[,D] <- as.factor(data[,D])
levels(data[,D]) <- c("D0","D1")  # necessary for caret implementation of ranger

DgivenGX.Model.sample1 <- caret::train(stats::as.formula(paste(D, paste(G,paste(X,collapse="+"),sep="+"), sep="~")), data=data[sample1,], method="nnet",
                                       preProc=c("center","scale"), trControl=caret::trainControl(method="cv"), linout=FALSE )
DgivenGX.Model.sample2 <- caret::train(stats::as.formula(paste(D, paste(G,paste(X,collapse="+"),sep="+"), sep="~")), data=data[sample2,], method="nnet",
                                       preProc=c("center","scale"), trControl=caret::trainControl(method="cv"), linout=FALSE )

data[,D] <- as.numeric(data[,D])-1

# treatment predictions
DgivenGX.Pred <- rep(NA, nrow(data))

DgivenGX.Pred[sample2] <- stats::predict(DgivenGX.Model.sample1, newdata = data[sample2,], type="prob")[,2]
DgivenGX.Pred[sample1] <- stats::predict(DgivenGX.Model.sample2, newdata = data[sample1,], type="prob")[,2]

### outcome regression model
YgivenDGX.Model.sample1 <- caret::train(stats::as.formula(paste(Y, paste(D,G,paste(X,collapse="+"),sep="+"), sep="~")), data=data[sample1,], method="nnet",
                                        preProc=c("center","scale"), trControl=caret::trainControl(method="cv"), linout=TRUE )
YgivenDGX.Model.sample2 <- caret::train(stats::as.formula(paste(Y, paste(D,G,paste(X,collapse="+"),sep="+"), sep="~")), data=data[sample2,], method="nnet",
                                        preProc=c("center","scale"), trControl=caret::trainControl(method="cv"), linout=TRUE )

### outcome predictions
YgivenGX.Pred_D0 <- YgivenGX.Pred_D1 <- rep(NA, nrow(data))

pred_data <- data
pred_data[,D] <- 0

YgivenGX.Pred_D0[sample2] <- stats::predict(YgivenDGX.Model.sample1, newdata = pred_data[sample2,])
YgivenGX.Pred_D0[sample1] <- stats::predict(YgivenDGX.Model.sample2, newdata = pred_data[sample1,])

pred_data <- data
pred_data[,D] <- 1

YgivenGX.Pred_D1[sample2] <- stats::predict(YgivenDGX.Model.sample1, newdata = pred_data[sample2,])
YgivenGX.Pred_D1[sample1] <- stats::predict(YgivenDGX.Model.sample2, newdata = pred_data[sample1,])

### Diagnostics 
# covariate overlap
p_score_nnet_g <- ggplot(data.frame(value=DgivenGX.Pred), aes(x=DgivenGX.Pred)) + 
  geom_histogram(bins=50) +
  labs(title = "(a) Neural networks", x = "Propensity score", y = "Frequency") +
  theme_minimal()
p_score_nnet_g <- ggplotGrob(p_score_nnet_g)

# Create a data frame with the predicted propensity scores and treatment indicator
df_ps <- data.frame(
  PS = DgivenGX.Pred,
  Treatment = factor(data[,D], levels = c(0,1), labels = c("Not Treated", "Treated"))
)

# Generate the facetted histogram
p_score_split_nnet_g <- ggplot(df_ps, aes(x = PS, fill = Treatment)) +
  geom_histogram(bins = 50, alpha = 0.6, position = "identity") +
  labs(
    title = "(a) Neural networks",
    x = "Propensity Score",
    y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        text = element_text(size = 12))
p_score_split_nnet_g <- ggplotGrob(p_score_split_nnet_g)

# covariate balance after weighting
balance_nnet <- sapply(c(G,X), ASD)

# outcome model fit
Ypred <- YgivenGX.Pred_D0*(1-DgivenGX.Pred) + YgivenGX.Pred_D1*DgivenGX.Pred
mse_nnet <- mean((data$Y - Ypred)^2)


##### ##### ##### ##### ##### ##### ##### ##### ##### 

##### ##### ##### ##### ##### ##### ##### ##### ##### 



##### Manually run the nuisance functions of cdgd0_pa
# treatment model
DgivenGX.Model <- stats::glm(stats::as.formula(paste(D, paste(G,paste(X,collapse="+"),sep="+"), sep="~")), data=data, family=stats::binomial(link="logit"))

# treatment predictions
DgivenGX.Pred <- rep(NA, nrow(data))
DgivenGX.Pred <- stats::predict(DgivenGX.Model, newdata = data, type="response")

### outcome regression model
YgivenDGX.Model <- stats::lm(stats::as.formula(paste(Y, paste(paste(D,c(G,X),sep="*"),collapse="+"), sep="~")), data=data)

# outcome predictions
YgivenGX.Pred_D0 <- YgivenGX.Pred_D1 <- rep(NA, nrow(data))

pred_data <- data
pred_data[,D] <- 0
YgivenGX.Pred_D0 <- stats::predict(YgivenDGX.Model, newdata = pred_data)

pred_data <- data
pred_data[,D] <- 1
YgivenGX.Pred_D1 <- stats::predict(YgivenDGX.Model, newdata = pred_data)

### Diagnostics 
# covariate overlap
p_score_para_g <- ggplot(data.frame(value=DgivenGX.Pred), aes(x=DgivenGX.Pred)) + 
  geom_histogram(bins=50) +
  labs(title = "(c) Parametric model", x = "Propensity score", y = "Frequency") +
  theme_minimal()
p_score_para_g <- ggplotGrob(p_score_para_g)

p_score_para_gran <- ggplot(data.frame(value=DgivenGX.Pred), aes(x=DgivenGX.Pred)) + 
  geom_histogram(bins=50) +
  labs(title = "Granparents/Relatives", x = "Propensity score", y = "Frequency") +
  theme_minimal()
p_score_para_gran <- ggplotGrob(p_score_para_gran)

# Create a data frame with the predicted propensity scores and treatment indicator
df_ps <- data.frame(
  PS = DgivenGX.Pred,
  Treatment = factor(data[,D], levels = c(0,1), labels = c("Not Treated", "Treated"))
)

# Generate the facetted histogram
p_score_split_para_g <- ggplot(df_ps, aes(x = PS, fill = Treatment)) +
  geom_histogram(bins = 50, alpha = 0.6, position = "identity") +
  labs(
    title = "(c) Parametric model",
    x = "Propensity Score",
    y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        text = element_text(size = 12))
p_score_split_para_g <- ggplotGrob(p_score_split_para_g)


# covariate balance after weighting
balance_para <- sapply(c(G,X), ASD)

# outcome model fit
# in order to make comparison with ML models (which are paired with cross-fitting) fair, we also use cross-fitting here to calculate the MSE
set.seed(1)
sample1 <- sample(nrow(data), floor(nrow(data)/2), replace=FALSE)
sample2 <- setdiff(1:nrow(data), sample1)

### outcome regression model
YgivenDGX.Model.sample1 <- stats::lm(stats::as.formula(paste(Y, paste(paste(D,c(G,X),sep="*"),collapse="+"), sep="~")), data=data[sample1,])
YgivenDGX.Model.sample2 <- stats::lm(stats::as.formula(paste(Y, paste(paste(D,c(G,X),sep="*"),collapse="+"), sep="~")), data=data[sample2,])

### outcome predictions
YgivenGX.Pred_D0 <- YgivenGX.Pred_D1 <- rep(NA, nrow(data))

pred_data <- data
pred_data[,D] <- 0

YgivenGX.Pred_D0[sample2] <- stats::predict(YgivenDGX.Model.sample1, newdata = pred_data[sample2,])
YgivenGX.Pred_D0[sample1] <- stats::predict(YgivenDGX.Model.sample2, newdata = pred_data[sample1,])

pred_data <- data
pred_data[,D] <- 1

YgivenGX.Pred_D1[sample2] <- stats::predict(YgivenDGX.Model.sample1, newdata = pred_data[sample2,])
YgivenGX.Pred_D1[sample1] <- stats::predict(YgivenDGX.Model.sample2, newdata = pred_data[sample1,])

Ypred <- YgivenGX.Pred_D0*(1-DgivenGX.Pred) + YgivenGX.Pred_D1*DgivenGX.Pred
mse_para <- mean((data$Y - Ypred)^2)


##### ##### ##### ##### ##### ##### ##### ##### ##### 

##### ##### ##### ##### ##### ##### ##### ##### ##### 




##### pool the results together
### Covariate overlap
gran_ps <- grid.arrange(
  p_score_nnet_g, 
  p_score_gbm_g, 
  p_score_para_g, 
  ncol = 1,
  top = "Grandparents/Relatives"
)
ggsave("PS_gran.png", plot = gran_ps, path=graphs, 
       dpi = 320, width = 12, height = 10, units = "in")


### Covariate overlap
gran_ps_split <- grid.arrange(
  p_score_split_nnet_g, 
  p_score_split_gbm_g, 
  p_score_split_para_g, 
  ncol = 1,
  top = "Grandparents/Relatives"
)
ggsave("PS_gran_split.png", plot = gran_ps_split, path=graphs, 
       dpi = 320, width = 12, height = 10, units = "in")

### Outcome model fit
## get the MSEs for intercept-only model
set.seed(1)
sample1 <- sample(nrow(data), floor(nrow(data)/2), replace=FALSE)
sample2 <- setdiff(1:nrow(data), sample1)

Ypred[sample1] <- mean(data$Y[sample2])
Ypred[sample2] <- mean(data$Y[sample1])

mse_intercept <- mean((data$Y - Ypred)^2)

c(mse_intercept, mse_para, mse_nnet, mse_gbm)

### Covariate balance after weighting
## get the balance when no weighted is used
ASD_unweighted <- function(j) {
  mean1 <- mean(data[,j]*(data[,D]/mean(data[,D])))
  mean0 <- mean(data[,j]*((1-data[,D])/mean((1-data[,D]))))
  
  var1 <- sum(data[,D])/( (sum(data[,D]))^2 - sum((data[,D])^2) ) * sum( data[,D]*(data[,j] - mean1)^2 )
  var0 <- sum((1-data[,D]))/( sum((1-data[,D]))^2 - sum(((1-data[,D]))^2) ) * sum( (1-data[,D])*(data[,j] - mean0)^2 )
  
  return( abs((mean1-mean0)/sqrt((var1+var0)/2)*100) )
}

balance_unweighted <- sapply(c(G,X), ASD_unweighted)

data_balance <- data.frame(model = rep(c("Parametric", "GBM", "Neural networks", "Unweighted"), each = length(balance_para)),
                           variable = rep(c(G,X), 4),
                           value = c(balance_para, balance_gbm, balance_nnet, balance_unweighted))
data_balance$variable <- factor(data_balance$variable, levels = c(G,X))
data_balance$model <- factor(data_balance$model, levels = c("Unweighted", "Parametric", "Neural networks", "GBM")) # for ordering the legend

GRAN_BALANCE <- ggplot(data_balance, aes(x = value, y = variable, shape = model, color = model)) +
  geom_point(size = 3) +
  scale_shape_manual(values = c(16, 17, 19, 15, 18)) + 
  scale_color_manual(values = c("black", "blue", "red", "green", "purple")) + 
  labs(x = "Absolute standardized difference", y = "") +
  scale_y_discrete(limits = rev(levels(data_balance$variable)), labels = covariate_labels) +  
  theme_minimal() +
  theme(axis.text.y = element_text(size = 11)) +
  geom_vline(xintercept = 20, linetype = "solid", color = "black") +
  guides(shape = guide_legend(title = ""), color = guide_legend(title = "")) 

GRAN_BALANCE



##### ##### ##### ##### ##### ##### ##### ##### ##### 
##### ##### ##### ##### ##### ##### ##### ##### ##### 
##### ##### ##### ##### ##### ##### ##### ##### #####



#'-----------------------------------------#
# ONLY PARENTS
#'-----------------------------------------#



#Y="math5_wle_100"
#D="pa3"
#G="SES_100_2"
#X=covariates
#data=data

data <- datagross

data$D <- data$pa3
#data$G <- data$SES_100_2
data$G <- data$eduM_B1
data$Y <- data$math5_wle_100 
# no differences if taking math or voc for covariates balance
#data$Y <- data$voc6_sum_100

Y="Y"
D="D"
G="G"
X=covariates
data=data

data <- data %>% select(G, D, all_of(X), Y, voc6_sum_100) %>% drop_na()



##### Manually run the nuisance functions of cdgd0_ml, gbm
set.seed(1)
data <- as.data.frame(data)

### estimate the nuisance functions with cross-fitting
sample1 <- sample(nrow(data), floor(nrow(data)/2), replace=FALSE)
sample2 <- setdiff(1:nrow(data), sample1)

### propensity score model
data[,D] <- as.factor(data[,D])
levels(data[,D]) <- c("D0","D1")  # necessary for caret implementation of ranger

DgivenGX.Model.sample1 <- caret::train(stats::as.formula(paste(D, paste(G,paste(X,collapse="+"),sep="+"), sep="~")), data=data[sample1,], method="gbm",
                                       trControl=caret::trainControl(method="cv"))
DgivenGX.Model.sample2 <- caret::train(stats::as.formula(paste(D, paste(G,paste(X,collapse="+"),sep="+"), sep="~")), data=data[sample2,], method="gbm",
                                       trControl=caret::trainControl(method="cv"))

data[,D] <- as.numeric(data[,D])-1

# treatment predictions
DgivenGX.Pred <- rep(NA, nrow(data))

DgivenGX.Pred[sample2] <- stats::predict(DgivenGX.Model.sample1, newdata = data[sample2,], type="prob")[,2]
DgivenGX.Pred[sample1] <- stats::predict(DgivenGX.Model.sample2, newdata = data[sample1,], type="prob")[,2]

### outcome regression model
YgivenDGX.Model.sample1 <- caret::train(stats::as.formula(paste(Y, paste(D,G,paste(X,collapse="+"),sep="+"), sep="~")), data=data[sample1,], method="gbm",
                                        trControl=caret::trainControl(method="cv"))
YgivenDGX.Model.sample2 <- caret::train(stats::as.formula(paste(Y, paste(D,G,paste(X,collapse="+"),sep="+"), sep="~")), data=data[sample2,], method="gbm",
                                        trControl=caret::trainControl(method="cv"))

### outcome predictions
YgivenGX.Pred_D0 <- YgivenGX.Pred_D1 <- rep(NA, nrow(data))

pred_data <- data
pred_data[,D] <- 0

YgivenGX.Pred_D0[sample2] <- stats::predict(YgivenDGX.Model.sample1, newdata = pred_data[sample2,])
YgivenGX.Pred_D0[sample1] <- stats::predict(YgivenDGX.Model.sample2, newdata = pred_data[sample1,])

pred_data <- data
pred_data[,D] <- 1

YgivenGX.Pred_D1[sample2] <- stats::predict(YgivenDGX.Model.sample1, newdata = pred_data[sample2,])
YgivenGX.Pred_D1[sample1] <- stats::predict(YgivenDGX.Model.sample2, newdata = pred_data[sample1,])

### Diagnostics 
# covariate overlap
p_score_gbm_p <- ggplot(data.frame(value=DgivenGX.Pred), aes(x=DgivenGX.Pred)) + 
  geom_histogram(bins=50) +
  labs(title = "(b) Generalized boosting machines", x = "Propensity score", y = "Frequency") +
  theme_minimal()
p_score_gbm_p <- ggplotGrob(p_score_gbm_p)

# Create a data frame with the predicted propensity scores and treatment indicator
df_ps <- data.frame(
  PS = DgivenGX.Pred,
  Treatment = factor(data[,D], levels = c(0,1), labels = c("Not Treated", "Treated"))
)

# Generate the facetted histogram
p_score_split_gbm_p <- ggplot(df_ps, aes(x = PS, fill = Treatment)) +
  geom_histogram(bins = 50, alpha = 0.6, position = "identity") +
  labs(
    title = "(b) Generalized boosting machines",
    x = "Propensity Score",
    y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        text = element_text(size = 12))
p_score_split_gbm_p <- ggplotGrob(p_score_split_gbm_p)

# covariate balance after weighting
balance_gbm <- sapply(c(G,X), ASD)

# outcome model fit
Ypred <- YgivenGX.Pred_D0*(1-DgivenGX.Pred) + YgivenGX.Pred_D1*DgivenGX.Pred
mse_gbm <- mean((data$Y - Ypred)^2)

##### ##### ##### ##### ##### ##### ##### ##### ##### 

##### ##### ##### ##### ##### ##### ##### ##### ##### 


##### Manually run the nuisance functions of cdgd0_ml, nnet
set.seed(1)
### estimate the nuisance functions with cross-fitting
sample1 <- sample(nrow(data), floor(nrow(data)/2), replace=FALSE)
sample2 <- setdiff(1:nrow(data), sample1)

### propensity score model
data[,D] <- as.factor(data[,D])
levels(data[,D]) <- c("D0","D1")  # necessary for caret implementation of ranger

DgivenGX.Model.sample1 <- caret::train(stats::as.formula(paste(D, paste(G,paste(X,collapse="+"),sep="+"), sep="~")), data=data[sample1,], method="nnet",
                                       preProc=c("center","scale"), trControl=caret::trainControl(method="cv"), linout=FALSE )
DgivenGX.Model.sample2 <- caret::train(stats::as.formula(paste(D, paste(G,paste(X,collapse="+"),sep="+"), sep="~")), data=data[sample2,], method="nnet",
                                       preProc=c("center","scale"), trControl=caret::trainControl(method="cv"), linout=FALSE )

data[,D] <- as.numeric(data[,D])-1

# treatment predictions
DgivenGX.Pred <- rep(NA, nrow(data))

DgivenGX.Pred[sample2] <- stats::predict(DgivenGX.Model.sample1, newdata = data[sample2,], type="prob")[,2]
DgivenGX.Pred[sample1] <- stats::predict(DgivenGX.Model.sample2, newdata = data[sample1,], type="prob")[,2]

### outcome regression model
YgivenDGX.Model.sample1 <- caret::train(stats::as.formula(paste(Y, paste(D,G,paste(X,collapse="+"),sep="+"), sep="~")), data=data[sample1,], method="nnet",
                                        preProc=c("center","scale"), trControl=caret::trainControl(method="cv"), linout=TRUE )
YgivenDGX.Model.sample2 <- caret::train(stats::as.formula(paste(Y, paste(D,G,paste(X,collapse="+"),sep="+"), sep="~")), data=data[sample2,], method="nnet",
                                        preProc=c("center","scale"), trControl=caret::trainControl(method="cv"), linout=TRUE )

### outcome predictions
YgivenGX.Pred_D0 <- YgivenGX.Pred_D1 <- rep(NA, nrow(data))

pred_data <- data
pred_data[,D] <- 0

YgivenGX.Pred_D0[sample2] <- stats::predict(YgivenDGX.Model.sample1, newdata = pred_data[sample2,])
YgivenGX.Pred_D0[sample1] <- stats::predict(YgivenDGX.Model.sample2, newdata = pred_data[sample1,])

pred_data <- data
pred_data[,D] <- 1

YgivenGX.Pred_D1[sample2] <- stats::predict(YgivenDGX.Model.sample1, newdata = pred_data[sample2,])
YgivenGX.Pred_D1[sample1] <- stats::predict(YgivenDGX.Model.sample2, newdata = pred_data[sample1,])

### Diagnostics 
# covariate overlap
p_score_nnet_p <- ggplot(data.frame(value=DgivenGX.Pred), aes(x=DgivenGX.Pred)) + 
  geom_histogram(bins=50) +
  labs(title = "(a) Neural networks", x = "Propensity score", y = "Frequency") +
  theme_minimal()
p_score_nnet_p <- ggplotGrob(p_score_nnet_p)

# Create a data frame with the predicted propensity scores and treatment indicator
df_ps <- data.frame(
  PS = DgivenGX.Pred,
  Treatment = factor(data[,D], levels = c(0,1), labels = c("Not Treated", "Treated"))
)

# Generate the facetted histogram
p_score_split_nnet_p <- ggplot(df_ps, aes(x = PS, fill = Treatment)) +
  geom_histogram(bins = 50, alpha = 0.6, position = "identity") +
  labs(
    title = "(a) Neural networks",
    x = "Propensity Score",
    y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        text = element_text(size = 12))
p_score_split_nnet_p <- ggplotGrob(p_score_split_nnet_p)

# covariate balance after weighting
balance_nnet <- sapply(c(G,X), ASD)

# outcome model fit
Ypred <- YgivenGX.Pred_D0*(1-DgivenGX.Pred) + YgivenGX.Pred_D1*DgivenGX.Pred
mse_nnet <- mean((data$Y - Ypred)^2)


##### ##### ##### ##### ##### ##### ##### ##### ##### 

##### ##### ##### ##### ##### ##### ##### ##### ##### 



##### Manually run the nuisance functions of cdgd0_pa
# treatment model
DgivenGX.Model <- stats::glm(stats::as.formula(paste(D, paste(G,paste(X,collapse="+"),sep="+"), sep="~")), data=data, family=stats::binomial(link="logit"))

# treatment predictions
DgivenGX.Pred <- rep(NA, nrow(data))
DgivenGX.Pred <- stats::predict(DgivenGX.Model, newdata = data, type="response")

### outcome regression model
YgivenDGX.Model <- stats::lm(stats::as.formula(paste(Y, paste(paste(D,c(G,X),sep="*"),collapse="+"), sep="~")), data=data)

# outcome predictions
YgivenGX.Pred_D0 <- YgivenGX.Pred_D1 <- rep(NA, nrow(data))

pred_data <- data
pred_data[,D] <- 0
YgivenGX.Pred_D0 <- stats::predict(YgivenDGX.Model, newdata = pred_data)

pred_data <- data
pred_data[,D] <- 1
YgivenGX.Pred_D1 <- stats::predict(YgivenDGX.Model, newdata = pred_data)

### Diagnostics 
# covariate overlap
p_score_para_p <- ggplot(data.frame(value=DgivenGX.Pred), aes(x=DgivenGX.Pred)) + 
  geom_histogram(bins=50) +
  labs(title = "(c) Parametric model", x = "Propensity score", y = "Frequency") +
  theme_minimal()
p_score_para_p <- ggplotGrob(p_score_para_p)

p_score_para_par <- ggplot(data.frame(value=DgivenGX.Pred), aes(x=DgivenGX.Pred)) + 
  geom_histogram(bins=50) +
  labs(title = "Parental Care Only", x = "Propensity score", y = "Frequency") +
  theme_minimal()
p_score_para_par <- ggplotGrob(p_score_para_par)

# Create a data frame with the predicted propensity scores and treatment indicator
df_ps <- data.frame(
  PS = DgivenGX.Pred,
  Treatment = factor(data[,D], levels = c(0,1), labels = c("Not Treated", "Treated"))
)

# Generate the facetted histogram
p_score_split_para_p <- ggplot(df_ps, aes(x = PS, fill = Treatment)) +
  geom_histogram(bins = 50, alpha = 0.6, position = "identity") +
  labs(
    title = "(c) Parametric model",
    x = "Propensity Score",
    y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        text = element_text(size = 12))
p_score_split_para_p <- ggplotGrob(p_score_split_para_p)


# covariate balance after weighting
balance_para <- sapply(c(G,X), ASD)

# outcome model fit
# in order to make comparison with ML models (which are paired with cross-fitting) fair, we also use cross-fitting here to calculate the MSE
set.seed(1)
sample1 <- sample(nrow(data), floor(nrow(data)/2), replace=FALSE)
sample2 <- setdiff(1:nrow(data), sample1)

### outcome regression model
YgivenDGX.Model.sample1 <- stats::lm(stats::as.formula(paste(Y, paste(paste(D,c(G,X),sep="*"),collapse="+"), sep="~")), data=data[sample1,])
YgivenDGX.Model.sample2 <- stats::lm(stats::as.formula(paste(Y, paste(paste(D,c(G,X),sep="*"),collapse="+"), sep="~")), data=data[sample2,])

### outcome predictions
YgivenGX.Pred_D0 <- YgivenGX.Pred_D1 <- rep(NA, nrow(data))

pred_data <- data
pred_data[,D] <- 0

YgivenGX.Pred_D0[sample2] <- stats::predict(YgivenDGX.Model.sample1, newdata = pred_data[sample2,])
YgivenGX.Pred_D0[sample1] <- stats::predict(YgivenDGX.Model.sample2, newdata = pred_data[sample1,])

pred_data <- data
pred_data[,D] <- 1

YgivenGX.Pred_D1[sample2] <- stats::predict(YgivenDGX.Model.sample1, newdata = pred_data[sample2,])
YgivenGX.Pred_D1[sample1] <- stats::predict(YgivenDGX.Model.sample2, newdata = pred_data[sample1,])

Ypred <- YgivenGX.Pred_D0*(1-DgivenGX.Pred) + YgivenGX.Pred_D1*DgivenGX.Pred
mse_para <- mean((data$Y - Ypred)^2)


##### ##### ##### ##### ##### ##### ##### ##### ##### 

##### ##### ##### ##### ##### ##### ##### ##### ##### 




##### pool the results together
### Covariate overlap
par_ps <- grid.arrange(
  p_score_nnet_p, 
  p_score_gbm_p, 
  p_score_para_p, 
  ncol = 1,
  top = "Parental Care Only"
)

ggsave("PS_parents.png", plot = par_ps, path=graphs, 
       dpi = 320, width = 12, height = 10, units = "in")

### Covariate overlap
par_ps_split <- grid.arrange(
  p_score_split_nnet_p, 
  p_score_split_gbm_p, 
  p_score_split_para_p, 
  ncol = 1,
  top = "Parental Care Only"
)
ggsave("PS_par_split.png", plot = par_ps_split, path=graphs, 
       dpi = 320, width = 12, height = 10, units = "in")

### Outcome model fit
## get the MSEs for intercept-only model
set.seed(1)
sample1 <- sample(nrow(data), floor(nrow(data)/2), replace=FALSE)
sample2 <- setdiff(1:nrow(data), sample1)

Ypred[sample1] <- mean(data$Y[sample2])
Ypred[sample2] <- mean(data$Y[sample1])

mse_intercept <- mean((data$Y - Ypred)^2)

c(mse_intercept, mse_para, mse_nnet, mse_gbm)

### Covariate balance after weighting
## get the balance when no weighted is used
ASD_unweighted <- function(j) {
  mean1 <- mean(data[,j]*(data[,D]/mean(data[,D])))
  mean0 <- mean(data[,j]*((1-data[,D])/mean((1-data[,D]))))
  
  var1 <- sum(data[,D])/( (sum(data[,D]))^2 - sum((data[,D])^2) ) * sum( data[,D]*(data[,j] - mean1)^2 )
  var0 <- sum((1-data[,D]))/( sum((1-data[,D]))^2 - sum(((1-data[,D]))^2) ) * sum( (1-data[,D])*(data[,j] - mean0)^2 )
  
  return( abs((mean1-mean0)/sqrt((var1+var0)/2)*100) )
}

balance_unweighted <- sapply(c(G,X), ASD_unweighted)

data_balance <- data.frame(model = rep(c("Parametric", "GBM", "Neural networks", "Unweighted"), each = length(balance_para)),
                           variable = rep(c(G,X), 4),
                           value = c(balance_para, balance_gbm, balance_nnet, balance_unweighted))
data_balance$variable <- factor(data_balance$variable, levels = c(G,X))
data_balance$model <- factor(data_balance$model, levels = c("Unweighted", "Parametric", "Neural networks", "GBM")) # for ordering the legend

PAR_BALANCE <- ggplot(data_balance, aes(x = value, y = variable, shape = model, color = model)) +
  geom_point(size = 3) +
  scale_shape_manual(values = c(16, 17, 19, 15, 18)) + 
  scale_color_manual(values = c("black", "blue", "red", "green", "purple")) + 
  labs(x = "Absolute standardized difference", y = "") +
  scale_y_discrete(limits = rev(levels(data_balance$variable)), labels = covariate_labels) +  
  theme_minimal() +
  theme(axis.text.y = element_text(size = 11)) +
  geom_vline(xintercept = 20, linetype = "solid", color = "black") +
  guides(shape = guide_legend(title = ""), color = guide_legend(title = "")) 

PAR_BALANCE

  
##### ##### ##### ##### ##### ##### ##### ##### ##### 
##### ##### ##### ##### ##### ##### ##### ##### ##### 
##### ##### ##### ##### ##### ##### ##### ##### #####
# PUT TOGETHER GRAPHS FOR BALANCE PS_SCORE & COVARIATES 


library(ggpubr)

##### pool the results together
### Covariate overlap
ps_parametric <- grid.arrange(
  p_score_para_ecec, 
  p_score_para_fdc, 
  p_score_para_gran, 
  p_score_para_par, 
  ncol = 2, nrow = 2, 
  top = "Propensity score distribution"
)

ggsave("PS_parametric.png", plot = ps_parametric, path=graphs, 
       dpi = 320, width = 12, height = 10, units = "in")

# BALANCE COVARIATES
balance <- ggarrange(
  ECEC_BALANCE + ggtitle("Center-based ECEC"), 
  FDC_BALANCE + ggtitle("Family day care"), 
  GRAN_BALANCE + ggtitle("Grandparents/Relatives"), 
  PAR_BALANCE + ggtitle("Parental Care Only"), 
  ncol = 2, nrow = 2, 
  common.legend = TRUE, legend = "bottom"
)
print(balance)

ggsave("Balance_covariates.png", plot = balance, path=graphs, 
       dpi = 320, width = 12, height = 10, units = "in")

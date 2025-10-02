#'############################################'
#' NEPS - GROUP DISPARITIES DECOMPOSITION          
#'############################################'


#'-----------------------------------------#
# Globals directories ---------------------------------
#'-----------------------------------------#
#'

#input 
wd <- file.path("...")
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


#'-----------------------------------------#
# Open data ---------------------------------
#'-----------------------------------------#
#'
#

#import 
wkname <- file.path(data, "data_selected.dta")
wkfile <- read_dta(wkname, encoding = 'latin1', col_select = NULL,  skip = 0, n_max = Inf, .name_repair = "check_unique")  



#'-----------------------------------------#
# Variables of interest & subdatasets ---------------------------------
#'-----------------------------------------#

#define variables of interest
#dependent <- c("math5_wle_100", "can4_wle_100", "voc6_sum_100")
dependent <- c("math5_wle_100", "voc6_sum_100")

wkfile$math5_wle <- as.numeric(wkfile$math5_wle)
#wkfile$can4_wle <- as.numeric(wkfile$can4_wle)
wkfile$voc6_sum <- as.numeric(wkfile$voc6_sum)
wkfile$math5_wle_100 <- as.numeric(wkfile$math5_wle_100)
#wkfile$can4_wle_100 <- as.numeric(wkfile$can4_wle_100)
wkfile$voc6_sum_100 <- as.numeric(wkfile$voc6_sum_100)

covariates <- c("c_gender1", "c_weight1", "c_healthDD2", "place_residence1", 
                "single_parent1", "c_migr_n1", "n_siblings1", "n_sib_d", "sensori1_wle_5")

wkfile$c_gender1 <- factor(wkfile$c_gender1,levels = c(0,1)) 
wkfile$c_weight1 <- factor(wkfile$c_weight1,levels = c(0,1))
wkfile$c_healthDD2 <- factor(wkfile$c_healthDD2,levels = c(0,1))
wkfile$place_residence1 <- factor(wkfile$place_residence1,levels = c(0,1))
wkfile$single_parent1 <- factor(wkfile$single_parent1,levels = c(0,1))
wkfile$c_migr_n1 <- factor(wkfile$c_migr_n1,levels = c(0,1))
wkfile$n_siblings1 <- factor(wkfile$n_siblings1,levels = c(0,1,2))
wkfile$n_sib_d <- factor(wkfile$n_sib_d,levels = c(0,1))
wkfile$sensori1_wle_5 <- factor(wkfile$sensori1_wle_5,levels = c(1,2,3,4,5))
#wkfile$sensori1_wle_10 <- factor(wkfile$sensori1_wle_10,levels = c(1,2,3,4,5,6,7,8,9,10))
#wkfile$sensori1_wle <- as.numeric(wkfile$sensori1_wle)

#'-----------------------------------------#
#'-----------------------------------------#


# Combining separate SES dimension & childcare treatment (keep all of them in the sample to get same sample size)
treatment <- c("ecec_cb_attendance3", "ecec_fdc_attendance3", "grandp_relatives3", "pa3")

#'-----------------------------------------#

# Alternative definitions A- ecec_cb_only, fdc_only, grandp_relatives_only - keep separated as baseline exclusive
#ECEC_A <- c("ecec_cb_only")
#FDC_A <- c("fdc_only")
#GRAN_A <- c("grandp_relatives_only")
#PAR_A <- c("pa3")

#'-----------------------------------------#

# Alternative definitions B- ecec_cb_only_A, fdc_only_A, grandp_relatives_only_A  - keep separated as baseline exclusive
ECEC_B <- c("ecec_cb_only_A")
FDC_B <- c("fdc_only_A")
GRAN_B <- c("grandp_relatives_only_A")
PAR_B <- c("pa3")

#'-----------------------------------------#
#'-----------------------------------------#

# MAIN ESTIMATION - SAMPLE WITH EDUCATION MOTHER
eduM_ecec <- wkfile %>% select(eduM_B1, all_of(treatment), all_of(covariates), all_of(dependent)) %>% drop_na()
eduM_fdc <- wkfile %>% select(eduM_B1, all_of(treatment), all_of(covariates), all_of(dependent)) %>% drop_na()
eduM_gran <- wkfile %>% select(eduM_B1, all_of(treatment), all_of(covariates), all_of(dependent)) %>% drop_na()
eduM_par <- wkfile %>% select(eduM_B1, all_of(treatment), all_of(covariates), all_of(dependent)) %>% drop_na()

#'-----------------------------------------#

# ALTERNATIVE ESTIMATION - SAMPLE WITH EDUCATION MOTHER - TREATMENT STRICT VERSION B
ses5_ececB <- wkfile %>% select(eduM_B1, all_of(ECEC_B), all_of(covariates), all_of(dependent)) %>% drop_na()
ses5_fdcB <- wkfile %>% select(eduM_B1, all_of(FDC_B), all_of(covariates), all_of(dependent)) %>% drop_na()
ses5_granB <- wkfile %>% select(eduM_B1, all_of(GRAN_B), all_of(covariates), all_of(dependent)) %>% drop_na()
ses5_parB <- wkfile %>% select(eduM_B1, all_of(PAR_B), all_of(covariates), all_of(dependent)) %>% drop_na()


#'-----------------------------------------#
#'-----------------------------------------#

#'############################################'#'############################################'
#'############################################'#'############################################'
#'############################################'#'############################################'
#'############################################'#'############################################'
#'
#'



#'-----------------------------------------#
#'-----------------------------------------#
# MAIN ANALYSIS SES5 - MATERNAL EDUCATION  ---------------------------------
#'-----------------------------------------#
#'-----------------------------------------#

##### Unconditional decomposition:  math5_wle_100 =================================

### ECEC
#
EDUM_math_ecec_pa <- cdgd0_pa(Y="math5_wle_100",D="ecec_cb_attendance3",G="eduM_B1",
                              X=covariates, 
                              data=eduM_ecec)
#
set.seed(1)
EDUM_math_ecec_gbm <- cdgd0_ml(Y="math5_wle_100",D="ecec_cb_attendance3",G="eduM_B1",
                               X=covariates,
                               data=eduM_ecec,algorithm="gbm")
#
set.seed(1)
EDUM_math_ecec_nnet <- cdgd0_ml(Y="math5_wle_100",D="ecec_cb_attendance3",G="eduM_B1",
                                X=covariates,
                                data=eduM_ecec,algorithm="nnet")

GEDUM_math_ecec_pa <- as.data.frame(cbind(c("Total","Baseline","Prevalence","Effect","Selection"),EDUM_math_ecec_pa$results))
GEDUM_math_ecec_gbm <- as.data.frame(cbind(c("Total","Baseline","Prevalence","Effect","Selection"),EDUM_math_ecec_gbm$results))
GEDUM_math_ecec_nnet <- as.data.frame(cbind(c("Total","Baseline","Prevalence","Effect","Selection"),EDUM_math_ecec_nnet$results))
colnames(GEDUM_math_ecec_pa)[1:2] <- colnames(GEDUM_math_ecec_nnet)[1:2] <- colnames(GEDUM_math_ecec_gbm)[1:2] <- c("Component","Estimate")

GEDUM_math_ecec <- as.data.frame(rbind(GEDUM_math_ecec_pa,GEDUM_math_ecec_nnet,GEDUM_math_ecec_gbm))
GEDUM_math_ecec$Model <- factor(c(rep("Param",5),rep("Nnet",5),rep("GBM",5)), levels=c("Param","Nnet","GBM"))
GEDUM_math_ecec <- GEDUM_math_ecec[GEDUM_math_ecec$Component!="Total",]
GEDUM_math_ecec$Component <- factor(GEDUM_math_ecec$Component, levels=c("Baseline","Prevalence","Effect","Selection"))

GEDUM_math_ecec_pa <- GEDUM_math_ecec_pa[GEDUM_math_ecec_pa$Component!="Total",]
GEDUM_math_ecec_pa$Component <- factor(GEDUM_math_ecec_pa$Component, levels=c("Baseline","Prevalence","Effect","Selection"))

### FAMILY DAY CARE
#
EDUM_math_fdc_pa <- cdgd0_pa(Y="math5_wle_100",D="ecec_fdc_attendance3",G="eduM_B1",
                             X=covariates, 
                             data=eduM_fdc)
#
set.seed(1)
EDUM_math_fdc_gbm <- cdgd0_ml(Y="math5_wle_100",D="ecec_fdc_attendance3",G="eduM_B1",
                              X=covariates,
                              data=eduM_fdc,algorithm="gbm")
#
set.seed(1)
EDUM_math_fdc_nnet <- cdgd0_ml(Y="math5_wle_100",D="ecec_fdc_attendance3",G="eduM_B1",
                               X=covariates,
                               data=eduM_fdc,algorithm="nnet")

GEDUM_math_fdc_pa <- as.data.frame(cbind(c("Total","Baseline","Prevalence","Effect","Selection"),EDUM_math_fdc_pa$results))
GEDUM_math_fdc_gbm <- as.data.frame(cbind(c("Total","Baseline","Prevalence","Effect","Selection"),EDUM_math_fdc_gbm$results))
GEDUM_math_fdc_nnet <- as.data.frame(cbind(c("Total","Baseline","Prevalence","Effect","Selection"),EDUM_math_fdc_nnet$results))
colnames(GEDUM_math_fdc_pa)[1:2] <- colnames(GEDUM_math_fdc_nnet)[1:2] <- colnames(GEDUM_math_fdc_gbm)[1:2] <- c("Component","Estimate")

GEDUM_math_fdc <- as.data.frame(rbind(GEDUM_math_fdc_pa,GEDUM_math_fdc_nnet,GEDUM_math_fdc_gbm))
GEDUM_math_fdc$Model <- factor(c(rep("Param",5),rep("Nnet",5),rep("GBM",5)), levels=c("Param","Nnet","GBM"))
GEDUM_math_fdc <- GEDUM_math_fdc[GEDUM_math_fdc$Component!="Total",]
GEDUM_math_fdc$Component <- factor(GEDUM_math_fdc$Component, levels=c("Baseline","Prevalence","Effect","Selection"))

GEDUM_math_fdc_pa <- GEDUM_math_fdc_pa[GEDUM_math_fdc_pa$Component!="Total",]
GEDUM_math_fdc_pa$Component <- factor(GEDUM_math_fdc_pa$Component, levels=c("Baseline","Prevalence","Effect","Selection"))


### RELATIVES/GRANDPARENTS
#
EDUM_math_gran_pa <- cdgd0_pa(Y="math5_wle_100",D="grandp_relatives3",G="eduM_B1",
                              X=covariates, 
                              data=eduM_gran)
#
set.seed(1)
EDUM_math_gran_gbm <- cdgd0_ml(Y="math5_wle_100",D="grandp_relatives3",G="eduM_B1",
                               X=covariates,
                               data=eduM_gran,algorithm="gbm")
#
set.seed(1)
EDUM_math_gran_nnet <- cdgd0_ml(Y="math5_wle_100",D="grandp_relatives3",G="eduM_B1",
                                X=covariates,
                                data=eduM_gran,algorithm="nnet")

GEDUM_math_gran_pa <- as.data.frame(cbind(c("Total","Baseline","Prevalence","Effect","Selection"),EDUM_math_gran_pa$results))
GEDUM_math_gran_gbm <- as.data.frame(cbind(c("Total","Baseline","Prevalence","Effect","Selection"),EDUM_math_gran_gbm$results))
GEDUM_math_gran_nnet <- as.data.frame(cbind(c("Total","Baseline","Prevalence","Effect","Selection"),EDUM_math_gran_nnet$results))
colnames(GEDUM_math_gran_pa)[1:2] <- colnames(GEDUM_math_gran_nnet)[1:2] <- colnames(GEDUM_math_gran_gbm)[1:2] <- c("Component","Estimate")

GEDUM_math_gran <- as.data.frame(rbind(GEDUM_math_gran_pa,GEDUM_math_gran_nnet,GEDUM_math_gran_gbm))
GEDUM_math_gran$Model <- factor(c(rep("Param",5),rep("Nnet",5),rep("GBM",5)), levels=c("Param","Nnet","GBM"))
GEDUM_math_gran <- GEDUM_math_gran[GEDUM_math_gran$Component!="Total",]
GEDUM_math_gran$Component <- factor(GEDUM_math_gran$Component, levels=c("Baseline","Prevalence","Effect","Selection"))

GEDUM_math_gran_pa <- GEDUM_math_gran_pa[GEDUM_math_gran_pa$Component!="Total",]
GEDUM_math_gran_pa$Component <- factor(GEDUM_math_gran_pa$Component, levels=c("Baseline","Prevalence","Effect","Selection"))

### PARENTS
#
EDUM_math_par_pa <- cdgd0_pa(Y="math5_wle_100",D="pa3",G="eduM_B1",
                             X=covariates, 
                             data=eduM_par)
#
set.seed(1)
EDUM_math_par_gbm <- cdgd0_ml(Y="math5_wle_100",D="pa3",G="eduM_B1",
                              X=covariates,
                              data=eduM_par,algorithm="gbm")
#
set.seed(1)
EDUM_math_par_nnet <- cdgd0_ml(Y="math5_wle_100",D="pa3",G="eduM_B1",
                               X=covariates,
                               data=eduM_par,algorithm="nnet")

GEDUM_math_par_pa <- as.data.frame(cbind(c("Total","Baseline","Prevalence","Effect","Selection"),EDUM_math_par_pa$results))
GEDUM_math_par_gbm <- as.data.frame(cbind(c("Total","Baseline","Prevalence","Effect","Selection"),EDUM_math_par_gbm$results))
GEDUM_math_par_nnet <- as.data.frame(cbind(c("Total","Baseline","Prevalence","Effect","Selection"),EDUM_math_par_nnet$results))
colnames(GEDUM_math_par_pa)[1:2] <- colnames(GEDUM_math_par_nnet)[1:2] <- colnames(GEDUM_math_par_gbm)[1:2] <- c("Component","Estimate")

GEDUM_math_par <- as.data.frame(rbind(GEDUM_math_par_pa,GEDUM_math_par_nnet,GEDUM_math_par_gbm))
GEDUM_math_par$Model <- factor(c(rep("Param",5),rep("Nnet",5),rep("GBM",5)), levels=c("Param","Nnet","GBM"))
GEDUM_math_par <- GEDUM_math_par[GEDUM_math_par$Component!="Total",]
GEDUM_math_par$Component <- factor(GEDUM_math_par$Component, levels=c("Baseline","Prevalence","Effect","Selection"))

GEDUM_math_par_pa <- GEDUM_math_par_pa[GEDUM_math_par_pa$Component!="Total",]
GEDUM_math_par_pa$Component <- factor(GEDUM_math_par_pa$Component, levels=c("Baseline","Prevalence","Effect","Selection"))

#'############################################'#'############################################'
#'############################################'#'############################################'
##### Unconditional decomposition:  voc6_sum_100 =================================


### ECEC
#
EDUM_voc_ecec_pa <- cdgd0_pa(Y="voc6_sum_100",D="ecec_cb_attendance3",G="eduM_B1",
                             X=covariates, 
                             data=eduM_ecec)
#
set.seed(1)
EDUM_voc_ecec_gbm <- cdgd0_ml(Y="voc6_sum_100",D="ecec_cb_attendance3",G="eduM_B1",
                              X=covariates,
                              data=eduM_ecec,algorithm="gbm")
#
set.seed(1)
EDUM_voc_ecec_nnet <- cdgd0_ml(Y="voc6_sum_100",D="ecec_cb_attendance3",G="eduM_B1",
                               X=covariates,
                               data=eduM_ecec,algorithm="nnet")

GEDUM_voc_ecec_pa <- as.data.frame(cbind(c("Total","Baseline","Prevalence","Effect","Selection"),EDUM_voc_ecec_pa$results))
GEDUM_voc_ecec_gbm <- as.data.frame(cbind(c("Total","Baseline","Prevalence","Effect","Selection"),EDUM_voc_ecec_gbm$results))
GEDUM_voc_ecec_nnet <- as.data.frame(cbind(c("Total","Baseline","Prevalence","Effect","Selection"),EDUM_voc_ecec_nnet$results))
colnames(GEDUM_voc_ecec_pa)[1:2] <- colnames(GEDUM_voc_ecec_nnet)[1:2] <- colnames(GEDUM_voc_ecec_gbm)[1:2] <- c("Component","Estimate")

GEDUM_voc_ecec <- as.data.frame(rbind(GEDUM_voc_ecec_pa,GEDUM_voc_ecec_nnet,GEDUM_voc_ecec_gbm))
GEDUM_voc_ecec$Model <- factor(c(rep("Param",5),rep("Nnet",5),rep("GBM",5)), levels=c("Param","Nnet","GBM"))
GEDUM_voc_ecec <- GEDUM_voc_ecec[GEDUM_voc_ecec$Component!="Total",]
GEDUM_voc_ecec$Component <- factor(GEDUM_voc_ecec$Component, levels=c("Baseline","Prevalence","Effect","Selection"))

GEDUM_voc_ecec_pa <- GEDUM_voc_ecec_pa[GEDUM_voc_ecec_pa$Component!="Total",]
GEDUM_voc_ecec_pa$Component <- factor(GEDUM_voc_ecec_pa$Component, levels=c("Baseline","Prevalence","Effect","Selection"))

### FAMILY DAY CARE
#
EDUM_voc_fdc_pa <- cdgd0_pa(Y="voc6_sum_100",D="ecec_fdc_attendance3",G="eduM_B1",
                            X=covariates, 
                            data=eduM_fdc)
#
set.seed(1)
EDUM_voc_fdc_gbm <- cdgd0_ml(Y="voc6_sum_100",D="ecec_fdc_attendance3",G="eduM_B1",
                             X=covariates,
                             data=eduM_fdc,algorithm="gbm")
#
set.seed(1)
EDUM_voc_fdc_nnet <- cdgd0_ml(Y="voc6_sum_100",D="ecec_fdc_attendance3",G="eduM_B1",
                              X=covariates,
                              data=eduM_fdc,algorithm="nnet")

GEDUM_voc_fdc_pa <- as.data.frame(cbind(c("Total","Baseline","Prevalence","Effect","Selection"),EDUM_voc_fdc_pa$results))
GEDUM_voc_fdc_gbm <- as.data.frame(cbind(c("Total","Baseline","Prevalence","Effect","Selection"),EDUM_voc_fdc_gbm$results))
GEDUM_voc_fdc_nnet <- as.data.frame(cbind(c("Total","Baseline","Prevalence","Effect","Selection"),EDUM_voc_fdc_nnet$results))
colnames(GEDUM_voc_fdc_pa)[1:2] <- colnames(GEDUM_voc_fdc_nnet)[1:2] <- colnames(GEDUM_voc_fdc_gbm)[1:2] <- c("Component","Estimate")

GEDUM_voc_fdc <- as.data.frame(rbind(GEDUM_voc_fdc_pa,GEDUM_voc_fdc_nnet,GEDUM_voc_fdc_gbm))
GEDUM_voc_fdc$Model <- factor(c(rep("Param",5),rep("Nnet",5),rep("GBM",5)), levels=c("Param","Nnet","GBM"))
GEDUM_voc_fdc <- GEDUM_voc_fdc[GEDUM_voc_fdc$Component!="Total",]
GEDUM_voc_fdc$Component <- factor(GEDUM_voc_fdc$Component, levels=c("Baseline","Prevalence","Effect","Selection"))

GEDUM_voc_fdc_pa <- GEDUM_voc_fdc_pa[GEDUM_voc_fdc_pa$Component!="Total",]
GEDUM_voc_fdc_pa$Component <- factor(GEDUM_voc_fdc_pa$Component, levels=c("Baseline","Prevalence","Effect","Selection"))

### RELATIVES/GRANDPARENTS
#
EDUM_voc_gran_pa <- cdgd0_pa(Y="voc6_sum_100",D="grandp_relatives3",G="eduM_B1",
                             X=covariates, 
                             data=eduM_gran)
#
set.seed(1)
EDUM_voc_gran_gbm <- cdgd0_ml(Y="voc6_sum_100",D="grandp_relatives3",G="eduM_B1",
                              X=covariates,
                              data=eduM_gran,algorithm="gbm")
#
set.seed(1)
EDUM_voc_gran_nnet <- cdgd0_ml(Y="voc6_sum_100",D="grandp_relatives3",G="eduM_B1",
                               X=covariates,
                               data=eduM_gran,algorithm="nnet")

GEDUM_voc_gran_pa <- as.data.frame(cbind(c("Total","Baseline","Prevalence","Effect","Selection"),EDUM_voc_gran_pa$results))
GEDUM_voc_gran_gbm <- as.data.frame(cbind(c("Total","Baseline","Prevalence","Effect","Selection"),EDUM_voc_gran_gbm$results))
GEDUM_voc_gran_nnet <- as.data.frame(cbind(c("Total","Baseline","Prevalence","Effect","Selection"),EDUM_voc_gran_nnet$results))
colnames(GEDUM_voc_gran_pa)[1:2] <- colnames(GEDUM_voc_gran_nnet)[1:2] <- colnames(GEDUM_voc_gran_gbm)[1:2] <- c("Component","Estimate")

GEDUM_voc_gran <- as.data.frame(rbind(GEDUM_voc_gran_pa,GEDUM_voc_gran_nnet,GEDUM_voc_gran_gbm))
GEDUM_voc_gran$Model <- factor(c(rep("Param",5),rep("Nnet",5),rep("GBM",5)), levels=c("Param","Nnet","GBM"))
GEDUM_voc_gran <- GEDUM_voc_gran[GEDUM_voc_gran$Component!="Total",]
GEDUM_voc_gran$Component <- factor(GEDUM_voc_gran$Component, levels=c("Baseline","Prevalence","Effect","Selection"))

GEDUM_voc_gran_pa <- GEDUM_voc_gran_pa[GEDUM_voc_gran_pa$Component!="Total",]
GEDUM_voc_gran_pa$Component <- factor(GEDUM_voc_gran_pa$Component, levels=c("Baseline","Prevalence","Effect","Selection"))

### PARENTS
#
EDUM_voc_par_pa <- cdgd0_pa(Y="voc6_sum_100",D="pa3",G="eduM_B1",
                            X=covariates, 
                            data=eduM_par)
#
set.seed(1)
EDUM_voc_par_gbm <- cdgd0_ml(Y="voc6_sum_100",D="pa3",G="eduM_B1",
                             X=covariates,
                             data=eduM_par,algorithm="gbm")
#
set.seed(1)
EDUM_voc_par_nnet <- cdgd0_ml(Y="voc6_sum_100",D="pa3",G="eduM_B1",
                              X=covariates,
                              data=eduM_par,algorithm="nnet")

GEDUM_voc_par_pa <- as.data.frame(cbind(c("Total","Baseline","Prevalence","Effect","Selection"),EDUM_voc_par_pa$results))
GEDUM_voc_par_gbm <- as.data.frame(cbind(c("Total","Baseline","Prevalence","Effect","Selection"),EDUM_voc_par_gbm$results))
GEDUM_voc_par_nnet <- as.data.frame(cbind(c("Total","Baseline","Prevalence","Effect","Selection"),EDUM_voc_par_nnet$results))
colnames(GEDUM_voc_par_pa)[1:2] <- colnames(GEDUM_voc_par_nnet)[1:2] <- colnames(GEDUM_voc_par_gbm)[1:2] <- c("Component","Estimate")

GEDUM_voc_par <- as.data.frame(rbind(GEDUM_voc_par_pa,GEDUM_voc_par_nnet,GEDUM_voc_par_gbm))
GEDUM_voc_par$Model <- factor(c(rep("Param",5),rep("Nnet",5),rep("GBM",5)), levels=c("Param","Nnet","GBM"))
GEDUM_voc_par <- GEDUM_voc_par[GEDUM_voc_par$Component!="Total",]
GEDUM_voc_par$Component <- factor(GEDUM_voc_par$Component, levels=c("Baseline","Prevalence","Effect","Selection"))

GEDUM_voc_par_pa <- GEDUM_voc_par_pa[GEDUM_voc_par_pa$Component!="Total",]
GEDUM_voc_par_pa$Component <- factor(GEDUM_voc_par_pa$Component, levels=c("Baseline","Prevalence","Effect","Selection"))


#'############################################'#'############################################'
#'############################################'#'############################################'
#'############################################'#'############################################'
#'############################################'#'############################################'
#'
#



#'-----------------------------------------#
#'-----------------------------------------#
# MAIN ANALYSIS EDUM - TREATMENT ALTERNATIVES  B---------------------------------
#'-----------------------------------------#
#'-----------------------------------------#

# Alternative definitions B- ecec_cb_only_A, fdc_only_A, grandp_relatives_only_A  - 

#'############################################'#'############################################'
#'############################################'#'############################################'
##### Unconditional decomposition:  math5_wle_100 =================================

### ECEC
#
ROBB_math_ecec_pa <- cdgd0_pa(Y="math5_wle_100",D="ecec_cb_only_A",G="eduM_B1",
                              X=covariates, 
                              data=ses5_ececB)
#
set.seed(1)
ROBB_math_ecec_gbm <- cdgd0_ml(Y="math5_wle_100",D="ecec_cb_only_A",G="eduM_B1",
                               X=covariates,
                               data=ses5_ececB,algorithm="gbm")
#
set.seed(1)
ROBB_math_ecec_nnet <- cdgd0_ml(Y="math5_wle_100",D="ecec_cb_only_A",G="eduM_B1",
                                X=covariates,
                                data=ses5_ececB,algorithm="nnet")

GROBB_math_ecec_pa <- as.data.frame(cbind(c("Total","Baseline","Prevalence","Effect","Selection"),ROBB_math_ecec_pa$results))
GROBB_math_ecec_gbm <- as.data.frame(cbind(c("Total","Baseline","Prevalence","Effect","Selection"),ROBB_math_ecec_gbm$results))
GROBB_math_ecec_nnet <- as.data.frame(cbind(c("Total","Baseline","Prevalence","Effect","Selection"),ROBB_math_ecec_nnet$results))
colnames(GROBB_math_ecec_pa)[1:2] <- colnames(GROBB_math_ecec_nnet)[1:2] <- colnames(GROBB_math_ecec_gbm)[1:2] <- c("Component","Estimate")

GROBB_math_ecec <- as.data.frame(rbind(GROBB_math_ecec_pa,GROBB_math_ecec_nnet,GROBB_math_ecec_gbm))
GROBB_math_ecec$Model <- factor(c(rep("Param",5),rep("Nnet",5),rep("GBM",5)), levels=c("Param","Nnet","GBM"))
GROBB_math_ecec <- GROBB_math_ecec[GROBB_math_ecec$Component!="Total",]
GROBB_math_ecec$Component <- factor(GROBB_math_ecec$Component, levels=c("Baseline","Prevalence","Effect","Selection"))

GROBB_math_ecec_pa <- GROBB_math_ecec_pa[GROBB_math_ecec_pa$Component!="Total",]
GROBB_math_ecec_pa$Component <- factor(GROBB_math_ecec_pa$Component, levels=c("Baseline","Prevalence","Effect","Selection"))


### FAMILY DAY CARE
#
ROBB_math_fdc_pa <- cdgd0_pa(Y="math5_wle_100",D="fdc_only_A",G="eduM_B1",
                             X=covariates, 
                             data=ses5_fdcB)
#
set.seed(1)
ROBB_math_fdc_gbm <- cdgd0_ml(Y="math5_wle_100",D="fdc_only_A",G="eduM_B1",
                              X=covariates,
                              data=ses5_fdcB,algorithm="gbm")
#
set.seed(1)
ROBB_math_fdc_nnet <- cdgd0_ml(Y="math5_wle_100",D="fdc_only_A",G="eduM_B1",
                               X=covariates,
                               data=ses5_fdcB,algorithm="nnet")

GROBB_math_fdc_pa <- as.data.frame(cbind(c("Total","Baseline","Prevalence","Effect","Selection"),ROBB_math_fdc_pa$results))
GROBB_math_fdc_gbm <- as.data.frame(cbind(c("Total","Baseline","Prevalence","Effect","Selection"),ROBB_math_fdc_gbm$results))
GROBB_math_fdc_nnet <- as.data.frame(cbind(c("Total","Baseline","Prevalence","Effect","Selection"),ROBB_math_fdc_nnet$results))
colnames(GROBB_math_fdc_pa)[1:2] <- colnames(GROBB_math_fdc_nnet)[1:2] <- colnames(GROBB_math_fdc_gbm)[1:2] <- c("Component","Estimate")

GROBB_math_fdc <- as.data.frame(rbind(GROBB_math_fdc_pa,GROBB_math_fdc_nnet,GROBB_math_fdc_gbm))
GROBB_math_fdc$Model <- factor(c(rep("Param",5),rep("Nnet",5),rep("GBM",5)), levels=c("Param","Nnet","GBM"))
GROBB_math_fdc <- GROBB_math_ecec[GROBB_math_fdc$Component!="Total",]
GROBB_math_fdc$Component <- factor(GROBB_math_fdc$Component, levels=c("Baseline","Prevalence","Effect","Selection"))

GROBB_math_fdc_pa <- GROBB_math_fdc_pa[GROBB_math_fdc_pa$Component!="Total",]
GROBB_math_fdc_pa$Component <- factor(GROBB_math_fdc_pa$Component, levels=c("Baseline","Prevalence","Effect","Selection"))

### GRAN PARENTS - RELATIVES
#
ROBB_math_gran_pa <- cdgd0_pa(Y="math5_wle_100",D="grandp_relatives_only_A",G="eduM_B1",
                              X=covariates, 
                              data=ses5_granB)
#
set.seed(1)
ROBB_math_gran_gbm <- cdgd0_ml(Y="math5_wle_100",D="grandp_relatives_only_A",G="eduM_B1",
                               X=covariates,
                               data=ses5_granB,algorithm="gbm")
#
set.seed(1)
ROBB_math_gran_nnet <- cdgd0_ml(Y="math5_wle_100",D="grandp_relatives_only_A",G="eduM_B1",
                                X=covariates,
                                data=ses5_granB,algorithm="nnet")

GROBB_math_gran_pa <- as.data.frame(cbind(c("Total","Baseline","Prevalence","Effect","Selection"),ROBB_math_gran_pa$results))
GROBB_math_gran_gbm <- as.data.frame(cbind(c("Total","Baseline","Prevalence","Effect","Selection"),ROBB_math_gran_gbm$results))
GROBB_math_gran_nnet <- as.data.frame(cbind(c("Total","Baseline","Prevalence","Effect","Selection"),ROBB_math_gran_nnet$results))
colnames(GROBB_math_gran_pa)[1:2] <- colnames(GROBB_math_gran_nnet)[1:2] <- colnames(GROBB_math_gran_gbm)[1:2] <- c("Component","Estimate")

GROBB_math_gran <- as.data.frame(rbind(GROBB_math_gran_pa,GROBB_math_gran_nnet,GROBB_math_gran_gbm))
GROBB_math_gran$Model <- factor(c(rep("Param",5),rep("Nnet",5),rep("GBM",5)), levels=c("Param","Nnet","GBM"))
GROBB_math_gran <- GROBB_math_gran[GROBB_math_gran$Component!="Total",]
GROBB_math_gran$Component <- factor(GROBB_math_gran$Component, levels=c("Baseline","Prevalence","Effect","Selection"))


GROBB_math_gran_pa <- GROBB_math_gran_pa[GROBB_math_gran_pa$Component!="Total",]
GROBB_math_gran_pa$Component <- factor(GROBB_math_gran_pa$Component, levels=c("Baseline","Prevalence","Effect","Selection"))


### PARENTAL ONLY 
#
ROBB_math_par_pa <- cdgd0_pa(Y="math5_wle_100",D="pa3",G="eduM_B1",
                             X=covariates, 
                             data=ses5_parB)
#
set.seed(1)
ROBB_math_par_gbm <- cdgd0_ml(Y="math5_wle_100",D="pa3",G="eduM_B1",
                              X=covariates,
                              data=ses5_parB,algorithm="gbm")
#
set.seed(1)
ROBB_math_par_nnet <- cdgd0_ml(Y="math5_wle_100",D="pa3",G="eduM_B1",
                               X=covariates,
                               data=ses5_parB,algorithm="nnet")

GROBB_math_par_pa <- as.data.frame(cbind(c("Total","Baseline","Prevalence","Effect","Selection"),ROBB_math_par_pa$results))
GROBB_math_par_gbm <- as.data.frame(cbind(c("Total","Baseline","Prevalence","Effect","Selection"),ROBB_math_par_gbm$results))
GROBB_math_par_nnet <- as.data.frame(cbind(c("Total","Baseline","Prevalence","Effect","Selection"),ROBB_math_par_nnet$results))
colnames(GROBB_math_par_pa)[1:2] <- colnames(GROBB_math_par_nnet)[1:2] <- colnames(GROBB_math_par_gbm)[1:2] <- c("Component","Estimate")

GROBB_math_par <- as.data.frame(rbind(GROBB_math_par_pa,GROBB_math_par_nnet,GROBB_math_par_gbm))
GROBB_math_par$Model <- factor(c(rep("Param",5),rep("Nnet",5),rep("GBM",5)), levels=c("Param","Nnet","GBM"))
GROBB_math_par <- GROBB_math_par[GROBB_math_par$Component!="Total",]
GROBB_math_par$Component <- factor(GROBB_math_par$Component, levels=c("Baseline","Prevalence","Effect","Selection"))

GROBB_math_par_pa <- GROBB_math_par_pa[GROBB_math_par_pa$Component!="Total",]
GROBB_math_par_pa$Component <- factor(GROBB_math_par_pa$Component, levels=c("Baseline","Prevalence","Effect","Selection"))

#'############################################'#'############################################'
#'############################################'#'############################################'
##### Unconditional decomposition:  voc6_sum_100 =================================


### ECEC
#
ROBB_voc_ecec_pa <- cdgd0_pa(Y="voc6_sum_100",D="ecec_cb_only_A",G="eduM_B1",
                             X=covariates, 
                             data=ses5_ececB)
#
set.seed(1)
ROBB_voc_ecec_gbm <- cdgd0_ml(Y="voc6_sum_100",D="ecec_cb_only_A",G="eduM_B1",
                              X=covariates,
                              data=ses5_ececB,algorithm="gbm")
#
set.seed(1)
ROBB_voc_ecec_nnet <- cdgd0_ml(Y="voc6_sum_100",D="ecec_cb_only_A",G="eduM_B1",
                               X=covariates,
                               data=ses5_ececB,algorithm="nnet")

GROBB_voc_ecec_pa <- as.data.frame(cbind(c("Total","Baseline","Prevalence","Effect","Selection"),ROBB_voc_ecec_pa$results))
GROBB_voc_ecec_gbm <- as.data.frame(cbind(c("Total","Baseline","Prevalence","Effect","Selection"),ROBB_voc_ecec_gbm$results))
GROBB_voc_ecec_nnet <- as.data.frame(cbind(c("Total","Baseline","Prevalence","Effect","Selection"),ROBB_voc_ecec_nnet$results))
colnames(GROBB_voc_ecec_pa)[1:2] <- colnames(GROBB_voc_ecec_nnet)[1:2] <- colnames(GROBB_voc_ecec_gbm)[1:2] <- c("Component","Estimate")


GROBB_voc_ecec <- as.data.frame(rbind(GROBB_voc_ecec_pa,GROBB_voc_ecec_nnet,GROBB_voc_ecec_gbm))
GROBB_voc_ecec$Model <- factor(c(rep("Param",5),rep("Nnet",5),rep("GBM",5)), levels=c("Param","Nnet","GBM"))
GROBB_voc_ecec <- GROBB_voc_ecec[GROBB_voc_ecec$Component!="Total",]
GROBB_voc_ecec$Component <- factor(GROBB_voc_ecec$Component, levels=c("Baseline","Prevalence","Effect","Selection"))

GROBB_voc_ecec_pa <- GROBB_voc_ecec_pa[GROBB_voc_ecec_pa$Component!="Total",]
GROBB_voc_ecec_pa$Component <- factor(GROBB_voc_ecec_pa$Component, levels=c("Baseline","Prevalence","Effect","Selection"))


### FAMILY DAY CARE
#
ROBB_voc_fdc_pa <- cdgd0_pa(Y="voc6_sum_100",D="fdc_only_A",G="eduM_B1",
                            X=covariates, 
                            data=ses5_fdcB)
#
set.seed(1)
ROBB_voc_fdc_gbm <- cdgd0_ml(Y="voc6_sum_100",D="fdc_only_A",G="eduM_B1",
                             X=covariates,
                             data=ses5_fdcB,algorithm="gbm")
#
set.seed(1)
ROBB_voc_fdc_nnet <- cdgd0_ml(Y="voc6_sum_100",D="fdc_only_A",G="eduM_B1",
                              X=covariates,
                              data=ses5_fdcB,algorithm="nnet")

GROBB_voc_fdc_pa <- as.data.frame(cbind(c("Total","Baseline","Prevalence","Effect","Selection"),ROBB_voc_fdc_pa$results))
GROBB_voc_fdc_gbm <- as.data.frame(cbind(c("Total","Baseline","Prevalence","Effect","Selection"),ROBB_voc_fdc_gbm$results))
GROBB_voc_fdc_nnet <- as.data.frame(cbind(c("Total","Baseline","Prevalence","Effect","Selection"),ROBB_voc_fdc_nnet$results))
colnames(GROBB_voc_fdc_pa)[1:2] <- colnames(GROBB_voc_fdc_nnet)[1:2] <- colnames(GROBB_voc_fdc_gbm)[1:2] <- c("Component","Estimate")

GROBB_voc_fdc <- as.data.frame(rbind(GROBB_voc_fdc_pa,GROBB_voc_fdc_nnet,GROBB_voc_fdc_gbm))
GROBB_voc_fdc$Model <- factor(c(rep("Param",5),rep("Nnet",5),rep("GBM",5)), levels=c("Param","Nnet","GBM"))
GROBB_voc_fdc <- GROBB_voc_ecec[GROBB_voc_fdc$Component!="Total",]
GROBB_voc_fdc$Component <- factor(GROBB_voc_fdc$Component, levels=c("Baseline","Prevalence","Effect","Selection"))

GROBB_voc_fdc_pa <- GROBB_voc_fdc_pa[GROBB_voc_fdc_pa$Component!="Total",]
GROBB_voc_fdc_pa$Component <- factor(GROBB_voc_fdc_pa$Component, levels=c("Baseline","Prevalence","Effect","Selection"))

### GRAN PARENTS - RELATIVES
#
ROBB_voc_gran_pa <- cdgd0_pa(Y="voc6_sum_100",D="grandp_relatives_only_A",G="eduM_B1",
                             X=covariates, 
                             data=ses5_granB)
#
set.seed(1)
ROBB_voc_gran_gbm <- cdgd0_ml(Y="voc6_sum_100",D="grandp_relatives_only_A",G="eduM_B1",
                              X=covariates,
                              data=ses5_granB,algorithm="gbm")
#
set.seed(1)
ROBB_voc_gran_nnet <- cdgd0_ml(Y="voc6_sum_100",D="grandp_relatives_only_A",G="eduM_B1",
                               X=covariates,
                               data=ses5_granB,algorithm="nnet")

GROBB_voc_gran_pa <- as.data.frame(cbind(c("Total","Baseline","Prevalence","Effect","Selection"),ROBB_voc_gran_pa$results))
GROBB_voc_gran_gbm <- as.data.frame(cbind(c("Total","Baseline","Prevalence","Effect","Selection"),ROBB_voc_gran_gbm$results))
GROBB_voc_gran_nnet <- as.data.frame(cbind(c("Total","Baseline","Prevalence","Effect","Selection"),ROBB_voc_gran_nnet$results))
colnames(GROBB_voc_gran_pa)[1:2] <- colnames(GROBB_voc_gran_nnet)[1:2] <- colnames(GROBB_voc_gran_gbm)[1:2] <- c("Component","Estimate")

GROBB_voc_gran <- as.data.frame(rbind(GROBB_voc_gran_pa,GROBB_voc_gran_nnet,GROBB_voc_gran_gbm))
GROBB_voc_gran$Model <- factor(c(rep("Param",5),rep("Nnet",5),rep("GBM",5)), levels=c("Param","Nnet","GBM"))
GROBB_voc_gran <- GROBB_voc_gran[GROBB_voc_gran$Component!="Total",]
GROBB_voc_gran$Component <- factor(GROBB_voc_gran$Component, levels=c("Baseline","Prevalence","Effect","Selection"))

GROBB_voc_gran_pa <- GROBB_voc_gran_pa[GROBB_voc_gran_pa$Component!="Total",]
GROBB_voc_gran_pa$Component <- factor(GROBB_voc_gran_pa$Component, levels=c("Baseline","Prevalence","Effect","Selection"))


### PARENTAL ONLY 
#
ROBB_voc_par_pa <- cdgd0_pa(Y="voc6_sum_100",D="pa3",G="eduM_B1",
                            X=covariates, 
                            data=ses5_parB)
#
set.seed(1)
ROBB_voc_par_gbm <- cdgd0_ml(Y="voc6_sum_100",D="pa3",G="eduM_B1",
                             X=covariates,
                             data=ses5_parB,algorithm="gbm")
#
set.seed(1)
ROBB_voc_par_nnet <- cdgd0_ml(Y="voc6_sum_100",D="pa3",G="eduM_B1",
                              X=covariates,
                              data=ses5_parB,algorithm="nnet")

GROBB_voc_par_pa <- as.data.frame(cbind(c("Total","Baseline","Prevalence","Effect","Selection"),ROBB_voc_par_pa$results))
GROBB_voc_par_gbm <- as.data.frame(cbind(c("Total","Baseline","Prevalence","Effect","Selection"),ROBB_voc_par_gbm$results))
GROBB_voc_par_nnet <- as.data.frame(cbind(c("Total","Baseline","Prevalence","Effect","Selection"),ROBB_voc_par_nnet$results))
colnames(GROBB_voc_par_pa)[1:2] <- colnames(GROBB_voc_par_nnet)[1:2] <- colnames(GROBB_voc_par_gbm)[1:2] <- c("Component","Estimate")

GROBB_voc_par <- as.data.frame(rbind(GROBB_voc_par_pa,GROBB_voc_par_nnet,GROBB_voc_par_gbm))
GROBB_voc_par$Model <- factor(c(rep("Param",5),rep("Nnet",5),rep("GBM",5)), levels=c("Param","Nnet","GBM"))
GROBB_voc_par <- GROBB_voc_par[GROBB_voc_par$Component!="Total",]
GROBB_voc_par$Component <- factor(GROBB_voc_par$Component, levels=c("Baseline","Prevalence","Effect","Selection"))


GROBB_voc_par_pa <- GROBB_voc_par_pa[GROBB_voc_par_pa$Component!="Total",]
GROBB_voc_par_pa$Component <- factor(GROBB_voc_par_pa$Component, levels=c("Baseline","Prevalence","Effect","Selection"))


#'############################################'#'############################################'
#'############################################'#'############################################'
#'############################################'#'############################################'
#'############################################'#'############################################'


#'-----------------------------------------#
#'-----------------------------------------#
# GRAPHS - JUST PARAMETRIC - MATH VOC TOGETHER  ---------------------------------
#'-----------------------------------------#
#'-----------------------------------------#



# --- 1. Prepare Combined Data ---

# For Math outcomes (parametric only)
GEDUM_math_param <- rbind(
  cbind(Childcare = "Center-based ECEC",    GEDUM_math_ecec_pa),
  cbind(Childcare = "Family Day Care",     GEDUM_math_fdc_pa),
  cbind(Childcare = "Grandparents/Relatives", GEDUM_math_gran_pa),
  cbind(Childcare = "Parental Care Only", GEDUM_math_par_pa)
)
GEDUM_math_param$Component <- factor(GEDUM_math_param$Component, 
                                     levels = c("Baseline", "Prevalence", "Effect", "Selection"))
GEDUM_math_param$outcome <- "Mathematics"

# For Vocabulary outcomes (parametric only)
GEDUM_voc_param <- rbind(
  cbind(Childcare = "Center-based ECEC",    GEDUM_voc_ecec_pa),
  cbind(Childcare = "Family Day Care",     GEDUM_voc_fdc_pa),
  cbind(Childcare = "Grandparents/Relatives", GEDUM_voc_gran_pa),
  cbind(Childcare = "Parental Care Only", GEDUM_voc_par_pa)
)
GEDUM_voc_param$Component <- factor(GEDUM_voc_param$Component, 
                                    levels = c("Baseline", "Prevalence", "Effect", "Selection"))
GEDUM_voc_param$outcome <- "Vocabulary"

# Combine both outcomes
combined_param <- rbind(GEDUM_math_param, GEDUM_voc_param)
combined_param$Childcare <- factor(combined_param$Childcare, 
                                   levels = c("Center-based ECEC", "Family Day Care", "Grandparents/Relatives", "Parental Care Only"))
combined_param$outcome <- factor(combined_param$outcome, 
                                 levels = c("Mathematics", "Vocabulary"))

# --- 2. Create a Data Frame with Total Disparity Values ---
# (These values were removed from the bar data but are needed for the dashed lines and annotations)
total_df <- data.frame(
  Childcare = rep(c("Center-based ECEC", "Family Day Care", "Grandparents/Relatives", "Parental Care Only"), each = 2),
  outcome = rep(c("Mathematics", "Vocabulary"), times = 4),
  total = c( EDUM_math_ecec_pa$results$point[1],  EDUM_voc_ecec_pa$results$point[1],
             EDUM_math_fdc_pa$results$point[1],   EDUM_voc_fdc_pa$results$point[1],
             EDUM_math_gran_pa$results$point[1],    EDUM_voc_gran_pa$results$point[1],
             EDUM_math_par_pa$results$point[1],     EDUM_voc_par_pa$results$point[1])
)

# --- 3. Build the Plot ---
# Use the same colors as before.
outcome_colors <- c("Mathematics" = "#00BFC4", "Vocabulary" = "#F8766D")

plot_para_edum <- ggplot(combined_param, aes(x = Component, y = Estimate, fill = outcome)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  # Color the error bars by outcome (CI colored)
  geom_errorbar(aes(ymin = Estimate - qnorm(0.975)*se, ymax = Estimate + qnorm(0.975)*se),
                width = 0.2, position = position_dodge(width = 0.9), show.legend = FALSE) +
  # Dashed lines for total disparity (colored by outcome)
  geom_hline(data = total_df, aes(yintercept = total, color = outcome), 
             linetype = "dashed", show.legend = FALSE) +
  geom_text(aes(label = round(Estimate, 1)),
            position = position_dodge(width = 0.9),
            vjust = -0.5,
            hjust = -0.08,
            size = 3) +
  # Annotate total disparity values at the right edge of each facet
  geom_text(data = total_df, 
            aes(x = Inf, y = total, 
                label = paste("Total (", outcome, "):", round(total, 1)),
                color = outcome),
            hjust = 1.1, vjust = -0.5, show.legend = FALSE) +
  facet_wrap(~ Childcare, ncol = 2) +
  scale_fill_manual(values = outcome_colors) +
  scale_color_manual(values = outcome_colors) +
  labs(title = "Parametric Decomposition by Childcare",
       x = "Component", y = "Disparity Estimate") +
  theme_minimal() +
  theme(text = element_text(size = 13),
        strip.text = element_text(face = "bold"))

# --- 4. Save the Plot ---
ggsave("PARAMETRIC_EDUM.png", plot = plot_para_edum, path=graphs, 
       dpi = 320, width = 12, height = 10, units = "in")






#'############################################'#'############################################'
#'############################################'#'############################################'
#'############################################'#'############################################'
#'############################################'#'############################################'
#'
#


#'-----------------------------------------#
#'-----------------------------------------#
# GRAPHS - ROBUST DEFINITION TREATMENT  ---------------------------------
#'-----------------------------------------#
#'-----------------------------------------#
#'


# --- 1. Prepare Combined Data ---

# For Math outcomes (parametric only)
GROBB_math_param <- rbind(
  cbind(Childcare = "Center-based ECEC",    GROBB_math_ecec_pa),
  cbind(Childcare = "Family Day Care",     GROBB_math_fdc_pa),
  cbind(Childcare = "Grandparents/Relatives", GROBB_math_gran_pa),
  cbind(Childcare = "Parental Care Only", GROBB_math_par_pa)
)
GROBB_math_param$Component <- factor(GROBB_math_param$Component, 
                                     levels = c("Baseline", "Prevalence", "Effect", "Selection"))
GROBB_math_param$outcome <- "Mathematics"

# For Vocabulary outcomes (parametric only)
GROBB_voc_param <- rbind(
  cbind(Childcare = "Center-based ECEC",    GROBB_voc_ecec_pa),
  cbind(Childcare = "Family Day Care",     GROBB_voc_fdc_pa),
  cbind(Childcare = "Grandparents/Relatives", GROBB_voc_gran_pa),
  cbind(Childcare = "Parental Care Only", GROBB_voc_par_pa)
)
GROBB_voc_param$Component <- factor(GROBB_voc_param$Component, 
                                    levels = c("Baseline", "Prevalence", "Effect", "Selection"))
GROBB_voc_param$outcome <- "Vocabulary"

# Combine both outcomes
combined_param <- rbind(GROBB_math_param, GROBB_voc_param)
combined_param$Childcare <- factor(combined_param$Childcare, 
                                   levels = c("Center-based ECEC", "Family Day Care", "Grandparents/Relatives", "Parental Care Only"))
combined_param$outcome <- factor(combined_param$outcome, 
                                 levels = c("Mathematics", "Vocabulary"))

# --- 2. Create a Data Frame with Total Disparity Values ---
# (These values were removed from the bar data but are needed for the dashed lines and annotations)
total_df <- data.frame(
  Childcare = rep(c("Center-based ECEC", "Family Day Care", "Grandparents/Relatives", "Parental Care Only"), each = 2),
  outcome = rep(c("Mathematics", "Vocabulary"), times = 4),
  total = c( ROBB_math_ecec_pa$results$point[1],  ROBB_voc_ecec_pa$results$point[1],
             ROBB_math_fdc_pa$results$point[1],   ROBB_voc_fdc_pa$results$point[1],
             ROBB_math_gran_pa$results$point[1],    ROBB_voc_gran_pa$results$point[1],
             ROBB_math_par_pa$results$point[1],     ROBB_voc_par_pa$results$point[1])
)

# --- 3. Build the Plot ---
# Use the same colors as before.
outcome_colors <- c("Mathematics" = "#00BFC4", "Vocabulary" = "#F8766D")

plot_robustD <- ggplot(combined_param, aes(x = Component, y = Estimate, fill = outcome)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  # Color the error bars by outcome (CI colored)
  geom_errorbar(aes(ymin = Estimate - qnorm(0.975)*se, ymax = Estimate + qnorm(0.975)*se),
                width = 0.2, position = position_dodge(width = 0.9), show.legend = FALSE) +
  # Dashed lines for total disparity (colored by outcome)
  geom_hline(data = total_df, aes(yintercept = total, color = outcome), 
             linetype = "dashed", show.legend = FALSE) +
  # Annotate total disparity values at the right edge of each facet
  geom_text(data = total_df, 
            aes(x = Inf, y = total, 
                label = paste("Total (", outcome, "):", round(total, 1)),
                color = outcome),
            hjust = 1.1, vjust = -0.5, show.legend = FALSE) +
  facet_wrap(~ Childcare, ncol = 2) +
  scale_fill_manual(values = outcome_colors) +
  scale_color_manual(values = outcome_colors) +
  labs(title = "Parametric Decomposition by Childcare",
       x = "Component", y = "Disparity Estimate") +
  theme_minimal() +
  theme(text = element_text(size = 13),
        strip.text = element_text(face = "bold"))

# --- 4. Save the Plot ---
ggsave("ROBUSTD_EDUM.png", plot = plot_robustD, path=graphs,  
       dpi = 320, width = 12, height = 10, units = "in")



#'############################################'#'############################################'
#'############################################'#'############################################'
#'############################################'#'############################################'
#'############################################'#'############################################'
#'
#


#'-----------------------------------------#
#'-----------------------------------------#
# GRAPHS - ALL METHODS - MAIN ESTIMATION  ---------------------------------
#'-----------------------------------------#
#'-----------------------------------------#
#'


############################################
##  Combine data for Math
############################################

# Label each arrangement for Math
GEDUM_math_ecec$Arrangement <- "Center-based ECEC"
GEDUM_math_fdc$Arrangement  <- "Family Day Care"
GEDUM_math_gran$Arrangement <- "Grandparents/Relatives"
GEDUM_math_par$Arrangement  <- "Parental Care Only"

# Combine into one data frame
math_data <- rbind(GEDUM_math_ecec,
                   GEDUM_math_fdc,
                   GEDUM_math_gran,
                   GEDUM_math_par)

############################################
## Combine data for Voc
############################################

# Label each arrangement for Voc
GEDUM_voc_ecec$Arrangement <- "Center-based ECEC"
GEDUM_voc_fdc$Arrangement  <- "Family Day Care"
GEDUM_voc_gran$Arrangement <- "Grandparents/Relatives"
GEDUM_voc_par$Arrangement  <- "Parental Care Only"

# Combine into one data frame
voc_data <- rbind(GEDUM_voc_ecec,
                  GEDUM_voc_fdc,
                  GEDUM_voc_gran,
                  GEDUM_voc_par)

############################################
##  Plot for Math
############################################

p_math <- ggplot(math_data, 
                 aes(x = Component,
                     y = as.numeric(Estimate),
                     fill = Model)) +
  geom_col(position = position_dodge(width = 0.7),
           width = 0.6) +
  geom_errorbar(aes(ymin = as.numeric(CI_lower),
                    ymax = as.numeric(CI_upper)),
                width = 0.2,
                position = position_dodge(width = 0.7)) +
  facet_wrap(~ Arrangement, ncol = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Decomposition Estimates for Mathematics",
       x = "Decomposition Component",
       y = "Estimate",
       fill = "Model") +
  theme_minimal() +
  theme(text = element_text(size = 12),
        plot.title = element_text(face = "bold", size = 14))

############################################
## Plot for Voc
############################################

p_voc <- ggplot(voc_data, 
                aes(x = Component,
                    y = as.numeric(Estimate),
                    fill = Model)) +
  geom_col(position = position_dodge(width = 0.7),
           width = 0.6) +
  geom_errorbar(aes(ymin = as.numeric(CI_lower),
                    ymax = as.numeric(CI_upper)),
                width = 0.2,
                position = position_dodge(width = 0.7)) +
  facet_wrap(~ Arrangement, ncol = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Decomposition Estimates for Vocabulary",
       x = "Decomposition Component",
       y = "Estimate",
       fill = "Model") +
  theme_minimal() +
  theme(text = element_text(size = 12),
        plot.title = element_text(face = "bold", size = 14))

############################################
##  Print or arrange the final figures
############################################

# Print individually:
print(p_math)
print(p_voc)


# ---  Save the Plot ---
ggsave("ALLMATH_EDUM.png", plot = p_math, path=graphs, 
       dpi = 320, width = 12, height = 10, units = "in")

ggsave("ALLVOC_EDUM.png", plot = p_voc, path=graphs, 
       dpi = 320, width = 12, height = 10, units = "in")


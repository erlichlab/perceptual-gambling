# initialize things

# dataframe manipulation
library(purrr)
library(dplyr)
library(tidyr)
library(tidybayes)
# modelling
library(rstan)
library(loo)
library(modelr)
library(brms)
library(lme4)
library(nimble) 
# plotting
library(ggplot2)
library(bayesplot)
library(patchwork)
library(ggExtra)
# utils
#library(el.db)
library(stringr)
library(latex2exp)
library(jsonlite)

rstan_options(auto_write = TRUE) # cache compiled model
options(mc.cores=parallel::detectCores())  # detect cpu cores

source('utils.R')
source('bdt.R')
source('get_csv.R')
source('figure_method.R')
source('figure_task.R')
source('figure_behavior.R')

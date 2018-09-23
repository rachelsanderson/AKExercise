# AK_main.R ~ Program for performing MCMC on likelihood of AK data. 
# Loads in data from akdata.csv, passes variables into do_mcmc.R function. 
# Variables defined from 2SLS model:
# y = beta*x + eps
# x = gamma*z + nu
# (eps, nu) ~ N(0, Sigma)

#Import relevant libraries
library(bayess)
library(moments) 
library(VARex5132017)
library(coda)
library(fitR)
source("ivlh_faster.R")
source("ivmcmc_faster.R")
source("do_mcmc.R")

#Question 3: One X variable (X=education)
akdataf <- read.csv("../Data/akdata.csv", header = TRUE)
hist(akdataf$logwage)
y = akdataf$logwage
x = akdataf$educ
qob = akdataf$qob
qob.f = factor(qob)
z = model.matrix(~qob.f)[,2:4]
w= rep(1, length(x))

nx=1
nw=1
nz=3

nit = 100000
draws <- do_mcmc(y,x,w,z, nx, nw, nz, nit)
effectiveSize(draws)

#Part 4: Add HighSchool Dummy

hs_dum = as.numeric(akdataf$educ >= 12)
X = cbind(akdataf$educ, hs_dum)
nx=2
hsDraws <- do_mcmc(y,X,w,z, nx, nw, nz, nit)
effectiveSize(hsDraws)



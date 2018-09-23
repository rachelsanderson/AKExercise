# AK_main.R
# Program for performing MCMC on likelihood of AK data

#Import relevant libraries
library(bayess)
library(moments) 
library(VARex5132017)
library(coda)
source("ivlh_faster.R")
source("ivmcmc_faster.R")
source("do_mcmc.R")

#Load in the data, define variables for model:
# y = beta*x + eps
# x = gamma*z + nu
# (eps, nu) ~ N(0, Sigma)
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

nit = 10000
draws <- do_mcmc(y,x,w,z, nx, nw, nz, nit)
effectiveSize(draws)



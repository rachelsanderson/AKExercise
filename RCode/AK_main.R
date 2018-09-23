# AK_main.R
# Program for performing MCMC on likelihood of AK data

#Import relevant libraries
library(bayess)

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



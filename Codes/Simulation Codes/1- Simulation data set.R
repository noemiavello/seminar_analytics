
setwd("C:/Users/Noe/Desktop/IBE Master/3rd Semester/Seminar_gh/Codes/Simulation Codes")

#load packages
library(dplyr)
library(tibble)
library(ggplot2)



#1- runiform(n, 0, 1) -> n observations between 0 and 1
#2- generate the function of p(x) using 0.5 or maybe 0.7 -> plot with the diagonal line and calibration curve
#3- then you can obtain the y 


#Set up
n = 10000
set.seed(123)
x = runif(n)

p <- function(X) x^.5

#defining CEP
cep = p(X=x)

y = rbinom(n,1,cep)  #y using binomial distribution 

#data simulation 
sim_data <- tibble(x,y,cep)     #y:observations, x:forecast values
sim_data

saveRDS(sim_data, "sim_data.rds")

# n = 100000
set.seed(123)
rand_unif <- runif(10000, min = 0, max = 1)
hist(rand_unif, freq = FALSE, xlab = "x", density = 20, col = "darkgray",
     main = "Uniform distribution for the interval [0,1]")




#CEP against forecast probabilities 
  pl <-
    ggplot()+
    geom_line(aes(x=sim_data$x,y=sim_data$cep), color="blue")+
    geom_abline(intercept = 0,slope = 1) + 
    xlab("Forecast probability") +
    ylab("CEP") 
  
  #configuration
  pl <- pl + theme_classic()
  pl


  #calibration curve
  pl+
    geom_line(
      mapping = aes(
        y= isoreg(x=sim_data$x,y=sim_data$y)$yf, #isotonic regression 
                    x =sort(sim_data$x)          
                    ),
      color="red"
    )                              
                                               
      

    
    
# Install and load packages
libraries = c("mvtnorm","doParallel", "nlme", "ggplot2", "ggthemes", "emdi", "reshape2","plyr","quantreg", "animation", "dplyr", "devtools", "data.table", "quantreg", "rmutil", "VGAM", "expectreg", "mboost", "multcomp")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {install.packages(x)})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

source("theme_pub.R")
load_all("PLEASE_SET_WD/mquantreg")
setwd("PLEASE_SET_WD")

#szen = 1 #norm
szen = 2 #chi
#la = 1  #large
la = 2  #small 


H = 30

#larger sample
if(la ==1) {
set.seed(181)  
n_h = round(runif(H, 15,44))
}


#smaller sample
if(la==2) {
set.seed(18)  
n_h = round(runif(H, 5,15))
}
n = sum(n_h)
n
N_h = n_h*10
N_h
N=sum(N_h)
N
#fix mu

if(szen==1) mu=runif(H,min=4,max = 10)
if(szen==2) mu=runif(H,min=8,max = 11)


#begin of simulation
sim.res <- NULL
dt.res <- NULL
res <- NULL
resul <- NULL

q=sort(c(seq(0.006,0.99,0.045),0.5,0.994,0.01,0.02,0.96,0.98))

S=500
B=1
L=400


median_smp <- NULL
mean_smp <- NULL
mean_ind_mq <- NULL
median_ind_mq<- NULL
true_mean<- NULL
true_med<- NULL

#results matrices
true_HC = array( dim=c(H,10,S ))
mqs = array( dim=c(H,10,S ))
MSE = array( dim=c(H,10,S ))

for( s in 1:S){
  
  if(szen == 2){
   gams = rchisq(H,1)
   gams = gams - mean(gams)
   e= rchisq(N,6)
   e = e - mean(e)
   x=NULL
   gam=NULL
   sae=NULL
   for(i in 1:H){
     x_<-rnorm(N_h[[i]],mu[[i]],1)
     x <- c(x,x_)
     gam_ <- rep(gams[[i]],N_h[[i]])
     gam= c(gam,gam_)
     sae_ <- rep(i,N_h[[i]])
     sae=c(sae,sae_)
   }

    pop = data.frame(x=x, gam=gam, e=e, sae=sae)
    pop$y=11-x+gam+e
    
      }
  else if(szen==1){
  gams = rnorm(H,0,200)
  e= rnorm(N,0,800)
  x=NULL
  gam=NULL
  sae=NULL
  for(i in 1:H){
    x_<-rnorm(N_h[[i]],mu[[i]],1)
    x <- c(x,x_)
    gam_ <- rep(gams[[i]],N_h[[i]])
    gam= c(gam,gam_)
    sae_ <- rep(i,N_h[[i]])
    sae=c(sae,sae_)
  }
  
  pop = data.frame(x=x, gam=gam, e=e, sae=sae)
  pop$y=3000-150*x+gam+e
  }
  grouped_data <- pop %>% group_by(sae)
  #threshold <- 0.6*median(pop$y)
  

smp <- as.data.frame(sample_frac(grouped_data, 0.1))
grouped_smp <- smp %>% group_by(sae)
smp_mean <- aggregate(grouped_smp$y,list(as.matrix(grouped_smp$sae)), FUN=mean)
mean_smp =  cbind(mean_smp, smp_mean$x)
smp_median <- aggregate(grouped_smp$y,list(as.matrix(grouped_smp$sae)), FUN=median)
true_est <- direct("y", smp_data = pop, smp_domains = "sae")
mq_est <- mq_sae(y~x, smp_data = smp, pop_data = pop, smp_domains = "sae", pop_domains = "sae",seed=NULL, L=L, B=1, grid=q, MSE=T, S=30, parallel_mode = "socket", cpus=3, smoothed=F)
# pov.hcr<-MQ.SAE.MC(my.ys=smp$y, my.x.s=cbind(1,smp$x), my.x.r=cbind(1,pop$x), 
#                    myregioncode.s=smp$sae, myregioncode.r=pop$sae, L=30, 
#                    method.est="uncond",FUN=mean,z=pov.line)
#mar[,s] <- pov.hcr$STAT.MQ.MC
true_HC[,,s] <- as.matrix(true_est$ind[-1])
mqs[,,s] <- as.matrix(mq_est$ind[-1])
MSE[,,s] <- as.matrix(mq_est$MSE[-1])

print(s)
}


objects   = c("true_HC", "mqs", "MSE")
#save(list = objects, file = "replication_results_normal_small.RData")
#save(list = objects, file = "replication_results_chi_small.RData")






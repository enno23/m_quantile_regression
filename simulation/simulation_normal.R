# Install and load packages
libraries = c("saeSim","devtools", "mvtnorm","doParallel", "nlme", "ggplot2", "ggthemes", "emdi", "reshape2","plyr","quantreg", "animation", "dplyr", "devtools", "data.table", "quantreg", "rmutil", "VGAM", "expectreg", "mboost", "multcomp")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {install.packages(x)})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

source("multiplot.R")
source("theme_pub.R")
source("theme_pub.R")
load_all("PLEASE_SET_WD/mquantreg")
setwd("PLEASE_SET_WD/simulation")




H = 50

q=sort(c(seq(0.006,0.99,0.045),0.5,0.994,0.01,0.02,0.96,0.98))

S=1
B=1
L=50


set.seed(100)
simruns=S
Domains=H
sample_size=c(12, 23, 28, 14, 10, 23, 19, 25, 29, 10, 14, 18, 15, 20, 13, 12, 16, 27, 20, 26, 27, 23, 12, 12, 11, 18, 17, 29, 11, 29, 17, 9, 14, 8, 8, 18, 21, 21, 16, 16, 25, 13, 26, 19, 28, 20, 24, 9, 25, 21)
sample_size=sample_size[1:50]
pop_size=rep(200,Domains)
sum(pop_size)
sum(sample_size)

#==================================================================================================================================#
#-----------------------------------  Scenario 1: Normal Error Term ---------------------------------------------------------------#
#==================================================================================================================================#

gen_XNorm=function(dat,m=dat$muD, s=3){
  dat["x"]=rnorm(nrow(dat), mean = m, sd = s)
  return(dat)
}

gen_myE=function(dat,m=0, s=1000) {
  dat["e"]=rnorm(nrow(dat), mean = m, sd = s)
  dat
}

setup=sim_base(data = base_id(nDomains=Domains, nUnits=pop_size)) %>%
  sim_gen(gen_generic(runif, min = -3, max = 3, groupVars="idD", name = "muD")) %>%
  sim_gen(gen_XNorm)       %>%  
  as.data.frame %>%
  sim_gen(generator=gen_myE)         %>% 
  sim_gen_v(mean=0,sd=500)         %>% 
  sim_resp_eq(y = 4500 - 400*x + v + e)  
formel=formula(y~x)



sampler=function(DAT){
  smp=as.data.frame(matrix(nrow=sum(sample_size) , ncol=ncol(DAT)))
  brd=append(0,cumsum(sample_size))
  for(i in 1:Domains){
    smp[((brd[i]+1):brd[i+1]),]=(DAT[DAT$idD==i,])[sample(1:sum(DAT$idD==i),size=sample_size[i]),]
  }
  attr(smp,"pop")=DAT
  colnames(smp)=colnames(DAT)
  return(smp)
}


#results matrices
true = array( dim=c(H,10,S ))
mqs = array( dim=c(H,10,S ))
ebs = array( dim=c(H,10,S ))
dirs = array( dim=c(H,10,S ))
mqsf = array( dim=c(H,10,S ))

MSE_dir = array( dim=c(H,10,S ))
MSE_eb = array( dim=c(H,10,S ))
MSE_mq = array( dim=c(H,10,S ))


for( s in 1:S){
  pop = lapply(sim(setup,R=1), function(X){ X$y[X$y<0] = 0; return (X) })[[1]]
  smp=NULL
   for(i in 1:Domains){
     smp = rbind(smp,      sample_n(pop[which(pop$idD==i),], sample_size[i] ))
     }
true_est <- direct("y", smp_data = pop, smp_domains = "idD", seed = NULL)
mq_est <- mq_sae(y~x, smp_data = smp, pop_data = pop, smp_domains = "idD", pop_domains = "idD",seed=NULL, L=50, B=50, grid=q, MSE=F, S=50, parallel_mode = "socket", cpus=3, smoothed=F)
#mq_est_f <- mq_sae(y~x, smp_data = smp, pop_data = pop, smp_domains = "idD", pop_domains = "idD",seed=NULL, L=50, B=50, grid=seq(0.001,0.999,0.001), MSE=F, S=50, parallel_mode = "socket", cpus=3, smoothed=F)

eb_est <- emdi::ebp(y~x, smp_data = smp, pop_data = pop, smp_domains = "idD", pop_domains = "idD",seed=NULL, L=50, B=50, MSE=F, parallel_mode = "socket", cpus=3)
dir_est <- direct("y", smp_data = smp, smp_domains = "idD", var=F, seed=NULL)

# pov.hcr<-MQ.SAE.MC(my.ys=smp$y, my.x.s=cbind(1,smp$x), my.x.r=cbind(1,pop$x), 
#                    myregioncode.s=smp$sae, myregioncode.r=pop$sae, L=30, 
#                    method.est="uncond",FUN=mean,z=pov.line)
#mar[,s] <- pov.hcr$STAT.MQ.MC
true[,,s] <- as.matrix(true_est$ind[-1])
mqs[,,s] <- as.matrix(mq_est$ind[-1])
#mqsf[,,s] <- as.matrix(mq_est_f$ind[-1])
#MSE_mq[,,s] <- as.matrix(mq_est$MSE[-1])
ebs[,,s] <- as.matrix(eb_est$ind[-1])
#MSE_eb[,,s] <- as.matrix(eb_est$MSE[-1])
dirs[,,s] <- as.matrix(dir_est$ind[-1])
#MSE_dir[,,s] <- as.matrix(dir_est$MSE[-1])

print(s)
}


objects   = c("true", "mqs", "ebs", "dirs")
#save(list = objects, file = "PLEASE_SET_WD/simulation/normal_point.RData")

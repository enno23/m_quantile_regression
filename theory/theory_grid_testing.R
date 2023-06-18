##################################################################################
#This function calculates the simulation for the grid interval in the theory part#
##################################################################################

require(devtools)
require(ggplot2)
require(ggthemes)
library(reshape2)
library(mquantreg)
#load_all("PLEASE_SET_WD/mquantreg")
theme_set(theme_bw(base_size = 18))
 
set.seed(123)
u <- rnorm(n,0,3)

#############
#HOW FINE SHOUL THE GRID BE? probably enough
#######
quicksim<-function(){
  #begin of simulation
  sim.res <- NULL
  dt.res <- NULL
  res <- NULL
  resul <- NULL

  n <- 50
  j <-30
  dff<-NULL
  lev<-NULL
  for(i in 1:j) {
    #    x1 <- abs(rnorm(n,0,1))
    x1 <- rnorm(n,0,3)
    #   x3 <- rnorm(n,0,1)

    sae <- i
    e<- rnorm(n,0,1)
    y<-  x1+u[[i]]+e
    df_t <- data.frame(y, x1, u[[i]], e, sae)
    dff<-rbind(dff, df_t)
    lev[[i]] <- paste("dom", i)
  }
  dff$sae <- as.factor(dff$sae)
  levels(dff$sae) <-   lev
  return(dff)
  
}



gr <- seq(0.001,0.1, 0.005)
 
sep=NULL
bip=NULL
rbp=NULL
for(s in 1:length(gr)) {
  ac=NULL
  
  for(p in 1:50){
  dff<-quicksim()
  a1 <- mmqm(y ~x1, data=dff, domains="sae", grid=seq(0.001,0.999, gr[s]))
  ac<-cbind(ac, a1$area.coef[,2])
}  
 se = rowMeans((ac-u)^2)
 bi = rowMeans(ac-u)
 rb = rowMeans((ac-u)/u)
 sep = cbind(sep, se)
 bip = cbind(bip, bi)
 rbp = cbind(rbp, rb)
 
}
  
mb = as.matrix(colMeans(rbp))
mbm <- data.frame(Val = mb, Interval= gr,  stat="RelBias")
mse = colMeans(sep)
msem <- data.frame(Val=mse, Interval= gr, stat="MSE")

 
df <- rbind(mbm,msem)
df<-df[which(df$Interval<=0.1),]

# colnames(rbp) = gr
# rbp <- as.data.frame(rbp)  
# rbp$id <- 1:30
# 
# colnames(mse) = gr
# rbp <- as.data.frame(rbp)  
# rbp$id <- 1:30

setwd("PLEASE_SET_WD/")
ggplot(df ,aes(x=Interval, y=abs(Val), colour=stat)) + geom_point(aes(linetype = stat)) +facet_wrap(~ stat, scales = "free") + scale_color_manual(name = "", labels = c("Rel. Bias","MSE"), values=cbPalette) +ylab("") 
ggsave("Plots/grid_test.pdf", width=12, height = 6)



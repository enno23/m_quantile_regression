theme_set(theme_bw(base_size = 18))
require(xtable)
require(emdi)
require(data.table)

require(ggplot2)
require(reshape2)

data("eusilcA_pop")
data("eusilcA_smp")

setwd("PLEASE_SET_WD")
ind_list <- c("Mean", "Median", "Head_Count","Poverty_Gap", "Gini", "Quantile_25")
 emdi_model <- emdi::ebp(fixed = eqIncome ~ gender + eqsize + cash + self_empl +   unempl_ben, pop_data = eusilcA_pop,
                        pop_domains = "district", smp_data = eusilcA_smp, smp_domains = "district" )
coln <-colnames(emdi_model$ind)

load(file = "PLEASE_SET_WD/simulation/normal_mse_500.RData")

##########
#results of mq approach

true_HC<-true
MSE <- sqrt(MSE_mq)

true_mse=  sqrt(apply((true_HC-mqs)^2,c(2), rowMeans))
dim3 <-sweep(MSE,1:2,true_mse,"-")
dim2 <-sweep(dim3,1:2,true_mse,"/")
rb <- apply(dim2, c(2), rowMeans)
rb_mq_ind <- rb

dim3 <-sweep(MSE,1:2,true_mse,"-")
dim3 <-sweep(dim3,1:2,true_mse,"/")
rmse <- sqrt(apply(dim3^2, c(2), rowMeans))
true_rmse_mq <- rmse

########## Add results from ebp
MSE <- sqrt(MSE_eb)

true_mse=  sqrt(apply((true_HC-ebs)^2,c(2), rowMeans))
dim3 <-sweep(MSE,1:2,true_mse,"-")
dim2 <-sweep(dim3,1:2,true_mse,"/")
rb <- apply(dim2, c(2), rowMeans)
rb_eb_ind <- rb

dim3 <-sweep(MSE,1:2,true_mse,"-")
dim3 <-sweep(dim3,1:2,true_mse,"/")
rmse <- sqrt(apply(dim3^2, c(2), rowMeans))
true_rmse_eb <- rmse
###################################################

colnames(true_rmse_mq) <- coln[-1]
colnames(true_rmse_eb) <- coln[-1]
 colnames(rb_mq_ind) <- coln[-1]
colnames(rb_eb_ind) <- coln[-1]
 
true_rmse_mq <- as.data.frame(true_rmse_mq) 
true_rmse_eb<- as.data.frame(true_rmse_eb)
 rb_mq_ind <- as.data.frame(rb_mq_ind) 
rb_eb_ind<- as.data.frame(rb_eb_ind) 
 

Scenario = "normal_mse"
cap=" Normal errors "
  
    #summary table for latex
  sumtab <- function(data, estimator){
    numeric.data= data[,colnames(data)%in%ind_list]
    sum.table <- data.frame(Indicator=NA, 
                            Estimator=estimator, 
                            Min = sapply(numeric.data, FUN = function(x)  min(x, na.rm=T)),
                            Q.25 = sapply(numeric.data, FUN = function(x) quantile(x, na.rm=T)[2]),
                            Median = sapply(numeric.data, FUN = function(x)  median(x, na.rm=T)), 
                            Mean = sapply(numeric.data, FUN = function(x)  mean(x, na.rm=T)),
                            Q.75 = sapply(numeric.data, FUN = function(x) quantile(x, na.rm=T)[4]),
                            Max = sapply(numeric.data, FUN = function(x) max(x, na.rm=T)))
    
    sum.table$Indicator <- rownames(sum.table)
    return(sum.table)
  }
  #set true 0 to na, because of division  
  # print("NA removed:")
  # print(length(true[true==0]))
  # mqs[true==0]<-NA
  # ebs[true==0]<-NA
  # dirs[true==0]<-NA
  # true[true==0]<-NA
  # 
  # true_ind<- apply(true, c(2), FUN = function(x) rowMeans(x, na.rm =T))
  # 
  # mq_ind <- apply(mqs, c(2), FUN = function(x) rowMeans(x, na.rm =T))
  # #mqf_ind <- apply(mqsf, c(2), FUN = function(x) rowMeans(x, na.rm =T))
  # eb_ind <- apply(ebs, c(2), FUN = function(x) rowMeans(x, na.rm =T))
  # dir_ind <- apply(dirs, c(2), FUN = function(x) rowMeans(x, na.rm =T))
  # 
  # rb_mq_ind <- as.data.frame(apply(((mqs-true)/true), c(2), FUN = function(x) rowMeans(x, na.rm =T)))
  # colnames(rb_mq_ind) <- colnames(true_est$ind[,-1])
  # rb_eb_ind <- as.data.frame(apply(((ebs-true)/true), c(2), FUN = function(x) rowMeans(x, na.rm =T)))
  # colnames(rb_eb_ind) <- colnames(true_est$ind[,-1])
  # rb_dir_ind <- as.data.frame(apply(((dirs-true)/true), c(2), FUN = function(x) rowMeans(x, na.rm =T)))
  # colnames(rb_dir_ind) <- colnames(true_est$ind[,-1])
  # 
  
  
  rb_eb_ind$id = 1:50
  rb_eb_ind$estimator = "EBP"

  rb_mq_ind$id = 1:50
  rb_mq_ind$estimator = "MQ (SAE)"
  
  point <- rbind(melt(rb_mq_ind, id=c("id", "estimator")), melt(rb_eb_ind,  id=c("id", "estimator"))) 
                 #melt(rb_dir_ind,  id=c("id", "estimator")))
  
  ggplot(point[which(point$variable %in% ind_list),], aes(x=1, y=value, fill=estimator)) +   geom_boxplot()  + facet_wrap(~variable) + ylab("Rel. Bias")+xlab("")+ labs(fill = "")
  ggsave(paste0("Plots/", Scenario, "_mse.pdf"), width=12, height=6)
  
  
  all_tab <- as.data.table(rbind(sumtab(rb_mq_ind, "MQS"), sumtab(rb_eb_ind, "EBP")))
                        #         sumtab(rb_dir_ind, "Direct")))
  setkey(all_tab, Indicator)
  print(xtable(as.data.frame(all_tab), caption=paste0("Summary statistics for the relative bias of the RMSE estimation in the  ", cap, " scenario")), type = "latex", digits=4, file = paste0("Tables/MSE_estimation_", Scenario, ".tex"), include.rownames = F)
  
  ggplot(data=point[which(point$variable %in% ind_list),], aes(x= id, y=value, group=estimator)) +   geom_line(data=point[which(point$variable %in% ind_list),], aes(color=estimator))+   geom_point(data=point[which(point$variable %in% ind_list),], aes(color=estimator)) + facet_wrap(~variable)+ylab("Rel. Bias")+xlab("")+ theme( legend.title=element_blank())
  ggsave(paste0("Plots/", Scenario, "_MSE_area.pdf"), width=12, height=6)
  
  
  #true_rmse_mqf=  as.data.frame(sqrt(apply((true-mqsf)^2,c(2), FUN = function(x) rowMeans(x, na.rm =T))))
  # true_rmse_mq= as.data.frame(sqrt(apply((true-mqs)^2,c(2), FUN = function(x) rowMeans(x, na.rm =T))))
  # true_rmse_eb= as.data.frame(sqrt(apply((true-ebs)^2,c(2), FUN = function(x) rowMeans(x, na.rm =T))))
  # true_rmse_dir=as.data.frame(sqrt(apply((true-dirs)^2,c(2), FUN = function(x) rowMeans(x, na.rm =T))))
  # colnames(true_rmse_mq) <- colnames(true_est$ind[,-1])
  # colnames(true_rmse_eb) <- colnames(true_est$ind[,-1])
  # colnames(true_rmse_dir) <- colnames(true_est$ind[,-1])
  
  
  true_rmse_eb$id = 1:50
  true_rmse_eb$estimator = "EBP"
   true_rmse_mq$id = 1:50
  true_rmse_mq$estimator = "MQ (SAE)"
  
  true_rmse <- rbind(melt(true_rmse_mq, id=c("id", "estimator")), melt(true_rmse_eb,  id=c("id", "estimator")))
  
  ggplot(data=true_rmse[which(true_rmse$variable %in% ind_list),], aes(x=1, y=value, fill=estimator)) +   geom_boxplot()  + facet_wrap(~variable) +ylab("RRMSE")+xlab("")+ labs(fill = "")
  ggsave(paste0("Plots/", Scenario, "_rrmse.pdf"), width=12, height=6)
  ggplot(data=true_rmse[which(true_rmse$variable %in% ind_list),], aes(x= id, y=value, group=estimator)) +   geom_line(data=true_rmse[which(true_rmse$variable %in% ind_list),], aes(color=estimator))+   geom_point(data=true_rmse[which(true_rmse$variable %in% ind_list),], aes(color=estimator))  + facet_wrap(~variable)+ylab("RRMSE")+xlab("")+ labs(fill = " ") + theme(legend.title=element_blank())
  ggsave(paste0("Plots/", Scenario, "_rrmse_area.pdf"), width=12, height=6)
  
  all_tab <- as.data.table(rbind(sumtab(true_rmse_mq, "MQS"), sumtab(true_rmse_eb, "EBP")))
  setkey(all_tab, Indicator)
  print(xtable(as.data.frame(all_tab), caption=paste0("Summary statistics for the RRMSE of the RMSE estimation results in the ", cap, " scenario")), type = "latex", digits=4, file = paste0("Tables/true_mse_", Scenario, ".tex"), include.rownames = F)



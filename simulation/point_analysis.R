theme_set(theme_bw(base_size = 18))
require(xtable)
require(emdi)
require(ggplot2)

require(reshape2)
require(data.table)

data("eusilcA_pop")
data("eusilcA_smp")

# get colnames
emdi_model <- ebp(fixed = eqIncome ~ gender, pop_data = eusilcA_pop,
                  pop_domains = "district", smp_data = eusilcA_smp, smp_domains = "district", 
                  na.rm = TRUE)

coln<- colnames(emdi_model$ind)

setwd("PLEASE_SET_WD/simulation")

load(file = "PLEASE_SET_WD/simulation/log_point.RData")
plot_table(Scenario = "log", cap="Log-scale Outcomes ")


load(file = "PLEASE_SET_WD/simulation/normal_point.RData")
plot_table(Scenario = "normal", cap="Normal Errors ")



load(file = "PLEASE_SET_WD/simulation/pareto_point.RData")
plot_table(Scenario = "pareto", cap="Pareto Errors ")

load(file = "PLEASE_SET_WD/simulation/cont_point.RData")
plot_table(Scenario = "cont", cap="Contaminated Normal Errors ")


load(file = "PLEASE_SET_WD/simulation/log_point_L.RData")
dirs <- mqs
mqs <- mqs1
ebs <- mqs2

plot_table(Scenario = "log_L", cap="Log_Scale_L")


ind_list <- c("Mean", "Median", "Quantile_25", "Head_Count","Poverty_Gap", "Gini")


rel_ind_list <- c("Mean", "Median", "Quantile_25")
ab_ind_list <- c("Head_Count","Poverty_Gap", "Gini")



plot_table <- function(Scenario = "normal", cap="Ntrunc"){
  
  
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
  print("NA removed:")
    print(length(true[true==0]))
    mqs[true==0]<-NA
   ebs[true==0]<-NA
    dirs[true==0]<-NA
    true[true==0]<-NA

  # true_ind<- apply(true, c(2), FUN = function(x) rowMeans(x, na.rm =T))
  # 
  # mq_ind <- apply(mqs, c(2), FUN = function(x) rowMeans(x, na.rm =T))
  # #mqf_ind <- apply(mqsf, c(2), FUN = function(x) rowMeans(x, na.rm =T))
  # eb_ind <- apply(ebs, c(2), FUN = function(x) rowMeans(x, na.rm =T))
  # dir_ind <- apply(dirs, c(2), FUN = function(x) rowMeans(x, na.rm =T))
  # 
  rb_mq_ind <- as.data.frame(apply(((mqs-true)/true), c(2), FUN = function(x) rowMeans(x, na.rm =T)))
  colnames(rb_mq_ind) <- coln[-1]
  rb_eb_ind <- as.data.frame(apply(((ebs-true)/true), c(2), FUN = function(x) rowMeans(x, na.rm =T)))
  colnames(rb_eb_ind) <-coln[-1]
  rb_dir_ind <- as.data.frame(apply(((dirs-true)/true), c(2), FUN = function(x) rowMeans(x, na.rm =T)))
  colnames(rb_dir_ind) <- coln[-1]
  
  ab_mq_ind <- as.data.frame(apply(((mqs-true)), c(2), FUN = function(x) rowMeans(x, na.rm =T)))
  colnames(ab_mq_ind) <- coln[-1]
  ab_eb_ind <- as.data.frame(apply(((ebs-true)), c(2), FUN = function(x) rowMeans(x, na.rm =T)))
  colnames(ab_eb_ind) <-coln[-1]
  ab_dir_ind <- as.data.frame(apply(((dirs-true)), c(2), FUN = function(x) rowMeans(x, na.rm =T)))
  colnames(ab_dir_ind) <- coln[-1]
  
  
  
  
  rb_eb_ind$id = 1:50
  rb_eb_ind$estimator = "EBP"
  rb_dir_ind$id = 1:50
  rb_dir_ind$estimator = "Direct"
  rb_mq_ind$id = 1:50
  rb_mq_ind$estimator = "MQ (SAE)"
  ab_eb_ind$id = 1:50
  ab_eb_ind$estimator = "EBP"
  ab_dir_ind$id = 1:50
  ab_dir_ind$estimator = "Direct"
  ab_mq_ind$id = 1:50
  ab_mq_ind$estimator = "MQ (SAE)"
  
  
  point <- rbind(melt(rb_mq_ind, id=c("id", "estimator")), melt(rb_eb_ind,  id=c("id", "estimator")), melt(rb_dir_ind,  id=c("id", "estimator")))
  pointa <- rbind(melt(ab_mq_ind, id=c("id", "estimator")), melt(ab_eb_ind,  id=c("id", "estimator")), melt(ab_dir_ind,  id=c("id", "estimator")))
  
  
  ggplot(point[which(point$variable %in% rel_ind_list),], aes(x=1, y=value, fill=estimator)) +   geom_boxplot()  + facet_wrap(~variable) + ylab("Rel. Bias")+xlab("")+ labs(fill = "")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
  ggsave(paste0("../Plots/", Scenario, "_pointr.pdf"), width=12, height=3)
  
  ggplot(pointa[which(pointa$variable %in% ab_ind_list),], aes(x=1, y=value, fill=estimator)) +   geom_boxplot()  + facet_wrap(~variable) + ylab("Bias")+xlab("")+ labs(fill = "")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
  ggsave(paste0("../Plots/", Scenario, "_pointa.pdf"), width=12, height=3)
  
  # relative bias
  r_tab <- as.data.table(rbind(sumtab(rb_mq_ind[,!colnames(rb_mq_ind) %in% ab_ind_list], "MQS"), sumtab(rb_eb_ind[,!colnames(rb_eb_ind) %in% ab_ind_list], "EBP"), sumtab(rb_dir_ind[,!colnames(rb_dir_ind) %in% ab_ind_list], "Direct")))
  #  bias
  a_tab <- as.data.table(rbind(sumtab(ab_mq_ind[,!colnames(ab_mq_ind) %in% rel_ind_list], "MQS"), sumtab(ab_eb_ind[,!colnames(ab_eb_ind) %in% rel_ind_list], "EBP"), sumtab(ab_dir_ind[,!colnames(ab_dir_ind) %in% rel_ind_list], "Direct")))
  setkey(a_tab, Indicator)
  setkey(r_tab, Indicator)
  all_tab <- rbind(r_tab, a_tab)
  print(xtable(as.data.frame(all_tab), caption=paste0("Summary statistics for bias (upper half)/rel. bias (lower half) of direct, EBP and MQ (SAE) point estimation results in the ", cap, " scenario over the areas.")), type = "latex", digits=4, file = paste0("../Tables/point_estimation_", Scenario, ".tex"), include.rownames = F, hline.after = c(-1,0,9,18))
  
  ggplot(data=point[which(point$variable %in% rel_ind_list),], aes(x= id, y=value, group=estimator)) +   geom_line(data=point[which(point$variable %in% rel_ind_list),], aes(color=estimator))+   geom_point(data=point[which(point$variable %in% rel_ind_list),], aes(color=estimator)) + facet_wrap(~variable)+ylab("Rel. Bias")+xlab("")+ theme( legend.title=element_blank())
  ggsave(paste0("../Plots/", Scenario, "_pointr_area.pdf"), width=12, height=3)
  
  ggplot(data=pointa[which(pointa$variable %in% ab_ind_list),], aes(x= id, y=value, group=estimator)) +   geom_line(data=pointa[which(pointa$variable %in% ab_ind_list),], aes(color=estimator))+   geom_point(data=pointa[which(pointa$variable %in% ab_ind_list),], aes(color=estimator)) + facet_wrap(~variable)+ylab("Bias")+xlab("")+ theme( legend.title=element_blank())
  ggsave(paste0("../Plots/", Scenario, "_pointa_area.pdf"), width=12, height=3)
  
  #true_rmse_mqf=  as.data.frame(sqrt(apply((true-mqsf)^2,c(2), FUN = function(x) rowMeans(x, na.rm =T))))
  true_rmse_mq= as.data.frame(sqrt(apply((true-mqs)^2,c(2), FUN = function(x) rowMeans(x, na.rm =T))))
  true_rmse_eb= as.data.frame(sqrt(apply((true-ebs)^2,c(2), FUN = function(x) rowMeans(x, na.rm =T))))
  true_rmse_dir=as.data.frame(sqrt(apply((true-dirs)^2,c(2), FUN = function(x) rowMeans(x, na.rm =T))))
  colnames(true_rmse_mq) <- coln[-1]
  colnames(true_rmse_eb) <- coln[-1]
  colnames(true_rmse_dir) <- coln[-1]
  
  
  true_rmse_eb$id = 1:50
  true_rmse_eb$estimator = "EBP"
  true_rmse_dir$id = c(1:50)
  true_rmse_dir$estimator = "Direct"
  true_rmse_mq$id = 1:50
  true_rmse_mq$estimator = "MQ (SAE)"
  
  true_rmse <- rbind(melt(true_rmse_mq, id=c("id", "estimator")), melt(true_rmse_eb,  id=c("id", "estimator")), melt(true_rmse_dir,  id=c("id", "estimator")))
  
  ggplot(data=true_rmse[which(true_rmse$variable %in% ind_list),], aes(x=1, y=value, fill=estimator)) +   geom_boxplot()  + facet_wrap(~variable, scales="free") +ylab("RMSE")+xlab("")+ labs(fill = " ") +   theme(axis.title.x=element_blank(),
                                                                                                                                                                                                                    axis.text.x=element_blank(),
                                                                                                                                                                                                                    axis.ticks.x=element_blank())
  ggsave(paste0("../Plots/", Scenario, "_true_mse.pdf"), width=12, height=6)
  ggplot(data=true_rmse[which(true_rmse$variable %in% ind_list),], aes(x= id, y=value, group=estimator)) +   geom_line(data=true_rmse[which(true_rmse$variable %in% ind_list),], aes(color=estimator))+   geom_point(data=true_rmse[which(true_rmse$variable %in% ind_list),], aes(color=estimator))  + facet_wrap(~variable, scales = "free")+ylab("RMSE")+xlab("")+ labs(fill = " ") + theme(legend.title=element_blank())
  ggsave(paste0("../Plots/", Scenario, "_true_mse_area.pdf"), width=12, height=6)
  
  all_tab <- as.data.table(rbind(sumtab(true_rmse_mq, "MQS"), sumtab(true_rmse_eb, "EBP"), sumtab(true_rmse_dir, "Direct")))
  setkey(all_tab, Indicator)
  all_tab <-rbind(all_tab[which(Indicator%in%rel_ind_list)],  all_tab[which(Indicator%in%ab_ind_list)])
  
  print(xtable(as.data.frame(all_tab), caption=paste0("Summary statistics for the RMSE of direct, EBP and MQ (SAE) point estimation results in the ", cap, " scenario over the areas.")), type = "latex", digits=4, file = paste0("../Tables/true_mse_", Scenario, ".tex"), include.rownames = F)
}

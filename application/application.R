# Install and load packages
libraries = c("xtable","ggpubr", "sp","doParallel", "colorRamps", "grid", "ggplot2", "ggthemes", "emdi", "reshape2","plyr","quantreg", "animation", "dplyr", "devtools", "data.table", "quantreg", "rmutil", "VGAM", "expectreg", "mboost", "multcomp")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {install.packages(x)})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)
theme_set(theme_bw(base_size = 18))

load_all("PLEASE_SET_WD/mquantreg")
setwd("PLEASE_SET_WD")


pop = eusilcA_pop
smp = eusilcA_smp

q=sort(c(seq(0.006,0.99,0.045),0.5,0.994,0.01,0.02,0.96,0.98))

#### mquantiles of the distribituin
mq<- mquantile(smp$eqIncome, tau = seq(0,1,0.1))
qq<-quantile(smp$eqIncome, probs = seq(0,1,0.1))

qq <- round(rbind(mq,qq),3)
rownames(qq) <- c("M-Quantiles", "Quantiles")

#table of quantiles and co
print(xtable(qq, caption="Quantiles and M-Quantiles of the eusilcA_smp dataset"), type = "latex", file = "Tables/eu_quant.tex",include.rownames = T)
 

#outreg(eusilcA_pop)
library(plyr)
dtf <- sapply(eusilcA_smp[colnames(eusilcA_smp)%in%c("eqIncome",  "eqsize" , "cash" ,  "self_empl" , "unempl_ben" ) ], each( mean, sd,  median, min, max))
dtf <- t(dtf)
print(xtable(dtf, caption="Summary statistics for selected variables of the eusilcA-smp dataset. N=1000."), type = "latex", file = "Tables/eu_summary1.tex",include.rownames = T)

 formula <- eqIncome ~ gender + eqsize + cash +  self_empl + unempl_ben 


mq_reg <- mq(formula, data=eusilcA_smp, t=    c(0.25, 0.5, 0.75 ))

res <- rbind(mq_reg$iterations, mq_reg$coefficients, mq_reg$r_2)
rownames(res)[1]<- "No. of iterations"
rownames(res)[8]<- "Pseudo R^2"

print(xtable(res, caption="Results of M-quantile regression in the sample data."), type = "latex", file = "Tables/reg1.tex",include.rownames = T)


capture.output(mq_reg, file = "out/mq_reg.txt", append = FALSE)


pdf("Plots/mq_plot.pdf")
 plot(mq_reg)
dev.off()

mmqm_out <- mmqm(formula, data=eusilcA_smp, domains = "state" ,  grid= seq(0.01,0.99,0.01))
capture.output(mmqm_out, file = "out/mmqm_out.txt", append = FALSE)


mmqm_out <- mmqm(formula, data=eusilcA_smp, domains = "district" ,  grid= seq(0.01,0.99,0.01))
#capture.output(mmqm_out, file = "out/mmqm_out.txt", append = FALSE)
mmqm_out_plots <- plot(mmqm_out, type="domain", legend=F)
ggarrange(mmqm_out_plots[[1]],mmqm_out_plots[[2]],mmqm_out_plots[[3]],mmqm_out_plots[[4]])
ggsave("Plots/mmqm_plot.pdf", width=8, height=8)

print(xtable(mmqm_out$area.tau[1:20,], caption="Average M-quantile per district in the eusilcA smp dataset (20 districts are shown)"), type = "latex", file = "Tables/area_tau.tex",include.rownames = F)



#mqsae_out <- mq_sae(fixed=formula, smp_data=eusilcA_smp, pop_data=eusilcA_pop, smp_domains = "district" , pop_domains = "district", MSE=T, B=30, S=30, L=30, parallel_mode = "socket", cpus=3)
load("mq_sae_out.RData")
mqsae_out
capture.output(mqsae_out, file = "out/mqsae_out.txt", append = FALSE)
modl="mqs"


#ebp_out <- ebp(fixed=formula, smp_data=eusilcA_smp, pop_data=eusilcA_pop, smp_domains = "district" , pop_domains = "district", MSE=T, B=30,  L=30, parallel_mode = "socket", cpus=3)
#save(ebp_out, file="ebp_out.Rdata")
# # 
# ##EBP results
# load("ebp_out.Rdata")
# mqsae_out <- ebp_out
# modl="ebp"

ggplot(smp, aes(eqIncome)) +   geom_density()
ggsave("Plots/dens_plot.pdf", width=6, height=6)


load("ebp_out.Rdata")
load("mq_sae_out.Rdata")

 
# Load shape file
load_shapeaustria()

# Create mapping table such that variables that indicate domains correspond
# in population data and shape file
mapping_table <- data.frame(unique(eusilcA_pop$district), 
                            unique(shape_austria_dis$NAME_2))

#load custom map plot!!!!!!
#source(map_plot_mod_three_colors.R)
theme_set(theme_bw(base_size = 12))

currStat="Poverty_Gap"
p1 <-map_plot(object = mqsae_out, MSE = F, CV = FALSE, 
         map_obj = shape_austria_dis, indicator = currStat, map_dom_id = "NAME_2", 
         map_tab = mapping_table) +  ggtitle("MQ (SAE)")

currStat="Head_Count"
p2<-map_plot(object = mqsae_out, MSE = F, CV = FALSE, 
         map_obj = shape_austria_dis, indicator = currStat, map_dom_id = "NAME_2", 
         map_tab = mapping_table) + ggtitle("")

currStat="Gini"
p3<-map_plot(object = mqsae_out, MSE = F, CV = FALSE, 
         map_obj = shape_austria_dis, indicator = currStat, map_dom_id = "NAME_2", 
         map_tab = mapping_table) + ggtitle("")


currStat="Poverty_Gap"
pe1 <-map_plot(object = ebp_out, MSE = F, CV = FALSE, 
              map_obj = shape_austria_dis, indicator = currStat, map_dom_id = "NAME_2", 
              map_tab = mapping_table, return_data=T) + ggtitle("EBP")

currStat="Head_Count"
pe2<-map_plot(object = ebp_out, MSE = F, CV = FALSE, 
             map_obj = shape_austria_dis, indicator = currStat, map_dom_id = "NAME_2", 
             map_tab = mapping_table) + ggtitle("")

 
currStat="Gini"
pe3<-map_plot(object = ebp_out, MSE = F, CV = FALSE, 
             map_obj = shape_austria_dis, indicator = currStat, map_dom_id = "NAME_2", 
             map_tab = mapping_table) + ggtitle("")



ggarrange(p1, pe1, p2, pe2, p3,  pe3, ncol=2,nrow=3)

  ggsave("Plots/point_map.pdf", width=12, height=8)
  
  
  currStat="Poverty_Gap"
  m1 <-map_plot(object = mqsae_out, MSE = T, CV = FALSE, 
                map_obj = shape_austria_dis, indicator = currStat, map_dom_id = "NAME_2", 
                map_tab = mapping_table) +  ggtitle("MQ (SAE)")
  
  currStat="Head_Count"
  m2<-map_plot(object = mqsae_out, MSE = T, CV = FALSE, 
               map_obj = shape_austria_dis, indicator = currStat, map_dom_id = "NAME_2", 
               map_tab = mapping_table) + ggtitle("")
  
  
  mqsae_out$MSE$Gini<- ifelse(mqsae_out$MSE$Gini>1,NA, mqsae_out$MSE$Gini)
  
   currStat="Gini"
  m3<-map_plot(object = mqsae_out, MSE = T, CV = FALSE, 
               map_obj = shape_austria_dis, indicator = currStat, map_dom_id = "NAME_2", 
               map_tab = mapping_table) + ggtitle("")
  
  
  currStat="Poverty_Gap"
  me1 <-map_plot(object = ebp_out, MSE = T, CV = FALSE, 
                 map_obj = shape_austria_dis, indicator = currStat, map_dom_id = "NAME_2", 
                 map_tab = mapping_table, return_data=T) + ggtitle("EBP")
  
  currStat="Head_Count"
  me2<-map_plot(object = ebp_out, MSE = T, CV = FALSE, 
                map_obj = shape_austria_dis, indicator = currStat, map_dom_id = "NAME_2", 
                map_tab = mapping_table) + ggtitle("")
  
  currStat="Gini"
  me3<-map_plot(object = ebp_out, MSE = T, CV = FALSE, 
                map_obj = shape_austria_dis, indicator = currStat, map_dom_id = "NAME_2", 
                map_tab = mapping_table) + ggtitle("")
  
  
  ggarrange(m1, me1, m2, me2, m3,  me3, ncol=2,nrow=3)
  
  ggsave("Plots/mse_map.pdf", width=12, height=8)
  
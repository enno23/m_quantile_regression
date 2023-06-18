load("PLEASE_SET_WD/replication_results_normal_small.RData")
# Install and load packages
theme_set(theme_bw(base_size = 18))
require(xtable)
require(emdi)
require(ggplot2)
require(reshape2)

data("eusilcA_pop")
data("eusilcA_smp")

setwd("PLEASE_SET_WD")

source("multiplot.R")
 
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

true_ind<- apply(true_HC, c(2), rowMeans)
# 
# dim3 <-sweep(true_HC,1:2,true_ind,"-")
# mse_var <- apply(dim3^2, c(2), rowMeans)


mean_ind <- apply(mqs, c(2), rowMeans)
mean_rmse <- sqrt(apply(MSE, c(2), rowMeans))
true_rmse=  sqrt(apply((true_HC-mqs)^2,c(2), rowMeans))
true_mse=  (apply((true_HC-mqs)^2,c(2), rowMeans))

colnames(true_ind)<- colnames(true_est$ind)[-1]
colnames(mean_ind)<- colnames(true_est$ind)[-1]
colnames(mean_mse)<- colnames(true_est$ind)[-1]
colnames(true_mse)<- colnames(true_est$ind)[-1]

colMeans(true_rmse)
colMeans(mean_rmse)

dim3 <-sweep(MSE,1:2,true_rmse^2,"-")
dim2 <-sweep(dim3,1:2,true_rmse^2,"/")
rb <- apply(dim2, c(2), rowMeans)

#carefgul: the authors calculated the rmse of the rmse, not of the
# dim3 <-sweep(MSE,1:2,true_mse,"-")
# rmse <- sqrt(apply(dim3^2, c(2), rowMeans))

dim3 <-sweep(sqrt(MSE),1:2,true_rmse,"-")
rmse <- sqrt(apply(dim3^2, c(2), rowMeans))


result_table <- data.frame(V1= c("", "Normal scenario, lamda=1", "True", "Estimated", "Rel. Bias (%)", "RMSE"),
                           V2=c("HCR", "Original", "0.096", "0.096", "0.19", "0.019"),
                           V3= c("HCR", "Replicated", round(mean(true_rmse[,2]),3), round(mean(mean_rmse[,2]),3), round(mean(rb[,2])*100,2), round(mean(rmse[,2]),3)),
                           V4=c("PG", "Original", "0.094", "0.095", "0.26", "0.019"),
                           V5= c("PG", "Replicated", round(mean(true_rmse[,3]),3), round(mean(mean_rmse[,3]),3), round(mean(rb[,3])*100,2), round(mean(rmse[,3]),3))
                           
)
result_table


table.x                 = xtable(result_table, caption = "Comparision of Original and Replicated Results for the MSE of HCR and PG in the Normal Scenario")
print(table.x, type = "latex", digits=3, file = "Tables/replication_table_normal.tex",include.rownames = FALSE, include.colnames = FALSE)

df_gg <- data.frame(Value= true_ind[,2],  Indicator="HCR", Area= 1:30, Type="True" )
df_gg <- rbind(df_gg, data.frame(Value= mean_ind[,2],  Indicator="HCR", Area= 1:30, Type="Est"))
df_gg <- rbind(df_gg, data.frame(Value= mean_ind[,3],  Indicator="PG", Area= 1:30, Type="Est"))
df_gg <- rbind(df_gg, data.frame(Value= true_ind[,3],  Indicator="PG", Area= 1:30, Type="True"))

ggplot(data=df_gg, aes(x= Area, y=Value, group=Type)) +   geom_line(data=df_gg, aes(color=Type))+   geom_point(data=df_gg, aes(color=Type))  + facet_wrap(~Indicator, scales = "free")
ggsave("Plots/replication_point_normal.pdf",  width = 12, height = 6)


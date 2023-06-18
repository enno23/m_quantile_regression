############################################
#Based on Examples presented in the Seminar#
############################################
#data.table, ggplot2 and Hmisc MUST BE INSTALLED FIRST.
lapply(c("data.table", "Hmisc", "ggplot2", "emdi"), function(x) if (!(x %in% installed.packages())) {install.packages(x)})

#please install mquantreg package second------------------------------------------------------------
#please specify directory to the package.
install.packages("PATH_TO_THE_PACKAGE/mquantreg_0.1.0.tar.gz", repos = NULL, type = "source")

#Load package--------------------------------------------------------------------------------------
libraries = c("mquantreg",  "emdi")
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

#Univariate M-quantiles----------------------------------------------------------------------------
?mquantile
x = rnorm(1000)
mquantile(x,tau=c(0.01,0.02,0.05,0.1,0.2,0.5,0.8,0.9,0.95,0.98,0.99))

#M-quantile regression-----------------------------------------------------------------------------
?mq
mq(stack.loss ~ ., stackloss, t=c(0.25,0.5,0.75))

#Mixed M-quantile models---------------------------------------------------------------------------
?mmqm
m1 <- mmqm(weight ~Time, data=ChickWeight, domains="Diet")
m1
plot(m1, type="domain")
plot(m1, type="overall")

#MQ(SAE) Approach----------------------------------------------------------------------------------
?mq_sae
#Load population and sample data
data("eusilcA_pop")
data("eusilcA_smp")

formula <- eqIncome ~ gender + eqsize + cash +  self_empl + unempl_ben
mqemdi_model <- mq_sae(fixed = formula , pop_data = eusilcA_pop, pop_domains = "district", smp_data = eusilcA_smp, smp_domains = "district", L=30)

#print model
mqemdi_model

#print estimators
emdi::estimators(mqemdi_model)

#plot map------------------------------------------------------------------------------------------

# Load shape file
load_shapeaustria()

# Create mapping table such that variables that indicate domains correspond
# in population data and shape file
mapping_table <- data.frame(unique(eusilcA_pop$district),
                            unique(shape_austria_dis$NAME_2))

# Example: Create map plot for mean and Gini - point estimates
emdi::map_plot(object = mqemdi_model, MSE = F, CV = FALSE,
               map_obj = shape_austria_dis, indicator = c("Mean"), map_dom_id = "NAME_2",
               map_tab = mapping_table)
emdi::map_plot(object = mqemdi_model, MSE = F, CV = FALSE,
               map_obj = shape_austria_dis, indicator = c("Head_Count"), map_dom_id = "NAME_2",
               map_tab = mapping_table)

#compare to ebp
ebp_model <- ebp(fixed = formula , pop_data = eusilcA_pop, pop_domains = "district", smp_data = eusilcA_smp, smp_domains = "district", L=30)
emdi::map_plot(object = ebp_model, MSE = F, CV = FALSE,
               map_obj = shape_austria_dis, indicator = c("Mean"), map_dom_id = "NAME_2",
               map_tab = mapping_table)

emdi::map_plot(object = ebp_model, MSE = F, CV = FALSE,
               map_obj = shape_austria_dis, indicator = c("Head_Count"), map_dom_id = "NAME_2",
               map_tab = mapping_table)


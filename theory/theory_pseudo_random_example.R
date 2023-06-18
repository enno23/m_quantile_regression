###################################################################################
#This function calculates the example for pseudo random effects in the theory part#
###################################################################################


# Install and load packages
libraries = c("mquantreg",  "ggplot2", "xtable", "ggthemes", "gridExtra", "reshape2","plyr","quantreg", "animation", "dplyr", "devtools", "data.table", "quantreg", "rmutil", "VGAM", "expectreg", "mboost", "multcomp")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {install.packages(x)})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)
theme_set(theme_bw(base_size = 18))

quicksim<-function(){
  #begin of simulation
  sim.res <- NULL
  dt.res <- NULL
  res <- NULL
  resul <- NULL
  n <- 50
  j <-10
  dff<-NULL
  lev<-NULL
  for(i in 1:j) {
    x <- abs(rnorm(n,0,1))
    #   x3 <- rnorm(n,0,1)
    u <- i
    Domain <- i
    e<- rnorm(n,0,1)
    y<-  x+x*u+ u+e
    df_t <- data.frame(y, x, u, e, Domain)
    dff<-rbind(dff, df_t)
    lev[[i]] <- paste("Domain", i)
  }
  dff$Domain <- as.factor(dff$Domain)
  levels(dff$Domain) <-   lev
  return(dff)
}
dff<-quicksim()

ggplot(dff, aes(x, y, colour=Domain))+ geom_point()
ggsave("Plots/dom_scatter.pdf", width=6, height=4)

m_mod <- mmqm(y~x, domain="Domain", data=dff, grid=seq(0.0001,0.9999,by=0.01))
plot(m_mod, type="domain")
ggsave("Plots/dom_lines.pdf", width=6, height=6)
plot(m_mod, type="overall")
ggsave("Plots/ov_lines.pdf", width=6, height=4)


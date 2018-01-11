######################################################################
#Bayesian Yield-Per-Recruit model
######################################################################
# R code to fit a Beverton-Holt Yield-Per-Recruit model
# Requires YPR_all.stan
#
# To accompany:
# Doll, J.C., T.E. Lauer, and S.Clark-Kolaks. 2017. Yield-per-recurit modeling of two 
# piscivores in a Midwestern reservoir: A Bayesian approach. 
# https://doi.org/10.1016/j.fishres.2017.03.012
#
# Jason Doll
# Code currently under development use at your own risk
######################################################################

require(rstan)
require(shinystan)

then_data<-(read.csv("then_data.csv"))
then_data<-na.omit(then_data[,c(1,4)])
WAE_LW <- read.csv("Monroe_LW.csv")
Monroe_LVB <- read.csv("Monroe_LVB.csv")

Monroe_LVB <- subset(Monroe_LVB,Monroe_LVB$Age>0)

mu<-seq(from=0.05,to=0.75,by=0.01)

minll<-c(203,254,305,356,406,457,508)

dataList = list(
  'obs_m'=log(then_data$M),
  'tmax'=log(then_data$tmax),
  'N_m'=length(then_data$M),
  'maxage' = 10,
  
  'TL_wl'=log((WAE_LW$TL*25.4)),
  'TW_wl'=log((WAE_LW$TW*453.6)),
  'N_wl'=length(WAE_LW$TL),
  
  'TL_vb'=(Monroe_LVB$TL*25.4),
  'age_vb'=Monroe_LVB$Age,
  'N_vb'=length(Monroe_LVB$TL),
  'yearnum'=Monroe_LVB$yrnum,
  'yearN'=max(Monroe_LVB$yrnum),
 
  'nll' = length(minll),
  'minll' = minll,
  'nmu' = length(mu),
  'mu' = mu,
  
  'N0' = 100
)


#Generate reasonable starting values for each model using MLE
mod2 = nls(M ~ alpha * (tmax ^ beta),
           data=then_data,
           start=list(alpha=1,beta=1))
summary(mod2)

#Run a nlm model with frequentist using nonlinear least squares to get good starting values
mod1<-nls((Monroe_LVB$TL*25.4) ~ Linf * (1-exp(-K * (Age - t0))),
          data=Monroe_LVB,
          start=list(Linf=(max(Monroe_LVB$TL)*25.4),K=0.1,t0=0))
lvbstart<-matrix(unlist(coef(mod1)),nrow=1)


mod2<-nls(then_data$M~ alpha * (then_data$tmax^beta),
          data=then_data,
          start=list(alpha=4.899,beta=-0.916))
mstart<-matrix(unlist(coef(mod2)),nrow=1)


waetl=(WAE_LW$TL*25.4)
waetw=(WAE_LW$TW*453.6)
lwmod<-nls(waetw ~ alpha * waetl^beta,
           start=list(alpha=1,beta=1),control=nls.control(maxiter=100))
alpha<-coef(lwmod)
params<-as.vector(unlist(coef(lwmod)))

nchains=3

initsList <- lapply(1:nchains,function(i) {
  list(
    #age_vb_est_log=log(Monroe_LVB$Age),  #initialize mean at observed values
    log_alpha_m = log(mstart[,1]),
    beta_m =mstart[,2],
    sdm = runif(1,1,1),
    t0_sd = runif(1,1,5),
    k_sd = runif(1,1,5),
    linf_sd = runif(1,1,5),
    vbsd = runif(1,1,5),
    sdwt = runif(1,1,5),
    Linf_1 = log(rep(lvbstart[,1],8)),
    k_1 = log(rep(lvbstart[,2],8)),
    t0_1 = log(rep(lvbstart[,3],8)+10),
    mu_linf = log(lvbstart[,1]),
    mu_k = log(lvbstart[,2]),
    mu_t0 = log(lvbstart[,3]+10),
    log_alpha=log(params[1]),
    Linf_1_raw=rep(0,8),
    k_1_raw=rep(0,8),
    t0_1_raw=rep(0,8),
    beta=params[2],
    agesd=runif(1,1,2)
    
  )
})



#send everything to Stan
post_ypr <- stan(file = 'YPR_all.stan',
                 data = dataList , 
                 init = initsList,
                 chains = nchains,
                 iter = 2000 , 
                 warmup = 1000 , 
                 cores = nchains,
                 thin = 1,control = list(adapt_delta = 0.99,max_treedepth=15))


#Exploring the output
options(max.print=999999)
names(post_ypr)
options(max.print=1000)

options(max.print=999999)
summary(post_ypr, pars=c("Winf","glbtlinf","glbtk","glbtt0","btLinf","estM","log_alpha_m","beta_m"))$summary
options(max.print=1000)

options(max.print=999999)
summary(post_ypr, pars=c("age_vb_est_log"))$summary
options(max.print=1000)

yieldsum=summary(post_ypr, pars=c("Y"))$summary


launch_shinystan(post_ypr)

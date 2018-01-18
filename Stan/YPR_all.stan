/*Bayesian Yield-Per-Recruit model
---------------------------------------------------------------------------------------
Stan code to fit a Beverton-Holt Yield-Per-Recruit model

To accompany:
Doll, J.C., T.E. Lauer, and S.Clark-Kolaks. 2017. Yield-per-recurit modeling of two 
piscivores in a Midwestern reservoir: A Bayesian approach. 
https://doi.org/10.1016/j.fishres.2017.03.012

Jason Doll
Code currently under development use at your own risk
---------------------------------------------------------------------------------------
*/
  
data {
  //data for estimating parameters for natural mortality following Then et al.
  int<lower=0> N_m;  //number of observations for natural mortality model
  real obs_m[N_m];    //natural mortality 
  real tmax[N_m];     //tmax from Then data
  real maxage;        //maximum age used for predicting natural mortality
    
  //weight/length model data - assumes complete pooling if using multiple years or year classes
  int<lower=0> N_wl; //number of observations for length weight model
  real TL_wl[N_wl];   //observed weight of individuals
  real TW_wl[N_wl];   //observed length of indivituals
  
  //von Bertalanffy growth model data
  int<lower=0> N_vb;  //number of observations for von Bertalanffy growth model
  real TL_vb[N_vb];    //Total length of each individual
  int age_vb[N_vb];   //age of each individual 
  int yearnum[N_vb];  //year class or year identifier for hierarchical model, must be continuous
  int<lower=0> yearN; //number of years or year classes

  int<lower=0> nll;
  real minll[nll];
  int<lower=0> nmu;
  real mu[nmu];
  
  int<lower=0> N0;   //initial number of individuals for yield-per-recruit model
  }

parameters {
  //parameters for natural mortality model
  real<lower=0> sdm;  
  real log_alpha_m;
  real beta_m;
  
  //parameters for weight-length model
  real<lower=0> sdwt;   //sd for weight-length model
  real log_alpha;
  real beta;
  
  //parameters for von Bertlanffy growth model
  //real<lower=0> agesd; 
  real<lower=0>vbsd; 
  // real mu_linf; 
  // real mu_k; 
  // real mu_t0;

  real Linf_1_raw[yearN];
  real k_1_raw[yearN];   
  real t0_1_raw[yearN];
  
  //real<lower=0>linf_sd;
  //real<lower=0>k_sd;
  //real<lower=0> t0_sd;

  //real age_vb_est_log[N_vb];
  vector[3] parammean;
  matrix[yearN,3] mulvb;
  cholesky_factor_corr[3] L_Omega;
  vector<lower=0>[3] sigma; 
    
}

transformed parameters{
  real Linf[yearN];
  real k[yearN];   
  real t0[yearN];
  
  //real Linf_1[yearN];
  //real k_1[yearN];   
  //real t0_1[yearN];
  
  //non-centered hierarchical prior
  for (j in 1:yearN){
    // Linf_1[j] = mulvb[1] + Linf_1_raw[j] * linf_sd;
    // k_1[j] = mulvb[2] + k_1_raw[j] * k_sd;
    // t0_1[j] = mulvb[3] + t0_1_raw[j] * t0_sd;
    
    Linf[j] = exp(mulvb[j,1]);
    k[j] = exp(mulvb[j,2]);
    t0[j] = exp(mulvb[j,3]) - 10;
  }

}

model {
  real eta_wl[N_wl];  //vector of mean weight
  real mu_m[N_m];     //vector of natural mortality
  real eta_vb[N_vb];
 
  //prior for natural mortality model
  sdm ~ cauchy(0,5);  
  log_alpha_m ~ normal(0, 10);
  beta_m ~ normal(0, 10);
  
  //priors for weight length model
  sdwt ~ cauchy(0,5);    
  beta ~ normal(0, 10);
  log_alpha ~ normal(0, 10);

  //priors for von Bertlanffy model
  //agesd ~ cauchy(0,5); //SD when estimating uncertainty in the aging process
  vbsd ~ cauchy(0,5); 
  //linf_sd ~ cauchy(0,5);
  //k_sd ~ cauchy(0,5);
  //t0_sd ~ cauchy(0,5);
  
  //Hyperprior means are drawn from multivariate normal distribuiton
  //mu_linf ~ uniform(0,10);
  //mu_k ~ normal(0,10); 
  //mu_t0 ~ uniform(-3,3); 
  
  parammean[1] ~ uniform(0,10); //hyperprior for Linf
  parammean[2] ~ normal(0,10);  //hyperprior for k
  parammean[3] ~ uniform(-3,3); //hyperprior for t0
  
  L_Omega ~ lkj_corr_cholesky(1);
  sigma ~ cauchy(0, 2.5); // prior on the standard deviations
  
  
  //Currenlty generating divergent transitions - look into non-entered version
  for (r in 1:yearN){
    mulvb[r,1:3] ~ multi_normal_cholesky(parammean[1:3],diag_pre_multiply(sigma, L_Omega));
  }
  
  //Likelihooh for Natural Mortality models
  for (i in 1:N_m) {  
    //mu_m is on the log scale
    mu_m[i]=log_alpha_m + tmax[i] * beta_m;
    obs_m[i] ~ normal(mu_m[i],sdm) ;
  }

  //likelihood for weight/length model
  for (i in 1:N_wl) {
    //TW_wl is on the log scale
    eta_wl[i] = log_alpha + TL_wl[i] * beta;
    TW_wl[i] ~ normal(eta_wl[i], sdwt);
  }

  for (j in 1:yearN){
    Linf_1_raw[j] ~ normal(0,1) ; // implies Linf_1[j] ~ normal(mu_linf,linf_sd) ;
    k_1_raw[j] ~ normal(0,1)  ;   // implies k_1[j] ~ normal(mu_k,k_sd)  ;   
    t0_1_raw[j] ~ normal(0,1) ;   // implies t0_1[j] ~ normal(mu_t0,t0_sd) ;
  }

  //likelihood for von Bertalanfgy growth model
  for (i in 1:N_vb) {
    //uncertainty in observed age age_vb
    //Original model incorporated unceratinty into the age - however, Stan is not converging.
    //Results are valid without it - however must assume aging was done without error
    //age_vb_est_log[i] ~ uniform(0,5);
    //age_vb[i] ~ lognormal(age_vb_est_log[i], agesd); //log-normal error for age observations (Hatch and Jiao 2016)
    //von Bertlanffy growth
    eta_vb[i] =Linf[yearnum[i]] * (1-exp(-k[yearnum[i]] * (age_vb[i] - t0[yearnum[i]])));
    TL_vb[i] ~ normal(eta_vb[i], vbsd);
  }
}

generated quantities {
  real glbtlinf; 
  real glbtk; 
  real glbtt0;
  real btLinf[yearN];
  real btk[yearN];   
  real btt0[yearN];
  real Winf;
  real estM;
  real Q;
  real Fmort[nll,nmu];
  real Z[nll,nmu];
  real S[nll,nmu];
  real tr[nll,nmu];
  real r[nll,nmu] ;
  real Nt[nll,nmu] ;
  real Y[nll,nmu] ;
  
  
  glbtlinf=exp(parammean[1]);
  glbtk=exp(parammean[2]);
  glbtt0=exp(parammean[3]) - 10;
    
  for (j in 1:yearN){
    btLinf[j] = exp(mulvb[j,1]);
    btk[j] = exp(mulvb[j,2])   ;
    btt0[j] = exp(mulvb[j,3]) - 10;
  }
  
  //estimated natural mortality based on maximum age
  estM = exp(normal_rng(log_alpha_m + log(maxage) * beta_m,sdm)) ;
  
  //maximum theoretical weight derived from L-inf and weight to length regression
  Winf =  exp(log_alpha + log(glbtlinf) * beta);

  //slope of the weight-length relation + 1
  //distribution comes from LW Bayes model
  Q = beta + 1;

  for (j in 1:nll){
    for (z in 1:nmu){    
      Fmort[j,z]=-log(-mu[z]+1);
      Z[j,z]=Fmort[j,z]+estM;
      S[j,z]=exp(-Z[j,z]);

      //time in years to recruit to the fishery (tr - to)
      if(glbtlinf > minll[j]){
         tr[j,z] = (log(1-minll[j]/glbtlinf))/-glbtk + glbtt0;
       }
       else{
         tr[j,z] = (log(1-minll[j]/(minll[j]+0.1)))/-glbtk + glbtt0;
       }

      r[j,z] = tr[j,z] - glbtt0;

      //Number of recruits entering the fishery at some minimum length at time (t):
      Nt[j,z] = N0 * exp(-estM * tr[j,z]);
    
      if(Nt[j,z]<0){
        Nt[j,z]=0;
      }
      if(Nt[j,z]>N0){
        Nt[j,z]=N0;
      }
         
      Y[j,z] = ((Fmort[j,z]*Nt[j,z]*exp(Z[j,z]*r[j,z]) * Winf) / glbtk) *
        (beta_cdf(exp(-glbtk*r[j,z]),Z[j,z]/glbtk,Q) * exp(lbeta(Z[j,z]/glbtk,Q))) -
        (beta_cdf(exp(-glbtk*(maxage-glbtt0)),Z[j,z]/glbtk,Q) * exp(lbeta(Z[j,z]/glbtk,Q)));
         
      //Yield can't be less than 0 so change to 0
      if(Y[j,z]<0){Y[j,z]=0;}
         
      }
    }
}


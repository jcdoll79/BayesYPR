/*Bayesian Yield-Per-Recruit model
---------------------------------------------------------------------------------------
Stan code to fit a Beverton-Holt Yield-Per-Recruit model
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
  int maxagevb;
  int yearnum[N_vb];  //year class or year identifier for hierarchical model, must be continuous
  int<lower=0> yearN; //number of years or year classes

  int<lower=0> nll;
  real minll[nll];
  int<lower=0> nmu;
  real mu[nmu];
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
  real<lower=0> agesd[maxagevb]; 
  real<lower=0>vbsd; 
  real mu_linf; 
  real mu_k; 
  real mu_t0;

  real Linf_1_raw[yearN];
  real k_1_raw[yearN];   
  real t0_1_raw[yearN];
  
  real<lower=0>linf_sd;
  real<lower=0>k_sd;
  real<lower=0> t0_sd;

   real age_vb_est_log[N_vb];
}

transformed parameters{
  real Linf[yearN];
  real k[yearN];   
  real t0[yearN];
  
  real Linf_1[yearN];
  real k_1[yearN];   
  real t0_1[yearN];
  
  //non-centered hierarchical prior
  for (j in 1:yearN){
    Linf_1[j] = mu_linf + Linf_1_raw[j] * linf_sd;
    k_1[j] = mu_k + k_1_raw[j] * k_sd;
    t0_1[j] = mu_t0 + t0_1_raw[j] * t0_sd;
    
    Linf[j] = exp(Linf_1[j]);
    k[j] = exp(k_1[j]);
    t0[j] = exp(t0_1[j]) - 10;
  }

}

model {
  real eta_wl[N_wl];  //vector of mean weight
  real mu_m[N_m];  //vector of natural mortality
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
   agesd ~ cauchy(0,5); 
  vbsd ~ cauchy(0,5); 
  mu_linf ~ uniform(0,10);
  mu_k ~ normal(0,10); 
  mu_t0 ~ uniform(-3,3); 
  linf_sd ~ cauchy(0,5);
  k_sd ~ cauchy(0,5);
  t0_sd ~ cauchy(0,5);
  
  //Likelihooh for Natural Mortality models
  for (i in 1:N_m) {  
    //mu_m[i]=alpha_m * (tmax[i] ^ beta_m);
    //mu_m is on the log scale
    mu_m[i]=log_alpha_m + tmax[i] * beta_m;
    obs_m[i] ~ normal(mu_m[i],sdm) ;
  }

  //likelihood for weight/length model
  for (i in 1:N_wl) {
    //eta_wl[i] = alpha * TL_wl[i]^(beta);
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
     age_vb_est_log[i] ~ normal(0,10);
     age_vb[i] ~ lognormal(age_vb_est_log[i], agesd[age_vb[i]]); //log-normal error for age observations (Hatch and Jiao 2016)
    //von Bertlanffy growth
    eta_vb[i] =Linf[yearnum[i]] * (1-exp(-k[yearnum[i]] * (exp(age_vb_est_log[i]) - t0[yearnum[i]])));
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
  
  
    glbtlinf=exp(mu_linf);
    glbtk=exp(mu_k);
    glbtt0=exp(mu_t0) - 10;
    
  for (j in 1:yearN){
    btLinf[j] = exp(Linf_1[j]);
    btk[j] = exp(k_1[j])   ;
    btt0[j] = exp(t0_1[j]) - 10;
  }
  

  estM = exp(normal_rng(log_alpha_m + log(10) * beta_m,sdm)) ;
  
//time for fish to recruit to a minimum length limit
//maximum theoretical weight derived from L-inf and weight to length regression

Winf =  exp(log_alpha + log(glbtlinf) * beta);


//maxage = 10  #maximum age for monroe walleye

//slope of the weight-length relation + 1
//distribution comes from LW Bayes model
Q = beta + 1;

for (j in 1:nll){
  for (z in 1:nmu){    
    //TL = minll[j];
    //u = mu[z] ; //u is cf

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
    Nt[j,z] = 100 * exp(-estM * tr[j,z]);
    
    if(Nt[j,z]<0){
      Nt[j,z]=0;
    }
    if(Nt[j,z]>100){
      Nt[j,z]=100;
    }

         
    Y[j,z] = ((Fmort[j,z]*Nt[j,z]*exp(Z[j,z]*r[j,z]) * Winf) / glbtk) *
       (beta_cdf(exp(-glbtk*r[j,z]),Z[j,z]/glbtk,Q) * exp(lbeta(Z[j,z]/glbtk,Q))) -
         (beta_cdf(exp(-glbtk*(10-glbtt0)),Z[j,z]/glbtk,Q) * exp(lbeta(Z[j,z]/glbtk,Q)));
         
    //can't be less than 0 so change to 0
    if(Y[j,z]<0){Y[j,z]=0;}
         
  }
}
  
}


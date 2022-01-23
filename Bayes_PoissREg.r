library(rstanarm)
mod_bglm <- stan_glm(y ~ lontani+vicini+vicini50+entrambiStud+sitSentim+conviventi+as.numeric(sms)+ soddisfazione + figli,data = dataset,
                           family = neg_binomial_2,seed=99)

summary(mod_bglm)
coef(mod_bglm)
(Intercept)              lontani               vicini             vicini50 
-0.51639013           0.38055644           0.30468455           0.51412542 
entrambiStud sitSentimFidanzato/a   sitSentimSposato/a           conviventi 
-0.27500465           0.98734521           2.06085791          -0.43324398 
as.numeric(sms)        soddisfazione                figli 
0.05268747           0.10300200          -0.32522368 

AIC(mod_bglm); BIC(mod_bglm)

round(exp(coef(mod_bglm))-1, 4)
(Intercept)              lontani               vicini             vicini50 
-0.4033               0.4631               0.3562               0.6722 
entrambiStud     sitSentimFidanzato/a   sitSentimSposato/a   conviventi 
-0.2404               1.6841               6.8527              -0.3516 
as.numeric(sms)        soddisfazione                figli 
0.0541               0.1085              -0.2776 



#lontani+vicini+vicini50+entrambiStud+sitSentim+conviventi+as.numeric(sms)+ soddisfazione + figli

library(bayesplot)
library(ggplot2)
ratios_cp <- neff_ratio(mod_bglm)
mcmc_neff(ratios_cp) + yaxis_text(hjust = 1)

stan_trace(mod_bglm, pars=c("(Intercept)","lontani",
                                   "vicini","vicini50"))

stan_trace(mod_bglm, pars=c("entrambiStud", "sitSentimFidanzato/a",
                            "sitSentimSposato/a", "conviventi"))
stan_trace(mod_bglm, pars=c("as.numeric(sms) ", "soddisfazione", "figli"))

fit<- mod_bglm
posterior <- as.matrix(fit)

plot_title <- ggtitle("Posterior distributions",
                      "with medians and 95% intervals")
mcmc_areas(posterior,
           pars=c("(Intercept)","lontani",
                  "vicini","vicini50", "entrambiStud"),
           prob = 0.95) + plot_title

mcmc_areas(posterior,
           pars=c("sitSentimFidanzato/a",
                  "sitSentimSposato/a", "conviventi", "as.numeric(sms)", "soddisfazione", "figli"),
           prob = 0.95) + plot_title

ss

library(rstan)
#stan_hist(mod_bglm, pars=c("entrambiStud"),bins=40)


post.samps.progGen<- as.data.frame(mod_bglm,
                                   pars=c("y"))[,"y"]
m.progGen <- mean(post.samps.progGen) # posterior mean

m.progGen


ci.progGen <- quantile(post.samps.progGen, probs=c(0.05, 0.95))

ci.progGen
#Graphical posterior predictive checks (A)

color_scheme_set("blue")
pp_check(mod_bglm)


#Graphical posterior predictive checks (B)
pp_check(mod_bglm, plotfun = "hist", nreps = 5)


#Graphical posterior predictive checks (C)
pp_check(mod_bglm, plotfun = "stat", stat = "mean")






#prior vs posterior

prior_summary(mod_bglm)#we obtain the prior distibutions and hyperparameters
posterior_vs_prior(mod_bglm, group_by_parameter= TRUE,
                   pars=c("lontani",
                                   "vicini","vicini50", "entrambiStud", "sitSentimFidanzato/a",
                          "sitSentimSposato/a", "conviventi", "sms", "soddisfazione", "figli"))


summary(mod_bglm)
round(coef(mod_bglm), 3)
summary(modfullnb5)
modzeroinfpoisson
#PSEUDO R SQUARE
require(DescTools)
PseudoR2(modfullnb5, which = "all")
McFadden     McFaddenAdj        CoxSnell      Nagelkerke   AldrichNelson VeallZimmermann 
2.024164e-02    1.265872e-02    1.056398e-01    1.060666e-01    1.004336e-01    1.186423e-01 
Efron             McKelveyZavoina            Tjur             AIC             BIC          logLik 
7.416494e-02              NA              NA    2.866530e+03    2.917714e+03   -1.421265e+03 
logLik0              G2 
-1.450628e+03    5.872618e+01 


residualPlot(modfullnb5)
residualPlots(modfullnb5)

1 - pchisq(summary(modfullnb5)$deviance,summary(modfullnb5)$df.residual)
#0.006299 

anova.negbin(modfullnb5)


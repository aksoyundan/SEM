################################################################
############### Computer Practical 1 ###########################
###### Structural Equation Modeling and Causal Inference########
########################################## 06Nov2023, Ozan Aksoy


################################################################
# Q1: 
################################################################

# a: see slides set-1

# b, c:

bd_low <- '
1.0000
0.5160  1.0000
0.4530  0.4380  1.0000
0.3320  0.4170  0.5380  1.0000
0.3220  0.4050  0.5960  0.5410  1.0000'
bd.corr <- getCov(bd_low, 
                  names = c("faed", "faoc",
                            "educ", "occ1", "occ2"))

m1 <- 'educ ~  a*faed + b*faoc 
       occ1 ~  c*faoc + d*educ       
       occ2 ~  e*faoc + f*educ + g*occ1
       ind1 := b*d*g          #via educ and occ1
       ind2 := b*f            #via educ
       ind3 := c*g            #via occ1
       indt := ind1+ind2+ind3 #total indirect
       tota := indt + e       #total effect
      '
#note lavaan can directly fit a model using covariance matrix
fit.bd <- sem(m1, sample.cov = bd.corr, 
              sample.nobs = 20700)
summary(fit.bd)

# I can see three indirect effects of faoc on occ2:
# via educ, via occ1, and via educ + occ1. These 
# are included in the model syntax above.
# A unit increase in father's occupational status
# increases the son's second occupation's status
# by 0.115 units (coeff e) directly, controlling
# for educ and occ1. An increase in father's
# occupational status, however, also increases
# education and the status of sons 1st occupation.
# Hence, a unit increase in faed results in 0.207
# units increase in son's 2nd occupational status
# through edu and first occupation. In fact, we 
# can even break down the indirect paths further:
# 0.11 through education only, 0.063 through occ1,
# and 0.034 through edu + occ1. 

################################################################

# d) All direct and indirect paths are statistically 
# significant with the delta method. Now let's try
# bootstrapping. Bootstrapping uses the sample as the 
# population. It draws a random sub-sample from the 
# sample we have and calculates the parameter of interest using 
# this subsample. This process is repeated thousands of
# times. Roughly, the proportion of times the parameter of 
# interest is calculated to be zero or negative in the 
# subsamples is used as a p-value of the test of
# the parameter of interest. For bootstrapping, we need
# raw data which we don't have. So we'll simulate raw
# data using the correlation matrix above, assuming multivariate
# normality (btw, the SEM model we fit assumes MVN anyways, so
# it is not a wilder assumption than what we routinely make.)
# We'll use the mvrnorm() function to simulate 20700 data points.
# We'll also assume than means are zero, hence all variables are
# mean centered (because we don't have variable means, but this
# is no problem as the means do not affect the regresison 
# coefficients, they affect intercepts). Try ?mvrnorm() to see
# the function in detail.

dat1 <- semTools::kd(bd.corr, 20700, type = "exact")

#check:
head(dat1)

# OK, we have the raw data. Let's fit the model using raw data.
# We won't need to retype the model as it is above, we'll only
# change the data. Before bootstrapping, I would like to make
# sure that with the simulated data we get exactly the same
# results as we did with the correlation matris:

fit.bd2 <- sem(m1, data = dat1)
summary(fit.bd2)

# Convince yourself that the results of fit.bd2 and fit.bd are
# the same.

# Now bootstrapping: (beware this will take some time to run)

fit.bd3 <- sem(m1, data = dat1, se = "bootstrap")
summary(fit.bd3)

# The standard errors change somwehat, but the results of 
# statistical tests of indirect effects remain virtually
# identical with bootstrapping. All indirect effects and
# total effect remains highly statistically significant.

################################################################

# e:

summary(fit.bd, fit.measures = TRUE)

# Model chi-sq (2) = 18.119 which is statistically 
# significant. This means that the exact fit hypothesis is
# rejected. This should normally flag up issues about model fit.
# The RMSEA is 0.020 a small number which indicates fine fit.
# The close fit hypothesis (RMSEA < 0.05) cannot be rejected,
# hence although the fit of our model is not exact, it is
# "close". CFI is rather high so our model fits much better 
# than the baseline model. Also, SRMR is rather low with 
# 0.005 which also indicates good fit. Overall, apart from
# model chi-square, all other indicates point to good fit. 
# Note that model chi-square is sensitive to N. We have a
# huge N (20700) which might be why our model fit is not
# exact. Let's look at correlation residuals. I'll look at 
# both standardized and plain (raw) residuals:

resid(fit.bd, type="standardized")
resid(fit.bd, type="raw")

# it seems that our model cannot capture the association
# between occ1 and faed. The standardized residual is 2.5.
# The absolute residual is 0.017. This is on the low side.
# So, I wouldn't worry that much about the fit and leave
# the model as is. One can add a path from faed to occ1
# which would improve the model fit. We can also see this
# in modification indices:

summary(fit.bd, modindices = TRUE)

# Adding occ1 <- faed path would improve chi-square by
# about 13. This could be added to the model, but I leave
# it as is. This discussion is also related to the next
# point:

# f: OK, I'll write down four fitmeasures with original N:
# Chi-sq(2) = 18.119, p < 0.001; RMSEA (90%CI) = 0.012-0.029
# CFI = 0.999; SRMR = 0.005. If we had 10x lower N:

summary(sem(m1, sample.cov = bd.corr, 
              sample.nobs = 2070), fit.measures = TRUE)

# Chi-sq(2) = 1.812, p = 0.4 (exact fit hypo cannot be 
# rejected!!), CFI = 1 (improved slightly), RMSEA (90%CI) 
# is now 0.000-0.042. RMSEA point estimate improved but
# the 90% CI has become wider as power is now low due to 
# low N. SRMR = 0.005, not affected by N. This shows that
# some fit measures are sensitive to sample size. Particularly
# the model chi-square is affected by N. Hence, if you have
# massive N (like we do here with 20700) even small misfits
# turn highly significant. So bear this in mind in 
# interpreting fit measures. 

# g:

# The indirect effects through multiplication annoyed some
# causal people and for good reason. Interpreting those
# indirect effects as causal mediation relies on too 
# strong assumptions. These assumptions are: sequential
# ignorability (e.g., no confounding between any pairs of
# faoc, edu, occ1, and occ2) and no interaction between 
# the exogenous variable and the mediators (e.g.,faoc and
# edu, occ1, and occ2). These assumptions come on top of other
# usual assumptions of the model (multivariate normality, 
# correct functional forms, etc.). Hence in most cases
# a causal mediation interpretation is often very dubious.
# Path models are good summaries of data, but for a causal
# interpretation, we need good designs. Randomized experiments
# which also manipulates the mediators are the gold standard. 
# There are other methods to study causal mediation with 
# observational data than path analysis which are beyond the 
# scope of this course...

################################################################
# Q2: 
################################################################

# a:

dhp_low <- '
1.0000  
.6247  1.0000
.3269   .3669  1.0000
.4216   .3275   .6404  1.0000
.2137   .2742   .1124   .0839  1.0000
.4105   .4043   .2903   .2598   .1839  1.0000
.3240   .4047   .3054   .2786   .0489   .2220  1.0000
.2930   .2407   .4105   .3607   .0186   .1861   .2707  1.0000
.2995   .2863   .5191   .5007   .0782   .3355   .2302   .2950  1.0000
.0760   .0702   .2784   .1988   .1147   .1021   .0931  -.0438   .2087  1.0000
'
dhp.corr <- getCov(dhp_low, 
                   names = c("occasp", "edasp", "fedasp", 
                             "foccasp", "parasp", "iq", "ses", 
                             "fses", "fiq","fparasp"))

N <- 329

# Model 1:
dhp1 <-   'occasp  ~  iq +  ses + fses + foccasp
           foccasp ~ fiq + fses +  ses + occasp
           occasp ~~ foccasp'

fit.dhp <- sem(dhp1, sample.cov = dhp.corr, sample.nobs = N)

summary(fit.dhp)

# Model 2: M2 constrains two path coefs to be zero:

dhp2 <-   'occasp  ~  iq +  ses + 0*fses + foccasp
           foccasp ~ fiq + fses + 0* ses + occasp
           occasp ~~ foccasp'

fit.dhp2 <- sem(dhp2, sample.cov = dhp.corr, sample.nobs = N)

summary(fit.dhp2)

# We can test if the fit of model 2 is significantly worse than
# that of model 1:

anova(fit.dhp, fit.dhp2)

# The test is insignificant. So removing those two path
# coefficients (by fixing at 0) does not worsen the fit
# significantly. 

# I'll fit below model 3 as well. But first I'll do part
# b:
# There is no reason to assume that the effects of ses and iq
# on aspirations should be different for ego and his/her friend
# so I'll constrain those effects to be the same (this is 
# called structural invariance which we'll see in the next few
# lectures!). The same also goes for the effect of iq on friend iq 
# and the effect of friend iq on iq. I'll add those constraints.
# Also, I'll constrain the disturbance error variances of occasp
# and foccasp to be the same, as again no need to assume 
# otherwise?

dhp1_2 <-   'occasp  ~  a*iq  +  b*ses  + 0*fses + d*foccasp
             foccasp ~  a*fiq +  b*fses + 0*ses  + d*occasp
             occasp  ~~  foccasp
             occasp  ~~ f*occasp
             foccasp ~~ f*foccasp'

fit.dhp1_2 <- sem(dhp1_2, sample.cov = dhp.corr, sample.nobs = N)

# Check if indeed our constraints worked:

summary(fit.dhp1_2)

# yes they did. Let's test if those constrains made the fit
# significantly worse or not:

anova(fit.dhp1_2, fit.dhp)

# Test chi-sq(6) = 6.238, p = 0.4. So those constraints did not
# make the fit significantly worse. In other words, they make 
# sense theoretically as well as empirically. We used LR test
# to test those constraints. One can also use a Wald test:

dhp1_3 <-   'occasp  ~  a1*iq  +  b1*ses  + c1*fses + d1*foccasp
             foccasp ~  a2*fiq +  b2*fses + c2*ses  + d2*occasp
             occasp  ~~  foccasp
             occasp  ~~ f1*occasp
             foccasp ~~ f2*foccasp'

con1_3  <- 'a1 == a2
            b1 == b2
            c1 == 0
            c2 == 0
            d1 == d2
            f1 == f2'

fit.dhp1_3 <- sem(dhp1_3, sample.cov = dhp.corr, sample.nobs = N)
lavTestWald(fit.dhp1_3, constraints = con1_3)

# Again no evidence in the data against the constraints. Wald 
# chi-sq(6) = 6.317, p = 0.4. 

# For completeness, I'll look at fitmeasures for our preferred
# model:

summary(fit.dhp1_2, fit.measures = TRUE)

# All those fit measures point to satisfactory fit. The residual
# moments ...:

resid(fit.dhp1_2, type="standardized")
resid(fit.dhp1_2, type="raw")

# ... also show that the observed correlations between variables 
# are well captured by the model.

# Here is model 3 (with equality constraints discussed above:)
# Not sure if this is fully correct., let me know if you see an
# error...

dhp3 <- 'occasp ~  a*iq  + b*ses  + c*fses + d*foccasp
        foccasp ~  a*fiq + b*fses + c*ses  + d*occasp
         edasp  ~  e*iq  + f*ses  + g*fses + h*fedasp + i*occasp 
        fedasp  ~  e*fiq + f*fses + g*ses  + h*edasp  + i*foccasp 
        occasp  ~~ j*occasp
        foccasp ~~ j*foccasp
        edasp   ~~ k*edasp
        fedasp  ~~ k*fedasp

        occasp  ~~ foccasp
        edasp   ~~ fedasp
        occasp  ~~ l*fedasp
        edasp   ~~ l*foccasp'

fit.dhp3 <- sem(dhp3, sample.cov = dhp.corr, sample.nobs = N)

summary(fit.dhp3, fit.measures = TRUE)

################################################################
# End of file, pheww this was hard work (or not?) ... 
################################################################
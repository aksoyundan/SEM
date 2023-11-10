################################################################
############## Answers to computer Practical 4 #################
############### Latent curve modeling  #########################
########################################## 10Nov2023, Ozan Aksoy

#################################################################
# A
#################################################################

# You'd need the read.dta function in foreign package to read in
# stata files. 

dtr <- foreign::read.dta("C:\\Users\\...tr.dta") #adjust filepath
plot(c(2009:2013), c(mean(dtr$MR1), mean(dtr$MR2), mean(dtr$MR3), 
                     mean(dtr$MR4), mean(dtr$MR5)), xlab = "Year",
                     ylab = "Marriage Rate (per 1000)") 

# It seems that the change from 2009 to 2010 was steeper than that 
# in later years. Again in 2013 we see a relatively larger drop in
# marriage rates, although that drop is still smaller compared to
# 2009-2010 difference. A linear trend will likely not work here
# but I will start with it anyways because interpretation is much
# easier with a linear specification.

library(lavaan)
m2 <- 'IMR =~ 1*MR1 + 1*MR2 + 1*MR3 + 1*MR4 + 1*MR5 
       SMR =~ 0*MR1 + 1*MR2 + 2*MR3 + 3*MR4 + 4*MR5 
       MR1 + MR2 + MR3 + MR4 + MR5  ~ 0*1 
       IMR + SMR ~ NA*1  
       IMR ~~ SMR

       MR1 ~~ b*MR2
       MR2 ~~ b*MR3
       MR3 ~~ b*MR4
       MR4 ~~ b*MR5
'
summary(MR2 <- sem(m2, data=dtr), fit.measures = TRUE, 
        standardized = TRUE)

# The fit is not good. Chi-square is rejected, RMSEA is too large.
# Close-fit hypothesis is rejected, too. Recall that N= 81 here.
# So a significant chi-square cannot be due to large N. It has to
# be due to bad fit.
# CFI points to reasonable fit, SRMR is just below the 0.05 cutoff.
# Overall, I would say fit is bad. However, I'll retain this model
# for now as asked in the question, and interpret the results. 

# Firstly, the mean of the latent intercept variable is 32.2.This
# is the adjusted and predicted mean in year 2009. The mean of 
# the linear change (slope) parameter is -0.53. So on average, 
# in every new year, there are about 0.5 less marriages per 1000.
# Both means are statistically significant. The variances
# of the latent variables are significant, too. This means that
# there is significant variations across the 81 provinces of TR
# with respect to the initial levels of marriage rates in 2009 
# and with respect to the change of marriage rates over time. The
# means show the average trend, but provinces do vary wrt the 
# initial marriage rates and how much marriage rates changed over
# time.

# There is a negative correlation between the latent intercept
# and latent slope parameter. This means that provinces with 
# high marriage rates in 2009 experienced a sharper drop (i.e.,
# the change parameter is lower--remember gradient is negative).

# The autocorrelations between the error terms are positive and 
# statistically significant. We could not have these serial
# correlations easily in a standard multilevel model. Also, it
# seems that the variances of error terms vary quite a lot over
# the years (0.92 in 2012, 2.5 in 2010). This heteroskedasticity
# could also not be easily accommodated in a standard multilevel
# model.

# Let's include time constant and time varying predictors. I'll
# constrain the effect of unemployment on marriage to be the same.
# I don't see why this effect should vary over time.

m3 <- 'IMR =~ 1*MR1 + 1*MR2 + 1*MR3 + 1*MR4 + 1*MR5 
       SMR =~ 0*MR1 + 1*MR2 + 2*MR3 + 3*MR4 + 4*MR5 
       MR1 + MR2 + MR3 + MR4 + MR5  ~ 0*1 
       IMR + SMR ~ NA*1  
       IMR ~~ SMR

       MR1 ~~ b*MR2
       MR2 ~~ b*MR3
       MR3 ~~ b*MR4
       MR4 ~~ b*MR5

       IMR  + SMR ~ prov_AKP

       MR1 ~ d*unemp1
       MR2 ~ d*unemp2
       MR3 ~ d*unemp3
       MR4 ~ d*unemp4
       MR5 ~ d*unemp5'

summary(MR3 <- sem(m3, data=dtr), fit.measures = TRUE)

# It seems that an AKP rule in 2005 has a significant positive 
# effect on marriage levels in 2009 (initial rates). However, AKP
# does not have a significant effect on the change in marriage 
# rates over time. Unemployment is negatively associated with
# marriage rates, but the effect is statistically insignificant
# (beta = -0.452, z = -1.430, P = 0.153). 

#################################################################
# B
#################################################################

# We know from the first graph we plotted that the trend is not
# linear. So I'll estimate this trend in a more flexible way. 
# Instead of fixing the growth trend to a linear one, I'll let
# R estimate those parameters below. As asked in Q, I'll keep
# the aurocorrelations in.

m2b <- '
IMR =~ 1*MR1 + 1*MR2 + 1*MR3 + 1*MR4 + 1*MR5 
SMR =~ 0*MR1 + 1*MR2 +   MR3 +   MR4 +   MR5 
MR1 + MR2 + MR3 + MR4 + MR5  ~ 0*1 
IMR + SMR ~ NA*1  
IMR ~~ SMR

MR1 ~~ b*MR2
MR2 ~~ b*MR3
MR3 ~~ b*MR4
MR4 ~~ b*MR5
'
summary(MR2b <- sem(m2b, data=dtr), fit.measures = TRUE, 
        standardized = TRUE)

# The fit is much better. Chi-sq(6) = 8.39, P = 0.211. Hence,
# exact fit hypothesis cannot be rejected. CFI improved to 0.997.
# RMSEA reduced greatly to 0.07. Still larger than ideal, but 
# close fit hypothesis cannot be rejected, either. SRMR is 0.025
# which points to good fit. This fits much better than the linear
# model in Part A. We can perform a likelihood ratio test:

anova(MR2, MR2b)

# Indeed, this model fits data significantly better than the 
# linear growth model. 

# The fitted loadings of items on the latent change/slope variable
# are (0, 1, 1.46, 1.63, 2.32)--note the first two are not fitted 
# but constrained. This shows that the difference between MR in 
# time = 3 (year 2011) and that in time = 1 (year 2009) is only
# 1.46 times higher than the 2009-2010 difference. If the change 
# was linear, the 2011-2009 difference should be 2 times higher 
# than the 2010-2009 difference. Similarly, 2012-2009 difference 
# is only 1.63 times higher than the 2010-2009 difference (a 
# linear trend would suggest the former would be 3 times higher). 

# The estimated mean of the latent intercept variable is 32.4. 
# This is the adjusted predicted mean in 2009. This is much 
# closer to the actual mean (32.39) in that year--than the linear 
# model in A. The mean of the change/slope latent variable is 
# -1.012. This shows the average change in 2010 compared to 2009. 
# So, on average there was 1 less marriage in 2010 per 1000 
# compared to 2009. In 2011, there was 1.460*1.012 = 1.48 less 
# marriages per 1000 compared to 2009, and so on. 

# Variances of both the intercept and slope variables are 
# statistically significant (the latter has a p-value of 0.05!).
# Also, again the correlation between intercept and slope is
# negative, large, and statistically highly significant. Also.
# the autocorrelations are significant. It also seems that
# correlations between the error terms somewhat increased
# over time. 

# Now the predictors. I'll add time constant and time varying
# predictors in two steps. First the time constant effect:

m2c <- '
IMR =~ 1*MR1 + 1*MR2 + 1*MR3 + 1*MR4 + 1*MR5 
SMR =~ 0*MR1 + 1*MR2 +   MR3 +   MR4 +   MR5 
MR1 + MR2 + MR3 + MR4 + MR5  ~ 0*1 
IMR + SMR ~ NA*1  
IMR ~~ SMR

MR1 ~~ b*MR2
MR2 ~~ b*MR3
MR3 ~~ b*MR4
MR4 ~~ b*MR5

IMR + SMR ~ prov_AKP
'
summary(MR2c <- sem(m2c, data=dtr),  
        fit.measures = TRUE, standardized = TRUE)

# We see the same pattern here: AKP affects significantly
# the initial levels of marriage rates, but it does not
# affect the change of marriage rates over time. 

inspect(MR2c, "r2")

# AKP explains about 5% of the variation in the latent intercept
# variable, and only 0.5% of the variation in the latent slope
# parameter. 

# Now the unemployment in. I'll remove the effect of AKP on slope
# as we know from the above model that that effect is insignificant.

m2d <- '
IMR =~ 1*MR1 + 1*MR2 + 1*MR3 + 1*MR4 + 1*MR5 
SMR =~ 0*MR1 + 1*MR2 +   MR3 +   MR4 +   MR5 
MR1 + MR2 + MR3 + MR4 + MR5  ~ 0*1 
IMR + SMR ~ NA*1  
IMR ~~ SMR

MR1 ~~ b*MR2
MR2 ~~ b*MR3
MR3 ~~ b*MR4
MR4 ~~ b*MR5

IMR  ~ prov_AKP

MR1 ~ d*unemp1
MR2 ~ d*unemp2
MR3 ~ d*unemp3
MR4 ~ d*unemp4
MR5 ~ d*unemp5
'
summary(MR2d <- sem(m2d, data=dtr), standardized = TRUE)

# Now the negative effect of unemployment on Marriage Rates
# approaches to significance (z = -1.775, P = 0.076). 

# Somewhat more advanced optional section:
# One can also model changes in unemployment as a latent curve, and
# use the intercept and slope of the unemployment part as 
# predictors for the intercept and slopes of marriage rates. I
# leave this extension to the enthusiastic nerd...

# ... well, ok see below an attempt for this dual-growth modelling:
# (beware, model fit is bad.)

m2e <- '
IMR =~ 1*MR1 + 1*MR2 + 1*MR3 + 1*MR4 + 1*MR5 
SMR =~ 0*MR1 + 1*MR2 +   MR3 +   MR4 +   MR5 
MR1 + MR2 + MR3 + MR4 + MR5  ~ 0*1 
IMR + SMR ~ NA*1  
IMR ~~ SMR

MR1 ~~ b*MR2
MR2 ~~ b*MR3
MR3 ~~ b*MR4
MR4 ~~ b*MR5

IUR =~ 1*unemp1 + 1*unemp2 + 1*unemp3 + 1*unemp4 + 1*unemp5 
SUR =~ 0*unemp1 + 1*unemp2 +   unemp3 +   unemp4 +   unemp5 
unemp1 + unemp2 + unemp3 + unemp4 + unemp5  ~ 0*1 
IUR + SUR ~ NA*1  
IUR ~~ SUR

#unemp1 ~~ e*unemp2
#unemp2 ~~ d*unemp3
unemp3 ~~ c*unemp4
unemp4 ~~ c*unemp5

IMR + IUR + SUR ~ prov_AKP
IMR ~ IUR 
SMR ~ SUR 
'
summary(MR2e <- sem(m2e, data=dtr), standardized = TRUE, 
        modindices = TRUE, fit.measures = TRUE)

#################################################################
# 1
#################################################################

# Let's first read the data in. Note that the original data is in
# Stata format with value labels, so it is better to use the 
# option convert.factors = FALSE in read.dta() command. 

# adjust file path
dbhps <- foreign::read.dta("C:\\Users\\...\\bhps.dta", 
                          convert.factors = FALSE)

#################################################################
# A
#################################################################

# Here is the cross-legged panel model. Below I am constraining 
# the coefficients to be the same across the different waves.You
# can try different specifications.

dmp <- '
# trust as outcome
tr2  ~ b*tr1 + a*vol1
tr3  ~ b*tr2 + a*vol2
tr4  ~ b*tr3 + a*vol3

# volunteering as outcome
vol2  ~ c*tr1 + d*vol1
vol3  ~ c*tr2 + d*vol2
vol4  ~ c*tr3 + d*vol3

#disturbance correlations
tr2 ~~ e*vol2
tr3 ~~ e*vol3
tr4 ~~ e*vol4

#the only two exogenous variables which should be correlated
tr1 ~~ vol1'

# Let's fit the model now:

fdmp <- sem(dmp, data = dbhps, missing="ML")
summary(fdmp, fit.measures = TRUE)

# N is nearly 23000 which is due to FIML. The Chi-sq is huge
# which is highly significant. CFI = 0.869, RMSEA = 0.085,
# SRMR = 0.084. These fit measures all indicate poor fit. 
# If we ignore the fit and look at the results, we will see
# that volunteering has a significant effect on subsequent
# measure of trust (b = 0.079, P < 0.0001), and trust has on
# volunteering (b = 0.027, P < 0.0001). 

modificationindices(fdmp)

# When we look at the modification indices we can see that
# there are some that are very large. Many of those e.g.
# tr3 ~~ tr1 are about correlations between the variable
# across further measurement occasions than the 1st occasion.
# Recent SEM literature suggests that it is generally good
# practice to allow the variable to have an effect on future
# values further than just the next wave. Let's now try this.

dmp2 <- '
# trust as outcome but also of earlier waves
tr2  ~ b*tr1 + a*vol1
tr3  ~ b*tr2 + a*vol2 + b2*tr1 + a2*vol1
tr4  ~ b*tr3 + a*vol3 + b2*tr2 + a2*vol2

# volunteering as outcome
vol2  ~ c*tr1 + d*vol1
vol3  ~ c*tr2 + d*vol2 + d2*vol1 + c2*tr1
vol4  ~ c*tr3 + d*vol3 + d2*vol2 + c2*tr2

#disturbance correlations
tr2 ~~ e*vol2
tr3 ~~ e*vol3
tr4 ~~ e*vol4

#the only two exogenous variables which should be correlated
tr1 ~~ vol1'

# Let's fit the new model. 

fdmp2 <- sem(dmp2, data = dbhps, missing="ML")
summary(fdmp2, fit.measures = TRUE)

# Indeed the fit has improved considerably. Apart from 
# the large Chi-sq (which is expected given large N), other
# fit measures indicate reasonable fit. In fact we can even 
# compare the two cross-legged models as they are nested:

anova(fdmp, fdmp2)

# The second model fits data significantly better than the
# first. The results of this model also show that both
# variables affect each other and that those effects decrease
# as the measurement lag increases. Let's now also add even
# further effects in time:

dmp3 <- '
# trust as outcome but also of earlier waves
tr2  ~ b*tr1 + a*vol1
tr3  ~ b*tr2 + a*vol2 + b2*tr1 + a2*vol1
tr4  ~ b*tr3 + a*vol3 + b2*tr2 + a2*vol2 + b3*tr1 + a3*vol1

# volunteering as outcome
vol2 ~ c*tr1 + d*vol1
vol3 ~ c*tr2 + d*vol2 + c2*tr1 + d2*vol1
vol4 ~ c*tr3 + d*vol3 + c2*tr2 + d2*vol2 + c3*tr1 + d3*vol1 

#disturbance correlations
tr2 ~~ e*vol2
tr3 ~~ e*vol3
tr4 ~~ e*vol4

#the only two exogenous variables which should be correlated
tr1 ~~ vol1'

fdmp3 <- sem(dmp3, data = dbhps, missing="ML")
summary(fdmp3, fit.measures = TRUE)

anova(fdmp3, fdmp2)

# Again the fit improved significantly with those further
# apart effects. The results are qualitatively the same:
# both variables affect the other variable, and this effect
# decreases, as the measurement distance increases. 
# The autoregressions are always significant

#################################################################
# B
#################################################################

# Let's now bring in the fixed effects:

dmpfe <- '
# The two fixed effects:

TR  =~ 1*tr2  + 1*tr3  + 1*tr4 
RA  =~ 1*vol2 + 1*vol3 + 1*vol4

# Correlations of all exogenous variables including the FE

TR ~~ tr1 + vol1 + RA
tr1 ~~ vol1 + RA
vol1 ~~ RA

# regressions
tr2  ~ b*tr1 + a*vol1
tr3  ~ b*tr2 + a*vol2
tr4  ~ b*tr3 + a*vol3

vol2  ~ c*tr1 + d*vol1
vol3  ~ c*tr2 + d*vol2
vol4  ~ c*tr3 + d*vol3

#disturbance correlations
tr2 ~~ e*vol2
tr3 ~~ e*vol3
tr4 ~~ e*vol4'

fdmfe <- sem(dmpfe, data = dbhps, missing="ML")
summary(fdmfe, fit.measures = TRUE)

# The fit of this model is already very good. Apart from
# the chi-sq, other fit measures (CFI, RMSEA, SRMR) all
# point to satisfactory fit. When we look at the results
# we see that the only significant coefficients are the
# auto-regressions. Trust has no significant effect on 
# volunteering and neither does volunteering on trust.
# So when we control for all time-constant variables 
# measured and unmeasured at the individual level, then
# within person change on one variable is not related to
# within person change on the other. These nil results
# cast doubt on the earlier findings above: if there was
# a causal effect, they should also appear here too.

# The model in fdmfe encompasses fdmp. So we can compare
# the two models:

anova(fdmfe, fdmp)

# Indeed the model with individual level fixed effects
# fits data significantly better than the one without. 

# So overall, the cross-leggel panel model shows significant
# effects which attenuate once we add fixed effects. This
# suggests that the effects we observed without fixed effects
# is likely due to some individual level confounder that
# has effects on both trust and volunteering. 

#################################################################
# C
#################################################################
# The final model a la Allison:

allison <- '
# FE of the outcome var only
VOL  =~ 1*vol2 + 1*vol3 + 1*vol4

# regressions
vol2  ~ b*vol1 + a*tr1
vol3  ~ b*vol2 + a*tr2
vol4  ~ b*vol3 + a*tr3

# reverse causality:
vol2  ~~ tr3

# all correlations
VOL  ~~ vol1 + tr1 + tr2 + tr3
vol1 ~~ tr1 + tr2 + tr3
tr1  ~~ tr2 + tr3
tr2  ~~ tr3
vol2 ~~ vol2
vol3 ~~ vol3
vol4 ~~ vol4'


fallison <- sem(dmpallison, data = dbhps, missing="ML")

summary(fallison, fit.measures = TRUE)

# This model too has a good fit, and it also shows that there is 
# no significant effect of trust on volunteering, once fixed 
# effects for individuals are taken into account. 

# We can also compare the models in C and B. They are not nested, 
# so we cannot perform a formal chi-square test. But we can 
# nevertheless compare them based on BIC. The anova() function
# conveniently prints those values. But we should ignore the
# result of the formal chi-sq test:

anova(fdmfe, fallison)

# The Allison ea model has a much lower BIC and a AIC score. So 
# that can be preferred over the fdmfe in B based on empirical 
# grounds. 
# Overall, howver, if one is interested in both the tr > vol and 
# vol > tr effects, the model in B is the way to go. But if one has 
# a clear DV such as vol in part C, then the allison ea model is 
# more flexible.

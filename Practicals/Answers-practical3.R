#################################################################
################ Computer Practical 3 ###########################
################# Multiple Group Analysis ####################### 
################ Structural Regression Models ###################
########################################### 06Nov2023, Ozan Aksoy

#################################################################
# A
#################################################################

gss <- as.data.frame(matrix(scan("gss1988_1998.dat"), ncol = 12, 
                            byrow = TRUE))
colnames(gss) <- c("year", "id", "sibs", "age", "educ", "paeduc",
                   "maeduc", "speduc", "sex", "race", "rincome", 
                   "sei")
gss[gss==9999] <- NA

# selecting only years 88,93,98; you can use subset() if you 
# like it more ...

gss2 <- gss[gss$year==1988 | gss$year==1993 | gss$year==1998, ]

mp <- '
  educ    ~ c(a88,a93,a98)*paeduc + c(b88,b93,b98)*sibs
  sei     ~ c(c88,c93,c98)*educ
  rincome ~ c(d88,d93,d98)*educ   + c(e88,e93,e98)*sei'

c <- '
a88==a93
a88==a98

b88==b93
b88==b98

c88==c93
c88==c98

d88==d93
d88==d98

e88==e93
e88==e98
'
fp <- sem(mp, data=gss2, group = "year", missing = "ML")

lavTestWald(fp, constraints = c)

# Chisq(10) = 29.7, p = 0.001. So structural invariance does not
# seem to hold. However, chisq value is not very big. So maybe 
# we can attain partial structural invariance. I'll now check
# whether we can constrain some of the parameters to be equal
# across years. To do this, I'll start with a fully invariant 
# model and use modification indices to see which parameters 
# could be relaxed. 

mpiv <- '
    educ~ c(a,a,a)*paeduc + c(b,b,b)*sibs
    sei ~ c(c,c,c)*educ
rincome ~ c(d,d,d)*educ + c(e,e,e)*sei'

fpiv <- sem(mpiv, data=gss2, group = "year", missing = "ML")

anova(fpiv, fp)

# LR test result show the same story: structural invariance does
# not hold. Let's see which parameters can be freed:

miv <- modindices(fpiv, free.remove=FALSE)
miv[miv$op == "~",]

# it seems that if we free the effect of sibs on sei in 
# year 88 (now this effect is fixed at zero), model fit will 
# improve. However,  I would not want to add new paths in
# the model. While a sei ~ sibs path may make sense, it is not 
# what we are after here. We are checking structural invariance 
# for the model we now have. So I'll relax the effect of sibs on 
# educ in 1988:

mpiv2 <- '
    educ~ c(a,a,a)*paeduc + c(b2,b,b)*sibs
    sei ~ c(c,c,c)*educ
rincome ~ c(d,d,d)*educ + c(e,e,e)*sei
'
fpiv2 <- sem(mpiv2, data=gss2, group = "year", missing = "ML")

anova(fpiv2, fp)

# And indeed the difference in fits of this model with the model
# which does not assume any invariance is statistically 
# insignificant (chi2(9) = 15.4, p = 0.08). One could also allow 
# the effect of educ on rincom to be free in 1988. But I'll leave 
# it here.

# The optional Q: We need to go back to the model without 
# structural invariance (otherwise--ie in model with invariance
# differences in coeffs across years are all zero). I'll show
# this for only one parameter, but the same approach applies
# for other parametes as well:

mp2 <- '
  educ    ~ c(a88,a93,a98)*paeduc + c(b88,b93,b98)*sibs
  sei     ~ c(c88,c93,c98)*educ
  rincome ~ c(d88,d93,d98)*educ   + c(e88,e93,e98)*sei
  d1         := b93 - b88
  d2         := b98 - b93
  diffIndiff := d2 - d1'

summary(fp2 <- sem(mp2, data=gss2, group = "year", missing = "ML"))

# We cannot reject the null that the differences in coeffs are the
# same, hence, linearity of effects (check the estimate, SE, and 
# p-value for new parameter called diffIndiff). 

#################################################################
# eof
#################################################################
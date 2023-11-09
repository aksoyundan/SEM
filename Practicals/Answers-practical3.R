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
# B Worries
#################################################################

pew <- as.data.frame(matrix(scan("pew_worries.dat"), ncol = 5, 
                            byrow = TRUE))
colnames(pew) <- c("country", "hostil", "q26a", "q26b", "q26c")

pew[pew==99] <- NA
pew[pew==9]  <- NA
pew[pew==8]  <- NA

# Model that assumes invariance (do not forget to free
# means for all but one country, it is silly that R-default
# is that all means are zero under measurement invariance):

miv <- 'W    =~ q26a + c(b,b,b,b)*q26b + c(c,c,c,c)*q26c
        q26a  ~ c(ia,ia,ia,ia)*1
        q26b  ~ c(ib,ib,ib,ib)*1
        q26c  ~ c(ic,ic,ic,ic)*1
        W     ~ c(0, NA,NA,NA)*1'

summary(fiv <- sem(miv, data=pew, group = "country"
                   , missing = "ML"), 
        fit.measures = TRUE)

# Note, I use missing = "ML" argument as there are some missing
# data, and this argument forces to use Full Information Maximum 
# Likelihood (FIML) which is far better than using listwise
# deletion (the default). We'll see why FIML is good tomorrow.

# Model Chi-2 (12) = 122.90, p < 0.001, CFI, RMSEA, SRMR all
# point to horrible fit. So obviously strong measurement 
# invariance does not hold. We can perform an LR test to 
# confirm this:

# The model that does not assume invariance:

m <- 'W =~ q26a + q26b + q26c'
summary(f <- sem(m, data=pew, group = "country", missing = "ML"))

anova(f, fiv)

# The model with strong invariance fits data significantly worse
# than the model without invariance. Let's see if we can have
# partial strong invariance:

print(miv <- modindices(fiv, free.remove=FALSE))

# It seems that the intercept of Q26b wants to be free.I'll 
# allow this parameter to be free and leave it to you to come up
# with a theoretical justification of this.

miv2 <- 'W    =~ q26a + c(b,b,b,b)*q26b + c(c,c,c,c)*q26c
        q26a  ~ c(ia,ia,ia,ia)*1
        q26b  ~ c(ib1,ib2,ib3,ib4)*1
        q26c  ~ c(ic,ic,ic,ic)*1
        W     ~ c(0, NA,NA,NA)*1'

summary(fiv2 <- sem(miv2, data=pew, group = "country",
                    missing = "ML"), 
        fit.measures = TRUE)

anova(fiv2, f)

# Chi-sq(9)=21.36, p = 0.011. I'd use 0.01 for cutoff value and
# conclude that this is insignificant. Other fit measures point
# to reasonable fit. RMSEA is large, but none of the modification 
# indices is larger than 10:

modindices(fiv2, free.remove=FALSE)

# So, I'll use this model with partial strong invariance for
# further analyses. 

# Tests of equality of means and variances across the four
# countries (note, I am doing an LR test here):

miv3 <- 'W    =~ q26a + c(b,b,b,b)*q26b + c(c,c,c,c)*q26c
        q26a  ~ c(ia,ia,ia,ia)*1
        q26b  ~ c(ib1,ib2,ib3,ib4)*1
        q26c  ~ c(ic,ic,ic,ic)*1
         W     ~ c(m, m, m, m)*1'
fiv3 <- sem(miv3, data=pew, group = "country", missing = "ML")

anova(fiv3, fiv2)

# Equality of means is flatly rejected. Hence the differences
# in the mean of worries across countries are highly 
# significant. If you look at the results of model fiv2, you'll
# see that the mean of worries in the UK is constrained to be 0
# for identification. The mean in all other countries are 
# negative. Hence, Muslim immigrants are significantly less
# worried about Westernization in those other countries 
# compared to the UK. One can also test if the differences 
# between France, Germany, and Spain are significant:

miv4 <- 'W    =~ q26a + c(b,b,b,b)*q26b + c(c,c,c,c)*q26c
        q26a  ~ c(ia,ia,ia,ia)*1
        q26b  ~ c(ib1,ib2,ib3,ib4)*1
        q26c  ~ c(ic,ic,ic,ic)*1
         W    ~ c(m0,m1,m2,m3)*1
         m0==0
         m1==m2
         m1==m3'
fiv4 <- sem(miv4, data=pew, group = "country", missing = "ML")

anova(fiv4, fiv2)

# The test is insignificant, hence we have UK on the one hand
# and the three other countries on the other. Worries are high
# in the UK, in the three other countries worries are lower, 
# and the differences between the three non-UK countries are
# insignificant. 

# Now the test of equality of variances:

miv5 <- 'W    =~ q26a + c(b,b,b,b)*q26b + c(c,c,c,c)*q26c
        q26a  ~ c(ia,ia,ia,ia)*1
        q26b  ~ c(ib1,ib2,ib3,ib4)*1
        q26c  ~ c(ic,ic,ic,ic)*1
        W     ~ c(0, NA,NA,NA)*1
        W     ~~ c(V,V,V,V)*W'

fiv5 <- sem(miv5, data=pew, group = "country", missing = "ML")

anova(fiv5, fiv2)

# The differences in variances are statistically significant as
# well.

# Finally the structural invariance. I'll now test if
# the effects of hostil on W and the intercepts of W are the same 
# across counries

msiv <- 'W    =~ q26a + c(b,b,b,b)*q26b + c(c,c,c,c)*q26c
        q26a  ~ c(ia,ia,ia,ia)*1
        q26b  ~ c(ib1,ib2,ib3,ib4)*1
        q26c  ~ c(ic,ic,ic,ic)*1
         W    ~  c(iw1,iw2,iw3,iw4)*1
         W    ~ c(bw1,bw2,bw3,bw4)*hostil
        iw1== 0   ' # required for identification

fsiv <- sem(msiv, data=pew, group = "country", missing = "ML")

# This time I'll use Wald tests--just to show different ways of 
# testing:

conb <- 'bw1 == bw2
         bw1 == bw3
         bw1 == bw4'

# Test of equality of regression parameters:

lavTestWald(fsiv, constraints = conb)

# Chi-sq(3) = 12.50, p = 0.006. The equality of coefficients 
# hypothesis is rejected. So there are significant differences
# between countries regarding the association between hostil 
# and worries. In fact, it seems that in the UK and France 
# there is no association but in Germany and Spain perceived
# hostility is strongly associated with worries. 

# Let's now test equality of intercepts:

coni <- 'iw1 == iw2
         iw1 == iw3
         iw1 == iw4'

lavTestWald(fsiv, constraints = coni)

# Chi-sq(3) = 70.71, p < 0.001. Again the test is flatly rejected
# so intercepts (expected value of worries when perceived 
# hostility == 0) do differ significantly across the countries.


#################################################################
# eof
#################################################################

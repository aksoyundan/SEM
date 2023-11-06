#################################################################
################ Computer Practical Week-4 ######################
################ Confirmatory Factor Analysis ###################
################ Structural Regression Models ###################
########################################### 06Nov2023, Ozan Aksoy

#################################################################
# A
#################################################################
brn_c <- '
0.8550
0.3020   0.7480
0.3020   0.4050  1.7090
0.2710   0.2780   0.4370   0.7860
0.2800   0.3410   0.5390   0.4780   1.4160
0.2350   0.3530   0.6960   0.3840   0.7150   1.2540
0.2220   0.3420   0.5040   0.2910   0.3280   0.3210   1.5960
-0.2970  -0.3610  -0.3860  -0.3320  -0.4180  -0.3900  -0.2360  2.2430
-0.2290  -0.2450  -0.3600  -0.3610  -0.5060  -0.4080  -0.2830  0.8520  2.2100
-0.2500  -0.2730  -0.3930  -0.3300  -0.4270  -0.4000  -0.2860  0.7970  1.5820  2.3730
-0.2490  -0.2540  -0.2880  -0.2300  -0.3770  -0.2890  -0.1520  0.7040  0.6100  0.6240  1.2350
-0.1460  -0.2410  -0.1500  -0.3100  -0.2510  -0.3150  -0.0710  0.7090  0.6270  0.7200  0.4730  2.4910
-0.0640  -0.1430  -0.2590  -0.3150  -0.2860  -0.2350  -0.1840  0.5660  0.7220  0.8510  0.3610  0.6270  2.6590
-0.0810  -0.1170  -0.2320  -0.2870  -0.2560  -0.2150  -0.2390  0.5050  0.6340  0.7120  0.2930  0.4970  1.8990  2.4560
-0.2010  -0.2720  -0.4890  -0.3600  -0.5470  -0.4350  -0.3570  0.7530  0.8360  0.9830  0.4790  0.5870  1.6450  1.5900  2.8740
-0.1200  -0.2820  -0.2510  -0.3470  -0.4350  -0.3570  -0.3150  0.9620  0.8320  0.8470  0.5220  0.6420  1.1530  1.0820  1.2860  2.6900
-0.2260  -0.3160  -0.5140  -0.4930  -0.6230  -0.4300  -0.4080  0.8130  0.9970  1.1500  0.5750  0.6790  1.8520  1.7200  1.9650  1.5340  3.1630
-0.2110  -0.3090  -0.5530  -0.3870  -0.5400  -0.4610  -0.3130  0.7730  1.0550  1.1590  0.5180  0.8600  1.5670  1.3590  1.5200  1.3040  1.8040  2.9240
-0.0410  -0.1140  -0.0910  -0.2190  -0.1750  -0.0970  -0.1970  0.4740  0.6720  0.8580  0.2950  0.5770  1.5940  1.5290  1.4720  1.0810  1.6260  1.7140  3.2550
-0.1790  -0.2340  -0.4170  -0.3810  -0.4340  -0.3280  -0.3490  0.6280  0.8190  0.9030  0.4890  0.6950  1.2470  1.1200  1.3270  1.0890  1.7510  1.4940  1.1940  2.1630  
'

c.burn <- getCov(brn_c,
                   names = c("ITEM4", "ITEM7", "ITEM9", "ITEM17", "ITEM18", "ITEM19", 
                             "ITEM21", "ITEM5", "ITEM10", "ITEM11", "ITEM15", "ITEM22", 
                             "ITEM1", "ITEM2", "ITEM3", "ITEM6", "ITEM8", "ITEM13", 
                             "ITEM14", "ITEM20"))

burn1 <- '
#emotional exhaustion
EE =~ ITEM1 + ITEM2 + ITEM3 + ITEM6 + ITEM8 + ITEM13 + ITEM14 + ITEM20
#depersonalisation	 
DP =~ ITEM5 + ITEM10 + ITEM11 + ITEM15 + ITEM22
#personal accomplishment 
PA =~ ITEM4 + ITEM7 + ITEM9 + ITEM17 + ITEM18 + ITEM19 +ITEM21
'

fburn1 <- sem(burn1, sample.cov = c.burn, 
            sample.nobs = 1159)

summary(fburn1, fit.measures = TRUE)

# If we look at the fit measures, exact fit hypothesis is rejected
# Chi-sq(167) = 1151.82, p < 0.001. Other fit measures also point to
# bad fit. CHI = 0.89,  RMSEA = 0.071 with a 90% CI of 0.067-0.075,
# close fit hypothesis is also rejected (p < 0.001). SRMR is also just
# above the traditional cutoff value of 0.05. All those point to 
# unsatisfactory fit. We'll need to modify the model. I'll first 
# inspect the R-sqs for items. If some R-squares are low it would mean
# that those items do not really measure well what they are supposed to
# measure. 

inspect(fburn1, 'r2')

# Items 21, 22, and 4 look suspicious, R^2 are too low. We should drop 
# these one by one, and see if R-squares of remanining items improve. 
# Here, however, I'll drop them at once to save time:

burn2 <- '
#emotional exhaustion
EE =~ ITEM1 + ITEM2 + ITEM3 + ITEM6 + ITEM8 + ITEM13 + ITEM14 + ITEM20
#depersonalisation	 
DP =~ ITEM5 + ITEM10 + ITEM11 + ITEM15 
#personal accomplishment 
PA =~  ITEM7 + ITEM9 + ITEM17 + ITEM18 + ITEM19 
'
summary(sem(burn2, sample.cov = c.burn, 
              sample.nobs = 1159), fit.measures = TRUE)
# Fit measures did not improve much, but at least we got rid of items
# with very low R-squares:

inspect(sem(burn2, sample.cov = c.burn, 
            sample.nobs = 1159), "r2")

# One could further drop ITEM5 and ITEM15, but I'll leave them for now.
# I'll now check modification indices to see if I can relax some of
# covariances between the item residuals:

mi <- modindices((sem(burn2, sample.cov = c.burn, 
            sample.nobs = 1159)))

mi[mi$mi>10,]

# Some modification indicises are huge. I'd relax cov(e.i1, e.i2) and
# cov(e.i10, e.i11). Again, normally do these one by one. But here I
# am adding them at once to save time and space:

burn3 <- '
#emotional exhaustion
EE =~ ITEM1 + ITEM2 + ITEM3 + ITEM6 + ITEM8 + ITEM13 + ITEM14 + ITEM20
ITEM1 ~~ ITEM2
#depersonalisation	 
DP =~ ITEM5 + ITEM10 + ITEM11 + ITEM15 
ITEM10 ~~ ITEM11
#personal accomplishment 
PA =~  ITEM7 + ITEM9 + ITEM17 + ITEM18 + ITEM19 
'

summary(sem(burn3, sample.cov = c.burn, 
            sample.nobs = 1159), fit.measures = TRUE)

# OK fit improved somewhat, but we are still not there.

mi2 <- modindices((sem(burn3, sample.cov = c.burn, 
                      sample.nobs = 1159)))
mi2[mi2$mi>10,]

# MI suggest that item 14 may also have something to do with PA. Several
# other large MIs also involve involve i14. So it seems like a 
# problematic item. I'd drop that too. And I'll add cov(item5, item6). 
# This latter modification is not ideal, we do not normally want to
# add error covriances between items of different factors unless
# there is a good reason to do so. But the fit is all over the place.

burn4 <- '
#emotional exhaustion
EE =~ ITEM1 + ITEM2 + ITEM3 + ITEM6 + ITEM8 + ITEM13 + ITEM20
ITEM1 ~~ ITEM2
#depersonalisation	 
DP =~ ITEM5 + ITEM10 + ITEM11 + ITEM15 
ITEM10 ~~ ITEM11
ITEM5 ~~ ITEM6
#personal accomplishment 
PA =~  ITEM7 + ITEM9 + ITEM17 + ITEM18 + ITEM19 
'

summary(fburn4 <- sem(burn4, sample.cov = c.burn, 
            sample.nobs = 1159), fit.measures = TRUE, 
                                  modindices = TRUE)

# Fit is still not perfect, exact fit hypothesis is rejected 
# (Chi-2 (98) = 405.086, p < 0.001). However, other fit measures
# point to satisfactory fit, CFI = 0.96, RMSEA is just above 0.05 
# with a 90% CI 0.047-0.057. Also, close fit hypothesis cannot be 
# rejected (p = 0.258), so although fit is not "exact", it seems
# "close". SRMR is also just below the traditional cutoff 0.05. 
# So, overall I'd live with this model. Just as a final check, 
# I'll inspect the residual moments:

resid(fburn4, type="standardized")
resid(fburn4, type="raw")

# It seems that the covariance between item 8 and item 13 is not 
# well captured by our model. The same is true for cov. between 
# Item 17 and Item 19. One could go further to modify the model 
# until we have a better fitting model that accounts for those 
# covariances better. However, I leave it here. The final model 
# has reasonable fit. N is relatively large, so model chi-sq 
# would be significant even for small deviations. The residual 
# moments, however, are indeed large. So the significant 
# chi-square doesn't look like a big N issue only.

#################################################################
# B
#################################################################

 m <- c(720.86, 15.54, 18.46, 14.90, 14.35, 19.57, 24.16, 21.36)
sd <- c(  2.09,  3.43,  2.81,  1.95,  2.06,  2.16,  2.06,  3.65)

c  <- ' 
1.0000  
0.4180  1.0000
0.3940  0.6270  1.0000
0.1290  0.2020  0.2660  1.0000
0.1890  0.2840  0.2080  0.3650  1.0000
0.5440  0.2810  0.3240  0.2010  0.1610  1.0000
0.5070  0.2250  0.3140  0.1720  0.1740  0.5460  1.0000
-0.3570 -0.1560 -0.0380 -0.1990 -0.2770 -0.2940 -0.1740 1.0000
'

d.job <- getCov(c, lower = TRUE, sds = sd,
                names = c("PERFORMM", "JBSATIS1", "JBSATIS2",
                          "ACHMOT1", "ACHMOT2", "TSSE1", "TSSE2",
                          "VERBINTM"))
mod <-'
#measurement part
AM  =~ ACHMOT1 + ACHMOT2
TS  =~   TSSE1 +   TSSE2
JBS =~JBSATIS1 +JBSATIS2 

#structural part
JBS      ~ AM + TS + PERFORMM + VERBINTM
PERFORMM ~ AM + TS            + VERBINTM'

mjob1 <- sem(mod, sample.cov = d.job, sample.nobs = 112, 
                                    sample.mean = m)

# This is the model. Let's have a look at the results:

summary(mjob1, fit.measures = TRUE, modindices = TRUE)

# First fit: model chi-sq is just rejected at the 0.05 level.
# CFI is not very high, RMSEA and SRMR are high, too though
# close-fit hypothesis cannot be rejected (p = 0.148). To me,
# fit can be improved. I also asked modindices to be printed.
# Let's look at those indices. Two of them are particularly
# high: TS  ~ PERFORMM and TS =~ PERFORMM. In the original 
# model, we predicted an effect of self-esteem on performance.
# The modification indices suggests that performance may also
# affect self-esteem. This sounds theoretically reasonable too.
# Self-esteem and performance may simultaneously affect each
# other. So, I'll add this to the model. Adding this path
# will make the model non-recursive because there will be a 
# feedback loop between TS and PERFORMM. And also, once we
# add this feedback loop, I think it makes sense to also add
# a  covariance between the residuals of TS and PERFORMM. This
# is because when two variables affect each other simultaneously,
# there may be other factors that effect both. This other 
# factors are captured by the error covariance. I'll thus also
# add the error cov. Note that this model will not be identified
# as such. So, for identification, I'll constrain the effect of 
# TS on PERFORMM to be the same as the effect of PERFORMM on
# TS. Do not forget to add TS ~~ AM in the model once you add
# TS ~ PERFORMM. Otherwise, lavaan will force TS ~~ AM to be
# zero. The modified model will look like below:

mod2 <-'
#measurement part
AM  =~ ACHMOT1 + ACHMOT2
TS  =~   TSSE1 +   TSSE2
JBS =~JBSATIS1 +JBSATIS2 

#structural part
JBS      ~ AM +   TS + PERFORMM + VERBINTM
PERFORMM ~ AM + a*TS            + VERBINTM

#modification
TS       ~ a*PERFORMM
TS ~~ PERFORMM
TS ~~ AM
'

mjob2 <- sem(mod2, sample.cov = d.job, sample.nobs = 112, 
             sample.mean = m)

summary(mjob2, fit.measures = TRUE)

# Chi-sq fine, CFI improved. RMSEA and SRMR decreased. The fit
# is still imperfect (SRMR and the upper 90%CI boundary of 
# RMSEA is too hight), but better than what we had. We can also
# compare this with the original specification:

anova(mjob1, mjob2)

# The updated model fits significantly better (at the 0.05) 
# level. So, I'd retain this updated version. 
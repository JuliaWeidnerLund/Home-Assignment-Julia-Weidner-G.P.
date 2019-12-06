########################################################################################################################################
####Exercise 3 Geoff####################################################################################################################
########################################################################################################################################

####Monte Carlo Simulation##############################################################################################################

require(stats)#for power anova test

#groupmean variable using standardized groupmeans 

groupmeans_standard <- c(-.5,0,0,.5)

#power analysis

power_analysis <- power.anova.test(groups = length(groupmeans_standard), 
                      between.var = var(groupmeans_standard), within.var = 6400, 
                      power=NULL,sig.level=0.05,n=n)

power_analysis

#################################################
####second try###################################
#################################################
#find out right within.var before doing this:
#try it for 80
power.anova.test(groups = 4, between.var = 1, within.var = .5,
                 power = .80)
#try for 50
power.anova.test(groups = 4, between.var = 1, within.var = .5, #for within var I took .5 but that is sum of squares I guess
                 power = .50)


#with replicate function we can replicate something many times

power_replicate <- replicate(n=100, rnorm(n=100, mean= ,sd=1))
power_replicate

####################################################
####new try#########################################
####################################################


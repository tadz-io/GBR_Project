# Plot x-intercept of spline correlogram per transect
# x-intercept of spline fit and mean x-intercept of bootstrap is displayed including confidence intervals
# script requires output from spline_par.R
#*********************************************************************************************************

# sp.autocorr is out from spline_par.R

# calculate standard error (bootstrap iterations = 1000)
se = sp.autocorr$sd/sqrt(1000)
# calculate 95% confidence interval
ci = 1.96 * se
sp.autocorr[,5] = ci
colnames(sp.autocorr)[5] = c("ci")

# plot x-intercept of spline fit + bootstrap and confidence interval
p = ggplot(data = sp.autocorr, aes(x = as.factor(trans), y=mean)) + 
  geom_point() +
  geom_errorbar(data = sp.autocorr, aes(ymin=mean-ci, ymax = mean+ci), width=0.4, colour="red") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
p

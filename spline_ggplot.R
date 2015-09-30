# Spline correlogram plot
# extract the 25th and 975th element of every bootstrap distribution to create 95% CI (see BJORNSTAND 2001 and EFRON & TIBSHIRANI 1993)
# in this case the function is applied to transect # 10001 and 10008
q.t10001 = t(apply(spline.t10001$boot$boot$predicted$y, 2, quantile, probs = c(0.025,0.975)))
q.t10008 = t(apply(spline.t10008$boot$boot$predicted$y, 2, quantile, probs = c(0.025,0.975)))

# create dataframes
dat.t10001 = data.frame(x = spline.t10001$real$predicted$x,
                        y = spline.t10001$real$predicted$y,
                        q25  = q.t10001[,1],
                        q975 = q.t10001[,2])

dat.t10008 = data.frame(x = spline.t10008$real$predicted$x,
                        y = spline.t10008$real$predicted$y,
                        q25  = q.t10008[,1],
                        q975 = q.t10008[,2])


# plot spline with confidence intervals
library(ggplot2)

# spline plot transect #10001
p.t10001 = ggplot(dat.t10001, aes(x = x, y = y)) +
  geom_abline(intercept = 0, slope = 0, size=0.3) +
  geom_vline(xintercept = dat.t10001$x[15], size = 0.2, colour="black", linetype="longdash") +
  geom_vline(xintercept = dat.t10001$x[23], size = 0.2, colour="black", linetype="longdash") +
  geom_ribbon(aes(ymin = q25, ymax = q975), alpha=0.3) +
  geom_line(colour="black") +
  geom_line(aes(y = q975), alpha=0.5) +
  geom_line(aes(y = q25), alpha=0.5) +
  scale_y_continuous(limits = c(-.15,.4)) +
  scale_x_discrete(limit = c(seq(0,650,100))) +
  coord_cartesian(xlim = c(1,500)) +
  theme_bw() +
  theme(axis.line = element_line(size=0.3, colour="black"),
        axis.line.x = element_blank(),
        axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14)) +
  labs(x = "distance (m)", y = "spatial autocorrelation")
p.t10001

# save in eps format
# need to specify cairo device for transparency support in graphics
ggsave("spline_ggplot_t10001_manu.eps", device=cairo_ps)

# spline plot transect #10008
p.t10008 = ggplot(dat.t10008, aes(x = x, y = y)) +
  geom_abline(intercept = 0, slope = 0, size=0.3) +
  geom_vline(xintercept = dat.t10008$x[18], size = 0.2, colour="black", linetype="longdash") +
  geom_vline(xintercept = dat.t10008$x[61], size = 0.2, colour="black", linetype="longdash") +
  geom_ribbon(aes(ymin = q25, ymax = q975), alpha=0.3) +
  geom_line(colour="black") +
  geom_line(aes(y = q975), alpha=0.5) +
  geom_line(aes(y = q25), alpha=0.5) +
  scale_y_continuous(limits = c(-.15,.4)) +
  scale_x_discrete(limit = c(seq(0,650,100))) +
  coord_cartesian(xlim = c(0,500)) +
  theme_bw() +
  theme(axis.line = element_line(size=0.3, colour="black"),
        axis.line.x = element_blank(),
        axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14)) +
  labs(x = "distance (m)", y = "spatial autocorrelation")
p.t10008

# save in eps format
# need to specify cairo device for transparency support in graphics
ggsave("spline_ggplot_t10008_manu.eps", device=cairo_ps)

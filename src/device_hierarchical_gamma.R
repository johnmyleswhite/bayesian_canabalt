library('ProjectTemplate')
load.project()

# Estimate a hierarchy of gamma distributions that produced the scores
# data over devices.
jags <- jags.model(file.path('jags', 'device_hierarchical_gamma.bug'),
                   data = list('score' = scores$score,
                               'device' = as.numeric(scores$device),
                               'N' = N,
                               'J' = J),
                   n.chains = 4,
                   n.adapt = 1000)
 
mcmc.samples <- coda.samples(jags,
                             c('shape', 'rate'),
                             10000)

# Estimate the model parameters using our samples.
credible.intervals <- data.frame()

for (i in 1:J)
{  
  estimated.values <- as.array(mcmc.samples[,paste('shape[', i, ']', sep = ''),])
  credible.interval <- as.numeric(quantile(estimated.values,
                                           prob = c(0.025, 0.975)))
  credible.intervals <- rbind(credible.intervals,
                              data.frame(Type = device.names[i],
                                         Parameter = 'Shape',
                                         Median = median(estimated.values),
                                         LowerBound = credible.interval[1],
                                         UpperBound = credible.interval[2]))

  estimated.values <- 1 / as.array(mcmc.samples[,paste('rate[', i, ']', sep = ''),])
  credible.interval <- as.numeric(quantile(estimated.values,
                                           prob = c(0.025, 0.975)))
  credible.intervals <- rbind(credible.intervals,
                              data.frame(Type = device.names[i],
                                         Parameter = 'Scale',
                                         Median = median(estimated.values),
                                         LowerBound = credible.interval[1],
                                         UpperBound = credible.interval[2]))
  
  estimated.values <- as.array(mcmc.samples[,paste('shape[', i, ']', sep = ''),]) * 1 / as.array(mcmc.samples[,paste('rate[', i, ']', sep = ''),])
  credible.interval <- as.numeric(quantile(estimated.values,
                                           prob = c(0.025, 0.975)))
  credible.intervals <- rbind(credible.intervals,
                              data.frame(Type = device.names[i],
                                         Parameter = 'Mean',
                                         Median = median(estimated.values),
                                         LowerBound = credible.interval[1],
                                         UpperBound = credible.interval[2]))

  estimated.values <- sqrt(as.array(mcmc.samples[,paste('shape[', i, ']', sep = ''),]) * 1 / as.array(mcmc.samples[,paste('rate[', i, ']', sep = ''),]) ^ 2)
  credible.interval <- as.numeric(quantile(estimated.values,
                                           prob = c(0.025, 0.975)))
  credible.intervals <- rbind(credible.intervals,
                              data.frame(Type = device.names[i],
                                         Parameter = 'Standard Deviation',
                                         Median = median(estimated.values),
                                         LowerBound = credible.interval[1],
                                         UpperBound = credible.interval[2]))
}

# Visualize the parameters and implied summary statistics.
png(file.path('graphs', 'device_gamma_1.png'))
p <- ggplot(subset(credible.intervals, Parameter %in% c('Mean', 'Standard Deviation')),
            aes(x = reorder(Type, Median),
                y = Median,
                color = Parameter)) +
  geom_point() +
  geom_pointrange(aes(ymin = LowerBound, ymax = UpperBound)) +
  coord_flip() +
  ylab('') +
  opts(title = 'Estimated score distribution in Canabalt across platforms') +
  xlab('') +
  facet_grid(Parameter ~ .) +
  scale_color_discrete(legend = FALSE)
print(p)
dev.off()

png(file.path('graphs', 'device_gamma_2.png'))
p <- ggplot(subset(credible.intervals, Parameter %in% c('Shape', 'Scale')),
            aes(x = reorder(Type, Median),
                y = Median,
                color = Parameter)) +
  geom_point() +
  geom_pointrange(aes(ymin = LowerBound, ymax = UpperBound)) +
  coord_flip() +
  ylab('') +
  opts(title = 'Estimated score distribution in Canabalt across platforms') +
  xlab('') +
  facet_grid(Parameter ~ .) +
  scale_color_discrete(legend = FALSE)
print(p)
dev.off()

library('ProjectTemplate')
load.project()

# Estimate a multinomial distribution that produced the deaths data.
jags <- jags.model(file.path('jags', 'multinomial.bug'),
                   data = list('deaths' = deaths,
                               'N' = N,
                               'K' = K),
                   n.chains = 4,
                   n.adapt = 1000)
 
mcmc.samples <- coda.samples(jags,
                             c('p'),
                             10000)

# Estimate the model parameters using our samples.
credible.intervals <- data.frame()

for (i in 1:K)
{
  estimated.values <- as.array(mcmc.samples[,paste('p[', i, ']', sep = ''),])
  credible.interval <- as.numeric(quantile(estimated.values,
                                           prob = c(0.025, 0.975)))
  credible.intervals <- rbind(credible.intervals,
                              data.frame(Type = death.type.names[i],
                                         Median = median(estimated.values),
                                         LowerBound = credible.interval[1],
                                         UpperBound = credible.interval[2]))
}

# Visualize the parameters.
pdf(file.path('graphs', 'death_type_probabilities.pdf'))
p <- ggplot(credible.intervals, aes(x = reorder(Type, Median), y = Median)) +
  geom_point() +
  geom_pointrange(aes(ymin = LowerBound, ymax = UpperBound)) +
  coord_flip() +
  ylim(c(0, 1)) +
  scale_y_log10() +
  ylab('') +
  opts(title = 'Estimated probability of dying in Canabalt because of...') +
  xlab('')
print(p)
dev.off()

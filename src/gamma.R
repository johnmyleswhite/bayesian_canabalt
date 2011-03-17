library('ProjectTemplate')
load.project()

# Estimate a gamma distribution that produced the scores data.
jags <- jags.model(file.path('jags', 'gamma.bug'),
                   data = list('score' = scores$score,
                               'N' = nrow(scores)),
                   n.chains = 4,
                   n.adapt = 1000)
 
mcmc.samples <- coda.samples(jags,
                             c('shape', 'rate'),
                             10000)

# Estimate the model parameters using our samples.
parameters <- apply(as.array(mcmc.samples), 2, mean)
shape <- parameters[['shape']]
rate <- parameters[['rate']]
scale <- 1 / parameters[['rate']]

# Using a K/S test, we can reject the gamma distribution.
ks.test(scores$score, 'pgamma', shape, rate)

# But visual inspection suggests the model is not so bad, except in the tails.
mean(scores$score) - shape * scale
sd(scores$score) - sqrt(shape * scale ^ 2)

comparison <- rbind(data.frame(Score = scores$score,
                               Source = 'Empirical'),
                    data.frame(Score = rgamma(500000, shape, rate),
                               Source = 'Gamma'))

png(file.path('graphs', 'density_comparison.png'))
ggplot(comparison, aes(x = Score, fill = Source)) +
  facet_grid(Source ~ .) +
  geom_density() +
  xlab('Score') +
  ylab('Estimated Density') +
  opts(title = 'Comparison between Empirical Data and Gamma Model')
dev.off()

png(file.path('graphs', 'tail_density_comparison.png'))
ggplot(comparison, aes(x = Score, fill = Source)) +
  facet_grid(Source ~ .) +
  geom_density() +
  scale_y_sqrt() +
  xlab('Score') +
  ylab('Estimated Density') +
  opts(title = 'Tail Comparison between Empirical Data and Gamma Model')
dev.off()

# What are the various ways players die?
death.type.names <- levels(scores$death)
death.types <- ddply(scores, 'death', nrow)
deaths <- death.types$V1

# Which iOS appear in our data?
device.names <- levels(scores$device)

# Set constants for all the JAGS code I've written.
N <- nrow(scores)
J <- length(device.names)
K <- length(death.type.names)

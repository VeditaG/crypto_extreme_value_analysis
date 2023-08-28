# This code is adapted from a post from Joel Schwartz on 14th Feb 2017 to Stackoverflow
# forum here:
# https://stackoverflow.com/questions/42229750/plot-lines-in-ggplot-from-a-list-of-dataframes
library(texmex)
library(tidyverse)
library(ggplot2)
library(extRemes)

N <- 10000
x <- seq(from = 0, to = 20, length.out=N)
sigma1 <- 5
sigma2 <- 10

gpd_dist_s1 <- list (
  data.frame(x = x, Density = pgpd(x, sigma = sigma1, xi=-2), Shape = '\u03c3=5, ξ=-2'), 
  data.frame(x = x, Density = pgpd(x, sigma = sigma1, xi=0), Shape = '\u03c3=5, ξ=0'), 
  data.frame(x = x, Density = pgpd(x, sigma = sigma1, xi=2), Shape = '\u03c3=5, ξ=2'))

gpd_dist_s2 <- list (
  data.frame(x = x, Density = pgpd(x, sigma = sigma2, xi=-2), Shape = '\u03c3=10, ξ=-2'),
  data.frame(x = x, Density = pgpd(x, sigma = sigma2, xi=0), Shape = '\u03c3=10, ξ=0'),
  data.frame(x = x, Density = pgpd(x, sigma = sigma2, xi=2), Shape = '\u03c3=10, ξ=2'))


ggplot(bind_rows(gpd_dist_s1), aes(x=x, color = Shape)) +
  geom_line(aes(y=Density), size = 1) +
  geom_line(data = gpd_dist_s2[[1]], aes(x=x, y=Density), size = 1) +
  geom_line(data = gpd_dist_s2[[2]], aes(x=x, y=Density), size = 1) +
  geom_line(data = gpd_dist_s2[[3]], aes(x=x, y=Density), size = 1) +
  geom_vline(xintercept = 0) +
  geom_vline(xintercept = 2.5, col = 'blue', linetype = 2) +
  geom_vline(xintercept = 5, col = 'black', linetype = 2) +
  geom_hline(yintercept = 0) +
  theme_classic() +
  scale_y_continuous(name = 'Density', breaks = NULL, expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0), limits = c(0, 20))

ggsave("gpd_shape.png")

#############################################################################
# Code adapted from R Documentation of the extRemes package (version 2.1-2):
# https://cran.r-project.org/web/packages/extRemes/index.html

# Author: Eric Gilleland
# Published On: 2022-05-04

# Load Fort data
# Katz, R. W., Parlange, M. B. and Naveau, P. (2002). Statistics of extremes in hydrology. Advances in Water Resources, 25, 1287–1304.
data(Fort)

# Plot the mean residual plot
png(file="mrlplot.png", width=900, height=600)

# opar <- par(no.readonly = TRUE)
# par(mar=c(5, 5, 4, 6))
par(cex=1.5)
abline(lm(mrlplot(Fort$Prec,
                  nint = 100,
                  alpha = 0.05,
                  xlab = "Threshold, u",
                  lwd = 1.5, bty = "l")[,"95% upper"] ~ seq(from = 0, to = 4, length.out=100)),
       col = 'blue', lty = 2, cex.axis = 2, cex.lab = 1.5)

legend("topleft", title = expression(bold("Legend")), 
       legend = c("95% Confidence Interval", "Mean Excess"),
       lty = c("dashed", "solid"), lwd = c(1, 1.5), col = c("grey", "black"), bty = "n")

# on.exit(par(opar))
dev.off()

# Parameter Stability Plot
par(col.main="#00000000")
par(col.lab="#00000000")
threshrange.plot(Fort$Prec, 
                 r = c(1, 2.5),
                 type = "GP",
                 nint = 10,
                 alpha = 0.05)
par(col.lab="black")
# title(main = "Graph")
mtext("Threshold, u", side = 1, line = 2, cex = 1.5)
mtext("Estimates of Shape Parameter", side = 2, line = 2, adj = -1.5)
mtext("Reparameterised Scale Estimates", side = 2, line = 2, adj = 3)
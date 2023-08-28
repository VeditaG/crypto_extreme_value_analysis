# Load libraries 
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(gridExtra)
library(biwavelet)

# Load image
load("image/AggregatedData.RData")

# Summary of all columns ----
summary(preCOVID_fileResults[[1]])
summary(postCOVID_fileResults[[1]])

summary(preCOVID_fileResults[[2]])
summary(postCOVID_fileResults[[2]])

summary(preCOVID_fileResults[[3]])
summary(postCOVID_fileResults[[3]])

# Check for presence of missing values ----
cbind(lapply(lapply(preCOVID_fileResults[[1]], is.na), sum))
cbind(lapply(lapply(preCOVID_fileResults[[2]], is.na), sum))
cbind(lapply(lapply(preCOVID_fileResults[[3]], is.na), sum))

cbind(lapply(lapply(postCOVID_fileResults[[1]], is.na), sum))
cbind(lapply(lapply(postCOVID_fileResults[[2]], is.na), sum))
cbind(lapply(lapply(postCOVID_fileResults[[3]], is.na), sum))
# None found

# Add per- or post-era col
for (i in 1:length(fileResults)){
  preCOVID_fileResults[[i]] <- preCOVID_fileResults[[i]] %>%
    add_column(period = "Pre-COVID19")
  postCOVID_fileResults[[i]] <- postCOVID_fileResults[[i]] %>%
    add_column(period = "Post-COVID19")
}
head(preCOVID_fileResults[[3]])
head(postCOVID_fileResults[[3]])

# Combine in 1 dataframe ----
combined <- bind_rows(preCOVID_fileResults[[1]], preCOVID_fileResults[[2]], 
                      preCOVID_fileResults[[3]],postCOVID_fileResults[[1]], 
                      postCOVID_fileResults[[2]], postCOVID_fileResults[[3]])
combined <- combined %>%
  mutate(Currency = case_when(file == "Bitcoin-data.xlsx" ~ "Bitcoin",
                              file == "Dodgecoin-data.xlsx" ~ "Dodgecoin",
                              TRUE ~ "Ethereum"))

head(combined)

bitcoin_combined <- combined[combined$Currency == "Bitcoin", ]
ethereum_combined <- combined[combined$Currency == "Ethereum", ]
dogecoin_combined <- combined[combined$Currency == "Dodgecoin", ]

# Explore High only ----
p1 <- ggplot(data = bitcoin_combined, aes(x = reorder(factor(period)), y = High, group = factor(period))) +
  geom_boxplot(aes(fill = factor(period))) +
  scale_fill_manual(name = "Period",
                    values = c("skyblue", "grey"),
                    labels = c("Pre-COVID19", "Post-COVID19")) +
  labs(y = "Highest daily  price (USD)",
       x = "Period", title = "Boxplots of Highest Daily Prices", subtitle = "Bitcoin") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(size = 10, face = "bold"),
        plot.subtitle = element_text(size = 10),
        axis.title = element_text(size = 10),
        legend.position = "none")

p2 <- ggplot(data = ethereum_combined, aes(x = factor(period), y = High, group = factor(period))) +
  geom_boxplot(aes(fill = factor(period))) +
  scale_fill_manual(name = "Period",
                    values = c("skyblue", "grey"),
                    labels = c("Pre-COVID19", "Post-COVID19")) +
  labs(y = "Highest daily  price (USD)",
       x = "Period", title = " ", subtitle = "Ethereum") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        plot.subtitle = element_text(size = 10),
        axis.title = element_text(size = 10),
        legend.position = "none")

p3 <- ggplot(data = dogecoin_combined, aes(x = factor(period), y = High, group = factor(period))) +
  geom_boxplot(aes(fill = factor(period))) +
  scale_fill_manual(name = "Period",
                    values = c("skyblue", "grey"),
                    labels = c("Pre-COVID19", "Post-COVID19")) +
  labs(y = "Highest daily  price (USD)",
       x = "Period", title = " ", subtitle = "Dogecoin") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        plot.subtitle = element_text(size = 10),
        axis.title = element_text(size = 10),
        legend.position = "none")
ggarrange(p1, p2, p3,
          nrow = 1, ncol = 3, common.legend = TRUE, legend="bottom")

# Boxplots ----
# Open
p1 <- ggplot(data = combined, aes(x = period, y = Open, group = period)) +
  geom_boxplot(aes(fill = period)) +
  labs(y = "Opening price (USD)",
       x = "Period") +
  facet_grid(Currency~., scales = "free") +
  theme_light() +
  theme(axis.text = element_text(size = 8))

# High
p2 <- ggplot(data = combined, aes(x = period, y = High, group = period)) +
  geom_boxplot(aes(fill = period)) +
  labs(y = "Highest daily  price (USD)",
       x = "Period") +
  facet_grid(.~Currency, scales = "free") +
  theme_light() +
  theme(axis.text = element_text(size = 8))

# Low
p3 <- ggplot(data = combined, aes(x = period, y = Low, group = period)) +
  geom_boxplot(aes(fill = period)) +
  labs(y = "Lowest daily price (USD)",
       x = "Period") +
  facet_grid(Currency~., scales = "free") +
  theme_light() +
  theme(axis.text = element_text(size = 8))

# Close
p4 <- ggplot(data = combined, aes(x = period, y = Close, group = period)) +
  geom_boxplot(aes(fill = period)) +
  labs(y = "Closing price (USD)",
       x = "Period") +
  facet_grid(Currency~., scales = "free") +
  theme_light() +
  theme(axis.text = element_text(size = 8))

# Volume
p5 <- ggplot(data = combined, aes(x = period, y = Close, group = period)) +
  geom_boxplot(aes(fill = period)) +
  labs(y = "Volume of trades",
       x = "Period") +
  facet_grid(Currency~., scales = "free") +
  theme_light() +
  theme(axis.text = element_text(size = 8))

# Market Cap
p6 <- ggplot(data = combined, aes(x = period, y = Close, group = period)) +
  geom_boxplot(aes(fill = period)) +
  labs(y = "Total Market Capitalisation (USD)",
       x = "Period") +
  facet_grid(Currency~., scales = "free") +
  theme_light() +
  theme(axis.text = element_text(size = 8))

ggarrange(p1, p2, p3,
          p4, p5, p6, 
          nrow = 2, ncol = 3, common.legend = TRUE, legend="bottom")


# Fixed-effect ANOVA ----
all_bitcoin <- bind_rows(preCOVID_fileResults[[1]],postCOVID_fileResults[[1]]) %>% 
  select(-c("Year", "Date", "file"))
all_ethereum <- bind_rows(preCOVID_fileResults[[2]],postCOVID_fileResults[[2]])%>% 
  select(-c("Year", "Date", "file"))
all_dodgecoin <- bind_rows(preCOVID_fileResults[[3]],postCOVID_fileResults[[3]])%>% 
  select(-c("Year", "Date", "file"))

# Fit the model 
fixEff.anova.bitcoin <- aov(period ~., data = all_bitcoin)
fixEff.anova.ethereum <- aov(period ~., data = all_ethereum)
fixEff.anova.dodgecoin <- aov(period ~., data = all_dodgecoin)

summary(fixEff.anova.bitcoin)
summary(fixEff.anova.ethereum)
summary(fixEff.anova.dodgecoin)

# Wavelet Coherence Analysis ----
# Combine pre-covid data
combined_pre <- bind_rows(preCOVID_fileResults[[1]], 
                          preCOVID_fileResults[[2]],
                          preCOVID_fileResults[[3]])

combined_pre <- combined_pre %>%
  mutate(Currency = case_when(file == "Bitcoin-data.xlsx" ~ "Bitcoin",
                              file == "Dodgecoin-data.xlsx" ~ "Dodgecoin",
                              TRUE ~ "Ethereum"))

#var <- c("Open", "High", "Low",
#         "Close", "Volume", "MarketCap")

#fileName <- c("Bitcoin-data.xlsx", "Dodgecoin-data.xlsx", "Ethereum-data.xlsx")

# Create an empty list for results
#lst_byVar <- vector(mode = "list", length = length(var))
#lst_byCur <- vector(mode = "list", length = length(file))

#for (i in 1:length(file)){
#  lst_byCur[[i]] <- append(lst_byCur, lst_byVar)
#  
#  for (j in 1:length(var)){
#    lst_byVar[[j]] <- combined_pre %>% 
#     filter(file == fileName[i]) %>% 
#      select(var[j])
#  }
#}

combined_pre_bitcoin_high <- combined_pre[which(combined_pre$Currency == "Bitcoin"),
                                          names(combined_pre) %in% c("High")]
combined_pre_ethereum_high <- combined_pre[which(combined_pre$Currency == "Ethereum"),
                                           names(combined_pre) %in% c("High")]
combined_pre_dodge_high <- combined_pre[which(combined_pre$Currency == "Dodgecoin"),
                                        names(combined_pre) %in% c("High")]

period <- 1:nrow(preCOVID_fileResults[[1]])
PRBH <- cbind(period, combined_pre_bitcoin_high)
PREH <- cbind(period, combined_pre_ethereum_high)
PRDH <- cbind(period, combined_pre_dodge_high)

# Combine post-covid data
combined_post <- bind_rows(postCOVID_fileResults[[1]], 
                           postCOVID_fileResults[[2]],
                           postCOVID_fileResults[[3]])
combined_post <- combined_post %>%
  mutate(Currency = case_when(file == "Bitcoin-data.xlsx" ~ "Bitcoin",
                              file == "Dodgecoin-data.xlsx" ~ "Dodgecoin",
                              TRUE ~ "Ethereum"))

combined_post_bitcoin_high <- combined_post[which(combined_post$Currency == "Bitcoin"),
                                            names(combined_post) %in% c("High")]
combined_post_ethereum_high <- combined_post[which(combined_post$Currency == "Ethereum"),
                                             names(combined_post) %in% c("High")]
combined_post_dodge_high <- combined_post[which(combined_post$Currency == "Dodgecoin"),
                                          names(combined_post) %in% c("High")]

post_period <- 1:nrow(postCOVID_fileResults[[1]])
POBH <- cbind(post_period, combined_post_bitcoin_high)
POEH <- cbind(post_period, combined_post_ethereum_high)
PODH <- cbind(post_period, combined_post_dodge_high)

# Monte Carlo Sim
wtc.POPRBH <- wtc(PRBH[518:731,], POBH[1:214,], nrands = 10) 
wtc.POPREH <- wtc(PREH[518:731,], POEH[1:214,], nrands = 10) 
wtc.POPRDH <- wtc(PRDH[518:731,], PODH[1:214,], nrands = 10) 

# Plot
par(oma = c(0, 0, 0, 1), mar = c(5, 4, 4, 8) + 0.0001)
par(mfrow = c(3, 1))
#layout(matrix(c(1,2,2,3,3), nrow = 3, ncol = 1, byrow = TRUE))
plot(wtc.POPRBH, plot.phase = TRUE, 
     lty.coi = 1, col.coi = "grey", lwd.coi = 2, 
     lwd.sig = 2, arrow.lwd = 0.03, arrow.len = 0.12, 
     xlab = "Time Period", ylab = "Frequency (Hz)", 
     main = list("Bitcoin", font = 1),
     cex = 0.8,
     plot.cb = TRUE)

plot(wtc.POPREH, plot.phase = TRUE, 
     lty.coi = 1, col.coi = "grey", lwd.coi = 2, 
     lwd.sig = 2, arrow.lwd = 0.03, arrow.len = 0.12, 
     xlab = "Time Period", ylab = "Frequency (Hz)", 
     main = list("Ethereum", font = 1),
     cex = 0.8,
     plot.cb = TRUE)

plot(wtc.POPRDH, plot.phase = TRUE, 
     lty.coi = 1, col.coi = "grey", lwd.coi = 2, 
     lwd.sig = 2, arrow.lwd = 0.03, arrow.len = 0.12, 
     xlab = "Time Period", ylab = "Frequency (Hz)", 
     main = list("Dodgecoin", font = 1),
     cex = 0.8,
     plot.cb = TRUE)

# Set up plot area
# High ----
plot.new()
frame()
nf <- layout(matrix(c(1,2,3),ncol=3), widths=c(3,3,3), heights=c(3,3,3), TRUE) 
par(cex.axis=0.6,cex.lab=0.9,cex=0.8)

acf(preCOVID_fileResults[[1]]$High,
    main = "")
title(main = list("Bitcoin", font = 1), adj = 0)
acf(preCOVID_fileResults[[2]]$High,
    main = "")
title(main = list("Dodgecoin", font = 1), adj = 0)
acf(preCOVID_fileResults[[3]]$High,
    main = "")
title(main = list("Ethereum", font = 1), adj = 0)
mtext("Pre-COVID19 Period - Highest Price in Last 24-hour",
      side = 3,
      line = - 2,
      outer = TRUE)

plot.new()
frame()
nf <- layout(matrix(c(1,2,3),ncol=3), widths=c(3,3,3), heights=c(3,3,3), TRUE) 
par(cex.axis=0.6,cex.lab=0.9,cex=0.8)

acf(postCOVID_fileResults[[1]]$High,
    main = "")
title(main = list("Bitcoin", font = 1), adj = 0)
acf(postCOVID_fileResults[[2]]$High,
    main = "")
title(main = list("Dodgecoin", font = 1), adj = 0)
acf(postCOVID_fileResults[[3]]$High,
    main = "")
title(main = list("Ethereum", font = 1), adj = 0)
mtext("Post-COVID19 Period - Highest Price in Last 24-hour",
      side = 3,
      line = - 2,
      outer = TRUE)

plot.new()
frame()
nf <- layout(matrix(c(1,2,3),ncol=3), widths=c(3,3,3), heights=c(3,3,3), TRUE) 
par(cex.axis=0.6,cex.lab=0.9,cex=0.8)

pacf(preCOVID_fileResults[[1]]$High,
     main = "")
title(main = list("Bitcoin", font = 1), adj = 0)
pacf(preCOVID_fileResults[[2]]$High,
     main = "")
title(main = list("Dodgecoin", font = 1), adj = 0)
pacf(preCOVID_fileResults[[3]]$High,
     main = "")
title(main = list("Ethereum", font = 1), adj = 0)
mtext("Pre-COVID19 Period - Highest Price in Last 24-hour",
      side = 3,
      line = - 2,
      outer = TRUE)

plot.new()
frame()
nf <- layout(matrix(c(1,2,3),ncol=3), widths=c(3,3,3), heights=c(3,3,3), TRUE) 
par(cex.axis=0.6,cex.lab=0.9,cex=0.8)

pacf(postCOVID_fileResults[[1]]$High,
     main = "")
title(main = list("Bitcoin", font = 1), adj = 0)
pacf(postCOVID_fileResults[[2]]$High,
     main = "")
title(main = list("Dodgecoin", font = 1), adj = 0)
pacf(postCOVID_fileResults[[3]]$High,
     main = "")
title(main = list("Ethereum", font = 1), adj = 0)
mtext("Post-COVID19 Period - Highest Price in Last 24-hour",
      side = 3,
      line = - 2,
      outer = TRUE)
# Open ----
plot.new()
frame()
nf <- layout(matrix(c(1,2,3),ncol=3), widths=c(3,3,3), heights=c(3,3,3), TRUE) 
par(cex.axis=0.6,cex.lab=0.9,cex=0.8)

acf(preCOVID_fileResults[[1]]$Open,
    main = "")
title(main = list("Bitcoin", font = 1), adj = 0)
acf(preCOVID_fileResults[[2]]$Open,
    main = "")
title(main = list("Dodgecoin", font = 1), adj = 0)
acf(preCOVID_fileResults[[3]]$Open,
    main = "")
title(main = list("Ethereum", font = 1), adj = 0)
mtext("Pre-COVID19 Period - Opening Price",
      side = 3,
      line = - 2,
      outer = TRUE)

plot.new()
frame()
nf <- layout(matrix(c(1,2,3),ncol=3), widths=c(3,3,3), heights=c(3,3,3), TRUE) 
par(cex.axis=0.6,cex.lab=0.9,cex=0.8)

acf(postCOVID_fileResults[[1]]$Open,
    main = "")
title(main = list("Bitcoin", font = 1), adj = 0)
acf(postCOVID_fileResults[[2]]$Open,
    main = "")
title(main = list("Dodgecoin", font = 1), adj = 0)
acf(postCOVID_fileResults[[3]]$Open,
    main = "")
title(main = list("Ethereum", font = 1), adj = 0)
mtext("Post-COVID19 Period - Opening Price",
      side = 3,
      line = - 2,
      outer = TRUE)

plot.new()
frame()
nf <- layout(matrix(c(1,2,3),ncol=3), widths=c(3,3,3), heights=c(3,3,3), TRUE) 
par(cex.axis=0.6,cex.lab=0.9,cex=0.8)

pacf(preCOVID_fileResults[[1]]$Open,
     main = "")
title(main = list("Bitcoin", font = 1), adj = 0)
pacf(preCOVID_fileResults[[2]]$Open,
     main = "")
title(main = list("Dodgecoin", font = 1), adj = 0)
pacf(preCOVID_fileResults[[3]]$Open,
     main = "")
title(main = list("Ethereum", font = 1), adj = 0)
mtext("Pre-COVID19 Period - Opening Price",
      side = 3,
      line = - 2,
      outer = TRUE)

plot.new()
frame()
nf <- layout(matrix(c(1,2,3),ncol=3), widths=c(3,3,3), heights=c(3,3,3), TRUE) 
par(cex.axis=0.6,cex.lab=0.9,cex=0.8)

pacf(postCOVID_fileResults[[1]]$Open,
     main = "")
title(main = list("Bitcoin", font = 1), adj = 0)
pacf(postCOVID_fileResults[[2]]$Open,
     main = "")
title(main = list("Dodgecoin", font = 1), adj = 0)
pacf(postCOVID_fileResults[[3]]$Open,
     main = "")
title(main = list("Ethereum", font = 1), adj = 0)
mtext("Post-COVID19 Period - Opening Price",
      side = 3,
      line = - 2,
      outer = TRUE)
# Low ----
plot.new()
frame()
nf <- layout(matrix(c(1,2,3),ncol=3), widths=c(3,3,3), heights=c(3,3,3), TRUE) 
par(cex.axis=0.6,cex.lab=0.9,cex=0.8)

acf(preCOVID_fileResults[[1]]$Low,
    main = "")
title(main = list("Bitcoin", font = 1), adj = 0)
acf(preCOVID_fileResults[[2]]$Low,
    main = "")
title(main = list("Dodgecoin", font = 1), adj = 0)
acf(preCOVID_fileResults[[3]]$Low,
    main = "")
title(main = list("Ethereum", font = 1), adj = 0)
mtext("Pre-COVID19 Period - Lowest Price in Last 24-hour",
      side = 3,
      line = - 2,
      outer = TRUE)

plot.new()
frame()
nf <- layout(matrix(c(1,2,3),ncol=3), widths=c(3,3,3), heights=c(3,3,3), TRUE) 
par(cex.axis=0.6,cex.lab=0.9,cex=0.8)

acf(postCOVID_fileResults[[1]]$Low,
    main = "")
title(main = list("Bitcoin", font = 1), adj = 0)
acf(postCOVID_fileResults[[2]]$Low,
    main = "")
title(main = list("Dodgecoin", font = 1), adj = 0)
acf(postCOVID_fileResults[[3]]$Low,
    main = "")
title(main = list("Ethereum", font = 1), adj = 0)
mtext("Post-COVID19 Period - Lowest Price in Last 24-hour",
      side = 3,
      line = - 2,
      outer = TRUE)

plot.new()
frame()
nf <- layout(matrix(c(1,2,3),ncol=3), widths=c(3,3,3), heights=c(3,3,3), TRUE) 
par(cex.axis=0.6,cex.lab=0.9,cex=0.8)

pacf(preCOVID_fileResults[[1]]$Low,
     main = "")
title(main = list("Bitcoin", font = 1), adj = 0)
pacf(preCOVID_fileResults[[2]]$Low,
     main = "")
title(main = list("Dodgecoin", font = 1), adj = 0)
pacf(preCOVID_fileResults[[3]]$Low,
     main = "")
title(main = list("Ethereum", font = 1), adj = 0)
mtext("Pre-COVID19 Period - Lowest Price in Last 24-hour",
      side = 3,
      line = - 2,
      outer = TRUE)

plot.new()
frame()
nf <- layout(matrix(c(1,2,3),ncol=3), widths=c(3,3,3), heights=c(3,3,3), TRUE) 
par(cex.axis=0.6,cex.lab=0.9,cex=0.8)

pacf(postCOVID_fileResults[[1]]$Low,
     main = "")
title(main = list("Bitcoin", font = 1), adj = 0)
pacf(postCOVID_fileResults[[2]]$Low,
     main = "")
title(main = list("Dodgecoin", font = 1), adj = 0)
pacf(postCOVID_fileResults[[3]]$Low,
     main = "")
title(main = list("Ethereum", font = 1), adj = 0)
mtext("Post-COVID19 Period - Lowest Price in Last 24-hour",
      side = 3,
      line = - 2,
      outer = TRUE)
# Close ----
plot.new()
frame()
nf <- layout(matrix(c(1,2,3),ncol=3), widths=c(3,3,3), heights=c(3,3,3), TRUE) 
par(cex.axis=0.6,cex.lab=0.9,cex=0.8)

acf(preCOVID_fileResults[[1]]$Close,
    main = "")
title(main = list("Bitcoin", font = 1), adj = 0)
acf(preCOVID_fileResults[[2]]$Close,
    main = "")
title(main = list("Dodgecoin", font = 1), adj = 0)
acf(preCOVID_fileResults[[3]]$Close,
    main = "")
title(main = list("Ethereum", font = 1), adj = 0)
mtext("Pre-COVID19 Period - Closing Price",
      side = 3,
      line = - 2,
      outer = TRUE)

plot.new()
frame()
nf <- layout(matrix(c(1,2,3),ncol=3), widths=c(3,3,3), heights=c(3,3,3), TRUE) 
par(cex.axis=0.6,cex.lab=0.9,cex=0.8)

acf(postCOVID_fileResults[[1]]$Close,
    main = "")
title(main = list("Bitcoin", font = 1), adj = 0)
acf(postCOVID_fileResults[[2]]$Close,
    main = "")
title(main = list("Dodgecoin", font = 1), adj = 0)
acf(postCOVID_fileResults[[3]]$Close,
    main = "")
title(main = list("Ethereum", font = 1), adj = 0)
mtext("Post-COVID19 Period - Closing Price",
      side = 3,
      line = - 2,
      outer = TRUE)

plot.new()
frame()
nf <- layout(matrix(c(1,2,3),ncol=3), widths=c(3,3,3), heights=c(3,3,3), TRUE) 
par(cex.axis=0.6,cex.lab=0.9,cex=0.8)

pacf(preCOVID_fileResults[[1]]$Close,
     main = "")
title(main = list("Bitcoin", font = 1), adj = 0)
pacf(preCOVID_fileResults[[2]]$Close,
     main = "")
title(main = list("Dodgecoin", font = 1), adj = 0)
pacf(preCOVID_fileResults[[3]]$Close,
     main = "")
title(main = list("Ethereum", font = 1), adj = 0)
mtext("Pre-COVID19 Period - Closing Price",
      side = 3,
      line = - 2,
      outer = TRUE)

plot.new()
frame()
nf <- layout(matrix(c(1,2,3),ncol=3), widths=c(3,3,3), heights=c(3,3,3), TRUE) 
par(cex.axis=0.6,cex.lab=0.9,cex=0.8)

pacf(postCOVID_fileResults[[1]]$Close,
     main = "")
title(main = list("Bitcoin", font = 1), adj = 0)
pacf(postCOVID_fileResults[[2]]$Close,
     main = "")
title(main = list("Dodgecoin", font = 1), adj = 0)
pacf(postCOVID_fileResults[[3]]$Close,
     main = "")
title(main = list("Ethereum", font = 1), adj = 0)
mtext("Post-COVID19 Period - Closing Price",
      side = 3,
      line = - 2,
      outer = TRUE)

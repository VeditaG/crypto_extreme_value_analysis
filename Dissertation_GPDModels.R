library(tidyverse)
library(extRemes)
# library(ggpubr)
# library(gridExtra)

# Load image
load("ModellingData.RData")

# mean residual life plot - Neg log ----
par(mfrow = c(2, 3))

extRemes::mrlplot(na.omit(preCOVID_stationary_neg[[1]]$diff_price))
title(main = "Mean Residual Life Plots - PreCOVID19", font.main = 2, adj = 0, line = 2.5)
title(main = "Bitcoin", font.main = 3, adj = 0, cex.main = 1, line = 1)

extRemes::mrlplot(na.omit(preCOVID_stationary_neg[[2]]$diff_price))
title(main = "Dodgecoin", font.main = 3, adj = 0, cex.main = 1, line = 1)

extRemes::mrlplot(na.omit(preCOVID_stationary_neg[[3]]$diff_price))
title(main = "Ethereum", font.main = 3, adj = 0, cex.main = 1, line = 1)

extRemes::mrlplot(na.omit(postCOVID_stationary_neg[[1]]$diff_price))
title(main = "Mean Residual Life Plots - PostCOVID19", font.main = 2, adj = 0, line = 2.5)
title(main = "Bitcoin", font.main = 3, adj = 0, cex.main = 1, line = 1)

extRemes::mrlplot(na.omit(postCOVID_stationary_neg[[2]]$diff_price))
title(main = "Dodgecoin", font.main = 3, adj = 0, cex.main = 1, line = 1)

extRemes::mrlplot(na.omit(postCOVID_stationary_neg[[3]]$diff_price))
title(main = "Ethereum", font.main = 3, adj = 0, cex.main = 1, line = 1)

dev.off()

# mean residual life plot - Pos log ----
par(mfrow = c(2, 3))

extRemes::mrlplot(na.omit(preCOVID_stationary_pos[[1]]$diff_price))
title(main = "Mean Residual Life Plots - PreCOVID19", font.main = 2, adj = 0, line = 2.5)
title(main = "Bitcoin", font.main = 3, adj = 0, cex.main = 1, line = 1)

extRemes::mrlplot(na.omit(preCOVID_stationary_pos[[2]]$diff_price))
title(main = "Dodgecoin", font.main = 3, adj = 0, cex.main = 1, line = 1)

extRemes::mrlplot(na.omit(preCOVID_stationary_pos[[3]]$diff_price))
title(main = "Ethereum", font.main = 3, adj = 0, cex.main = 1, line = 1)

extRemes::mrlplot(na.omit(postCOVID_stationary_pos[[1]]$diff_price))
title(main = "Mean Residual Life Plots - PostCOVID19", font.main = 2, adj = 0, line = 2.5)
title(main = "Bitcoin", font.main = 3, adj = 0, cex.main = 1, line = 1)

extRemes::mrlplot(na.omit(postCOVID_stationary_pos[[2]]$diff_price))
title(main = "Dodgecoin", font.main = 3, adj = 0, cex.main = 1, line = 1)

extRemes::mrlplot(na.omit(postCOVID_stationary_pos[[3]]$diff_price))
title(main = "Ethereum", font.main = 3, adj = 0, cex.main = 1, line = 1)

dev.off()

# MLE ----
pre_neg <- NULL
pre_pos <- NULL
post_neg <- NULL
post_pos <- NULL

pre_neg_model <- list()
pre_pos_model <- list()
post_neg_model <- list()
post_pos_model <- list()

for (u in 1:10){
  for (i in 1:3){
    m <- fevd(as.vector(na.omit(preCOVID_stationary_neg[[i]]$diff_price)), 
              method = "MLE", 
              type="GP", 
              threshold = u)
    pre_neg_model <- append(pre_neg_model, list(m))
    s <- summary(m, silent = TRUE)
    pre_neg <- rbind(pre_neg, data.frame(u, head(na.omit(preCOVID_stationary_neg[[i]]$file),1),
                                         s$par[[1]], s$par[[2]], pre_neg_model[[i]]$n, 
                                         sum(pre_neg_model[[i]]$x > pre_neg_model[[i]]$threshold, na.rm = TRUE),
                                         s$AIC))
  }
}

for (u in 1:10){
  for (i in 1:3){
    m <- fevd(as.vector(na.omit(preCOVID_stationary_pos[[i]]$diff_price)), 
              method = "MLE", 
              type="GP", 
              threshold = u)
    pre_pos_model <- append(pre_pos_model, list(m))
    s <- summary(m, silent = TRUE)
    pre_pos <- rbind(pre_pos, data.frame(u, head(na.omit(preCOVID_stationary_pos[[i]]$file),1),
                                         s$par[[1]], s$par[[2]], pre_pos_model[[i]]$n, 
                                         sum(pre_pos_model[[i]]$x > pre_pos_model[[i]]$threshold, na.rm = TRUE),
                                         s$AIC))
  }
}

for (u in 1:10){
  for (i in 1:3){
    m <- fevd(as.vector(na.omit(postCOVID_stationary_neg[[i]]$diff_price)), 
              method = "MLE", 
              type="GP", 
              threshold = u)
    post_neg_model <- append(post_neg_model, list(m))
    s <- summary(m, silent = TRUE)
    post_neg <- rbind(post_neg, data.frame(u, head(na.omit(postCOVID_stationary_neg[[i]]$file),1),
                                           s$par[[1]], s$par[[2]], post_neg_model[[i]]$n, 
                                           sum(post_neg_model[[i]]$x > post_neg_model[[i]]$threshold, na.rm = TRUE),
                                           s$AIC))
  }
}

for (u in 1:10){
  for (i in 1:3){
    m <- fevd(as.vector(na.omit(postCOVID_stationary_pos[[i]]$diff_price)), 
              method = "MLE", 
              type="GP", 
              threshold = u)
    post_pos_model <- append(post_pos_model, list(m))
    s <- summary(m, silent = TRUE)
    post_pos <- rbind(post_pos, data.frame(u, head(na.omit(postCOVID_stationary_pos[[i]]$file),1),
                                           s$par[[1]], s$par[[2]], post_pos_model[[i]]$n, 
                                           sum(post_pos_model[[i]]$x > post_pos_model[[i]]$threshold, na.rm = TRUE),
                                           s$AIC))
  }
}

colName <- c("u", "Crypto", "Scale", "Shape", "N", "n", "AIC")
colnames(pre_neg) <- colName
colnames(pre_pos) <- colName
colnames(post_neg) <- colName
colnames(post_pos) <- colName

head(pre_neg)

# save.image("model_param.RData")



# Reload image
load("model_param.RData")

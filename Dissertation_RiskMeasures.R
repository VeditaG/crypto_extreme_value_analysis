library(tidyverse)
library(extRemes)
library(patchwork)

# Load image
load("model_param.RData")

# VaR ----
df.list <- list(pre_neg, pre_pos, post_neg, post_pos)
alpha <- c(0.9, 0.95, 0.99)

# Nested loop to iterate through df.list and alpha
for (i in seq_along(df.list)) {
  df <- df.list[[i]]
  for (j in seq_along(alpha)) {
    current_alpha <- alpha[j]
    VaR <- paste0("var_", current_alpha)
    ES <- paste0("es_", current_alpha)
    df[VaR] <- df$u + (df$Scale / df$Shape)*((((df$n / df$N)*(1-current_alpha))^df$Shape) - 1)
    df[ES] <- ((df$u + (df$Scale / df$Shape)*((((df$n / df$N)*(1-current_alpha))^df$Shape) - 1)) / (1 - df$Shape)) + ((df$Scale - df$Shape * df$u) / (1 - df$Shape))
  }
  df.list[[i]] <- df
}

# save image
save.image("riskMeasures.RData")

# Reload image
load("riskMeasures.RData")

# Plot ----
# VaR pre- vs post-neg ----
p1 <- ggplot(df.list[[1]],
             aes(u, var_0.9, color = factor(Crypto))) +
  geom_line() + 
  labs(x = "Threshold", y = "VaR at p=0.9", title = "Pre-COVID19 Period") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.title = element_text(size = 10),
        legend.position = "none") +
  scale_color_manual(labels = c("Bitcoin", "Dodgecoin", "Ethereum"), values = c("red", "green", "blue"))

p2 <- ggplot(df.list[[1]],
             aes(u, var_0.95, color = factor(Crypto))) +
  geom_line() + 
  labs(x = "Threshold", y = "VaR at p=0.95") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.title = element_text(size = 10),
        legend.position = "none") +
  scale_color_manual(labels = c("Bitcoin", "Dodgecoin", "Ethereum"), values = c("red", "green", "blue"))

p3 <- ggplot(df.list[[1]],
             aes(u, var_0.99, color = factor(Crypto))) +
  geom_line() + 
  labs(x = "Threshold", y = "VaR at p=0.99") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.title = element_text(size = 10),
        legend.position = "none") +
  scale_color_manual(labels = c("Bitcoin", "Dodgecoin", "Ethereum"), values = c("red", "green", "blue"))

p4 <- ggplot(df.list[[3]],
             aes(u, var_0.9, color = factor(Crypto))) +
  geom_line() + 
  labs(x = "Threshold", y = "VaR at p=0.9", title = "Post-COVID19 Period") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.title = element_text(size = 10),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 8)) +
  scale_color_manual(labels = c("Bitcoin", "Dodgecoin", "Ethereum"), values = c("red", "green", "blue"))

p5 <- ggplot(df.list[[3]],
             aes(u, var_0.95, color = factor(Crypto))) +
  geom_line() + 
  labs(x = "Threshold", y = "VaR at p=0.95") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.title = element_text(size = 10),
        legend.position = "none") +
  scale_color_manual(labels = c("Bitcoin", "Dodgecoin", "Ethereum"), values = c("red", "green", "blue"))

p6 <- ggplot(df.list[[3]],
             aes(u, var_0.99, color = factor(Crypto))) +
  geom_line() + 
  labs(x = "Threshold", y = "VaR at p=0.99") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.title = element_text(size = 10),
        legend.position = "none") +
  scale_color_manual(labels = c("Bitcoin", "Dodgecoin", "Ethereum"), values = c("red", "green", "blue"))

p1 + p2 + p3 + p4 + p5 + p6 + plot_layout(nrow = 2)

# VaR pre- vs post-pos ----
p1 <- ggplot(df.list[[2]],
             aes(u, var_0.9, color = factor(Crypto))) +
  geom_line() + 
  labs(x = "Threshold", y = "VaR at p=0.9", title = "Pre-COVID19 Period") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.title = element_text(size = 10),
        legend.position = "none") +
  scale_color_manual(labels = c("Bitcoin", "Dodgecoin", "Ethereum"), values = c("red", "green", "blue"))

p2 <- ggplot(df.list[[2]],
             aes(u, var_0.95, color = factor(Crypto))) +
  geom_line() + 
  labs(x = "Threshold", y = "VaR at p=0.95") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.title = element_text(size = 10),
        legend.position = "none") +
  scale_color_manual(labels = c("Bitcoin", "Dodgecoin", "Ethereum"), values = c("red", "green", "blue"))

p3 <- ggplot(df.list[[2]],
             aes(u, var_0.99, color = factor(Crypto))) +
  geom_line() + 
  labs(x = "Threshold", y = "VaR at p=0.99") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.title = element_text(size = 10),
        legend.position = "none") +
  scale_color_manual(labels = c("Bitcoin", "Dodgecoin", "Ethereum"), values = c("red", "green", "blue"))

p4 <- ggplot(df.list[[4]],
             aes(u, var_0.9, color = factor(Crypto))) +
  geom_line() + 
  labs(x = "Threshold", y = "VaR at p=0.9", title = "Post-COVID19 Period") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.title = element_text(size = 10),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 8)) +
  scale_color_manual(labels = c("Bitcoin", "Dodgecoin", "Ethereum"), values = c("red", "green", "blue"))

p5 <- ggplot(df.list[[4]],
             aes(u, var_0.95, color = factor(Crypto))) +
  geom_line() + 
  labs(x = "Threshold", y = "VaR at p=0.95") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.title = element_text(size = 10),
        legend.position = "none") +
  scale_color_manual(labels = c("Bitcoin", "Dodgecoin", "Ethereum"), values = c("red", "green", "blue"))

p6 <- ggplot(df.list[[4]],
             aes(u, var_0.99, color = factor(Crypto))) +
  geom_line() + 
  labs(x = "Threshold", y = "VaR at p=0.99") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.title = element_text(size = 10),
        legend.position = "none") +
  scale_color_manual(labels = c("Bitcoin", "Dodgecoin", "Ethereum"), values = c("red", "green", "blue"))

p1 + p2 + p3 + p4 + p5 + p6 + plot_layout(nrow = 2)

# CVaR pre- vs post-neg ----
p1 <- ggplot(df.list[[1]],
             aes(u, es_0.9, color = factor(Crypto))) +
  geom_line() + 
  labs(x = "Threshold", y = "ES at p=0.9", title = "Pre-COVID19 Period") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.title = element_text(size = 10),
        legend.position = "none") +
  scale_color_manual(labels = c("Bitcoin", "Dodgecoin", "Ethereum"), values = c("red", "green", "blue"))

p2 <- ggplot(df.list[[1]],
             aes(u, es_0.95, color = factor(Crypto))) +
  geom_line() + 
  labs(x = "Threshold", y = "ES at p=0.95") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.title = element_text(size = 10),
        legend.position = "none") +
  scale_color_manual(labels = c("Bitcoin", "Dodgecoin", "Ethereum"), values = c("red", "green", "blue"))

p3 <- ggplot(df.list[[1]],
             aes(u, es_0.99, color = factor(Crypto))) +
  geom_line() + 
  labs(x = "Threshold", y = "ES at p=0.99") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.title = element_text(size = 10),
        legend.position = "none") +
  scale_color_manual(labels = c("Bitcoin", "Dodgecoin", "Ethereum"), values = c("red", "green", "blue"))

p4 <- ggplot(df.list[[3]],
             aes(u, es_0.9, color = factor(Crypto))) +
  geom_line() + 
  labs(x = "Threshold", y = "ES at p=0.9", title = "Post-COVID19 Period") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.title = element_text(size = 10),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 8)) +
  scale_color_manual(labels = c("Bitcoin", "Dodgecoin", "Ethereum"), values = c("red", "green", "blue"))

p5 <- ggplot(df.list[[3]],
             aes(u, es_0.95, color = factor(Crypto))) +
  geom_line() + 
  labs(x = "Threshold", y = "ES at p=0.95") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.title = element_text(size = 10),
        legend.position = "none") +
  scale_color_manual(labels = c("Bitcoin", "Dodgecoin", "Ethereum"), values = c("red", "green", "blue"))

p6 <- ggplot(df.list[[3]],
             aes(u, es_0.99, color = factor(Crypto))) +
  geom_line() + 
  labs(x = "Threshold", y = "ES at p=0.99") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.title = element_text(size = 10),
        legend.position = "none") +
  scale_color_manual(labels = c("Bitcoin", "Dodgecoin", "Ethereum"), values = c("red", "green", "blue"))

p1 + p2 + p3 + p4 + p5 + p6 + plot_layout(nrow = 2)

# CVaR pre- vs post-pos ----
p1 <- ggplot(df.list[[2]],
             aes(u, es_0.9, color = factor(Crypto))) +
  geom_line() + 
  labs(x = "Threshold", y = "ES at p=0.9", title = "Pre-COVID19 Period") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.title = element_text(size = 10),
        legend.position = "none") +
  scale_color_manual(labels = c("Bitcoin", "Dodgecoin", "Ethereum"), values = c("red", "green", "blue"))

p2 <- ggplot(df.list[[2]],
             aes(u, es_0.95, color = factor(Crypto))) +
  geom_line() + 
  labs(x = "Threshold", y = "ES at p=0.95") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.title = element_text(size = 10),
        legend.position = "none") +
  scale_color_manual(labels = c("Bitcoin", "Dodgecoin", "Ethereum"), values = c("red", "green", "blue"))

p3 <- ggplot(df.list[[2]],
             aes(u, es_0.99, color = factor(Crypto))) +
  geom_line() + 
  labs(x = "Threshold", y = "ES at p=0.99") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.title = element_text(size = 10),
        legend.position = "none") +
  scale_color_manual(labels = c("Bitcoin", "Dodgecoin", "Ethereum"), values = c("red", "green", "blue"))

p4 <- ggplot(df.list[[4]],
             aes(u, es_0.9, color = factor(Crypto))) +
  geom_line() + 
  labs(x = "Threshold", y = "ES at p=0.9", title = "Post-COVID19 Period") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.title = element_text(size = 10),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 8)) +
  scale_color_manual(labels = c("Bitcoin", "Dodgecoin", "Ethereum"), values = c("red", "green", "blue"))

p5 <- ggplot(df.list[[4]],
             aes(u, es_0.95, color = factor(Crypto))) +
  geom_line() + 
  labs(x = "Threshold", y = "ES at p=0.95") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.title = element_text(size = 10),
        legend.position = "none") +
  scale_color_manual(labels = c("Bitcoin", "Dodgecoin", "Ethereum"), values = c("red", "green", "blue"))

p6 <- ggplot(df.list[[4]],
             aes(u, es_0.99, color = factor(Crypto))) +
  geom_line() + 
  labs(x = "Threshold", y = "ES at p=0.99") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.title = element_text(size = 10),
        legend.position = "none") +
  scale_color_manual(labels = c("Bitcoin", "Dodgecoin", "Ethereum"), values = c("red", "green", "blue"))

pblank <- ggplot() + theme_void()

p1 + p2 + p3 + pblank +
  p4 + p5 + p6 + pblank +
  plot_layout(nrow = 4)

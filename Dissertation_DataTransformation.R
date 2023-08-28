# Load libraries 
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(grid)
library(gridExtra)
library(extRemes)
library(evd)

# Load image
load("image/AggregatedData.RData")

# Create a new list for stationary dataframes
preCOVID_stationary <- preCOVID_fileResults
postCOVID_stationary <- postCOVID_fileResults

# Create a closing price change series ----
# 100(log Pt - log Pt-1) ----

for (i in 1:length(preCOVID_stationary)){
  
  preCOVID_stationary[[i]] <- preCOVID_stationary[[i]] %>% 
    select(Date, Year, High, file)
  
  preCOVID_stationary[[i]] <- preCOVID_stationary[[i]] %>%
    mutate(lag_price = lag(preCOVID_stationary[[i]]$High))
  preCOVID_stationary[[i]] <- preCOVID_stationary[[i]] %>%
    mutate(diff_price = 100 * (log(preCOVID_stationary[[i]]$High) - 
                                 log(preCOVID_stationary[[i]]$lag_price)))
}

for (i in 1:length(postCOVID_stationary)){
  
  postCOVID_stationary[[i]] <- postCOVID_stationary[[i]] %>% 
    select(Date, Year, High, file)
  
  postCOVID_stationary[[i]] <- postCOVID_stationary[[i]] %>%
    mutate(lag_price = lag(postCOVID_stationary[[i]]$High))
  postCOVID_stationary[[i]] <- postCOVID_stationary[[i]] %>%
    mutate(diff_price = 100 * (log(postCOVID_stationary[[i]]$High) - 
                                 log(postCOVID_stationary[[i]]$lag_price)))
}

# Save image
save.image(file = "image/StationaryData.RData")

# Plot ----
# Panel 1 - pre-COVID19 Raw series ----
lst1 <- c(
  which(preCOVID_fileResults[[1]]$Date == "2018-03-12"),
  which(preCOVID_fileResults[[1]]$Date == "2018-09-10"),
  which(preCOVID_fileResults[[1]]$Date == "2019-03-12"),
  which(preCOVID_fileResults[[1]]$Date == "2019-09-10"),
  which(preCOVID_fileResults[[1]]$Date == "2020-03-11"))

p1 <- ggplot(data = preCOVID_fileResults[[1]], aes(x = Date, y = High)) +
  geom_line() +
  geom_vline(xintercept = as.numeric(preCOVID_fileResults[[1]]$Date[lst1]), 
             linetype = 2) +
  theme_bw() + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 6.5)) +
  scale_x_date(breaks = preCOVID_fileResults[[1]]$Date[lst1],
               date_labels = "%b-%Y") +
  labs(y = "Bitcoin Highest Price (USD)")

lst2 <- c(
  which(preCOVID_fileResults[[2]]$Date == "2018-03-12"),
  which(preCOVID_fileResults[[2]]$Date == "2018-09-10"),
  which(preCOVID_fileResults[[2]]$Date == "2019-03-12"),
  which(preCOVID_fileResults[[2]]$Date == "2019-09-10"),
  which(preCOVID_fileResults[[2]]$Date == "2020-03-11"))

p2 <- ggplot(data = preCOVID_fileResults[[2]], aes(x = Date, y = High)) +
  geom_line() +
  geom_vline(xintercept = as.numeric(preCOVID_fileResults[[2]]$Date[lst2]), 
             linetype = 2) +
  theme_bw() + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 6.5)) +
  scale_x_date(breaks = preCOVID_fileResults[[2]]$Date[lst2],
               date_labels = "%b-%Y") +
  labs(y = "Dodgecoin Highest Price (USD)")

lst3 <- c(
  which(preCOVID_fileResults[[3]]$Date == "2018-03-12"),
  which(preCOVID_fileResults[[3]]$Date == "2018-09-10"),
  which(preCOVID_fileResults[[3]]$Date == "2019-03-12"),
  which(preCOVID_fileResults[[3]]$Date == "2019-09-10"),
  which(preCOVID_fileResults[[3]]$Date == "2020-03-11"))

p3 <- ggplot(data = preCOVID_fileResults[[3]], aes(x = Date, y = High)) +
  geom_line() +
  geom_vline(xintercept = as.numeric(preCOVID_fileResults[[3]]$Date[lst3]), 
             linetype = 2) +
  theme_bw() + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 6.5)) +
  scale_x_date(breaks = preCOVID_fileResults[[3]]$Date[lst3],
               date_labels = "%b-%Y") +
  labs(y = "Ethereum Highest Price (USD)")

grid.arrange(p1, p2, p3, ncol = 3,
             top = textGrob("Pre-COVID19 - Raw Time Series of Highest Price movement",gp = 
                              gpar(fontsize = 12,font = 1)))

# Panel 2 - pre-COVID19 differenced unadjusted  ----
lst4 <- c(
  which(preCOVID_stationary[[1]]$Date == "2018-03-12"),
  which(preCOVID_stationary[[1]]$Date == "2018-09-10"),
  which(preCOVID_stationary[[1]]$Date == "2019-03-12"),
  which(preCOVID_stationary[[1]]$Date == "2019-09-10"),
  which(preCOVID_stationary[[1]]$Date == "2020-03-11"))

p4 <- ggplot(data = preCOVID_stationary[[1]], aes(x = Date, y = diff_price)) +
  geom_line() +
  geom_vline(xintercept = as.numeric(preCOVID_stationary[[1]]$Date[lst4]), 
             linetype = 2) +
  theme_bw() + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 6.5)) +
  scale_x_date(breaks = preCOVID_stationary[[1]]$Date[lst4],
               date_labels = "%b-%Y") +
  labs(y = "100(log ht - log ht-1), Bitcoin")


lst5 <- c(
  which(preCOVID_stationary[[2]]$Date == "2018-03-12"),
  which(preCOVID_stationary[[2]]$Date == "2018-09-10"),
  which(preCOVID_stationary[[2]]$Date == "2019-03-12"),
  which(preCOVID_stationary[[2]]$Date == "2019-09-10"),
  which(preCOVID_stationary[[2]]$Date == "2020-03-11"))

p5 <- ggplot(data = preCOVID_stationary[[2]], aes(x = Date, y = diff_price)) +
  geom_line() +
  geom_vline(xintercept = as.numeric(preCOVID_stationary[[2]]$Date[lst5]), 
             linetype = 2) +
  theme_bw() + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 6.5)) +
  scale_x_date(breaks = preCOVID_stationary[[2]]$Date[lst5],
               date_labels = "%b-%Y") +
  labs(y = "100(log ht - log ht-1), Dodgecoin")


lst6 <- c(
  which(preCOVID_stationary[[3]]$Date == "2018-03-12"),
  which(preCOVID_stationary[[3]]$Date == "2018-09-10"),
  which(preCOVID_stationary[[3]]$Date == "2019-03-12"),
  which(preCOVID_stationary[[3]]$Date == "2019-09-10"),
  which(preCOVID_stationary[[3]]$Date == "2020-03-11"))

p6 <- ggplot(data = preCOVID_stationary[[3]], aes(x = Date, y = diff_price)) +
  geom_line() +
  geom_vline(xintercept = as.numeric(preCOVID_stationary[[3]]$Date[lst6]), 
             linetype = 2) +
  theme_bw() + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 6.5)) +
  scale_x_date(breaks = preCOVID_stationary[[3]]$Date[lst6],
               date_labels = "%b-%Y") +
  labs(y = "100(log ht - log ht-1), Ethereum")

grid.arrange(p4, p5, p6, ncol = 3,
             top = textGrob("Pre-COVID19 - Differenced Highest Price movement",gp = 
                              gpar(fontsize = 12,font = 1)))

# Panel 3 - post-COVID19 Raw series ----
lst7 <- c(
  which(postCOVID_fileResults[[1]]$Date == "2022-06-01"),
  which(postCOVID_fileResults[[1]]$Date == "2022-07-24"),
  which(postCOVID_fileResults[[1]]$Date == "2022-09-15"),
  which(postCOVID_fileResults[[1]]$Date == "2022-11-07"),
  which(postCOVID_fileResults[[1]]$Date == "2022-12-31"))

p7 <- ggplot(data = postCOVID_fileResults[[1]], aes(x = Date, y = High)) +
  geom_line() +
  geom_vline(xintercept = as.numeric(postCOVID_fileResults[[1]]$Date[lst7]), 
             linetype = 2) +
  theme_bw() + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 6.5)) +
  scale_x_date(breaks = postCOVID_fileResults[[1]]$Date[lst7],
               date_labels = "%b-%Y") +
  labs(y = "Bitcoin Highest Price (USD)")

lst8 <- c(
  which(postCOVID_fileResults[[2]]$Date == "2022-06-01"),
  which(postCOVID_fileResults[[2]]$Date == "2022-07-24"),
  which(postCOVID_fileResults[[2]]$Date == "2022-09-15"),
  which(postCOVID_fileResults[[2]]$Date == "2022-11-07"),
  which(postCOVID_fileResults[[2]]$Date == "2022-12-31"))

p8 <- ggplot(data = postCOVID_fileResults[[2]], aes(x = Date, y = High)) +
  geom_line() +
  geom_vline(xintercept = as.numeric(postCOVID_fileResults[[2]]$Date[lst8]), 
             linetype = 2) +
  theme_bw() + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 6.5)) +
  scale_x_date(breaks = postCOVID_fileResults[[2]]$Date[lst8],
               date_labels = "%b-%Y") +
  labs(y = "Dodgecoin Highest Price (USD)")

lst9 <- c(
  which(postCOVID_fileResults[[3]]$Date == "2022-06-01"),
  which(postCOVID_fileResults[[3]]$Date == "2022-07-24"),
  which(postCOVID_fileResults[[3]]$Date == "2022-09-15"),
  which(postCOVID_fileResults[[3]]$Date == "2022-11-07"),
  which(postCOVID_fileResults[[3]]$Date == "2022-12-31"))

p9 <- ggplot(data = postCOVID_fileResults[[3]], aes(x = Date, y = High)) +
  geom_line() +
  geom_vline(xintercept = as.numeric(postCOVID_fileResults[[3]]$Date[lst9]), 
             linetype = 2) +
  theme_bw() + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 6.5)) +
  scale_x_date(breaks = postCOVID_fileResults[[3]]$Date[lst9],
               date_labels = "%b-%Y") +
  labs(y = "Ethereum Highest Price (USD)")

grid.arrange(p7, p8, p9, ncol = 3,
             top = textGrob("Post-COVID19 - Raw Time Series of Highest Price movement",gp = 
                              gpar(fontsize = 12,font = 0.1)))

# Panel 4 - post-COVID19 differenced unadjusted ----

lst10 <- c(
  which(postCOVID_stationary[[1]]$Date == "2022-06-01"),
  which(postCOVID_stationary[[1]]$Date == "2022-07-24"),
  which(postCOVID_stationary[[1]]$Date == "2022-09-15"),
  which(postCOVID_stationary[[1]]$Date == "2022-11-07"),
  which(postCOVID_stationary[[1]]$Date == "2022-12-31"))

p10 <- ggplot(data = postCOVID_stationary[[1]], aes(x = Date, y = diff_price)) +
  geom_line() +
  geom_vline(xintercept = as.numeric(postCOVID_stationary[[1]]$Date[lst10]), 
             linetype = 2) +
  theme_bw() + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 6.5)) +
  scale_x_date(breaks = postCOVID_stationary[[1]]$Date[lst10],
               date_labels = "%b-%Y") +
  labs(y = "100(log ht - log ht-1), Bitcoin")


lst11 <- c(
  which(postCOVID_stationary[[2]]$Date == "2022-06-01"),
  which(postCOVID_stationary[[2]]$Date == "2022-07-24"),
  which(postCOVID_stationary[[2]]$Date == "2022-09-15"),
  which(postCOVID_stationary[[2]]$Date == "2022-11-07"),
  which(postCOVID_stationary[[2]]$Date == "2022-12-31"))

p11 <- ggplot(data = postCOVID_stationary[[2]], aes(x = Date, y = diff_price)) +
  geom_line() +
  geom_vline(xintercept = as.numeric(postCOVID_stationary[[2]]$Date[lst11]), 
             linetype = 2) +
  theme_bw() + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 6.5)) +
  scale_x_date(breaks = postCOVID_stationary[[2]]$Date[lst11],
               date_labels = "%b-%Y") +
  labs(y = "100(log ht - log ht-1), Dodgecoin")


lst12 <- c(
  which(postCOVID_stationary[[3]]$Date == "2022-06-01"),
  which(postCOVID_stationary[[3]]$Date == "2022-07-24"),
  which(postCOVID_stationary[[3]]$Date == "2022-09-15"),
  which(postCOVID_stationary[[3]]$Date == "2022-11-07"),
  which(postCOVID_stationary[[3]]$Date == "2022-12-31"))

p12 <- ggplot(data = postCOVID_stationary[[3]], aes(x = Date, y = diff_price)) +
  geom_line() +
  geom_vline(xintercept = as.numeric(postCOVID_stationary[[3]]$Date[lst12]), 
             linetype = 2) +
  theme_bw() + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 6.5)) +
  scale_x_date(breaks = postCOVID_stationary[[3]]$Date[lst12],
               date_labels = "%b-%Y") +
  labs(y = "100(log ht - log ht-1), Ethereum")

grid.arrange(p10, p11, p12, ncol = 3,
             top = textGrob("Post-COVID19 - Differenced Highest Price movement", gp = 
                              gpar(fontsize = 12,font = 1)))

# Dickey-Fuller Test
library(tseries)
adf.test(na.omit(preCOVID_stationary[[1]]$diff_price))
adf.test(na.omit(preCOVID_stationary[[2]]$diff_price))
adf.test(na.omit(preCOVID_stationary[[3]]$diff_price))

adf.test(na.omit(postCOVID_stationary[[1]]$diff_price))
adf.test(na.omit(postCOVID_stationary[[2]]$diff_price))
adf.test(na.omit(postCOVID_stationary[[3]]$diff_price))

adf.test(na.omit(preCOVID_fileResults[[1]]$High))
adf.test(na.omit(preCOVID_fileResults[[2]]$High))
adf.test(na.omit(preCOVID_fileResults[[3]]$High))

adf.test(na.omit(postCOVID_fileResults[[1]]$High))
adf.test(na.omit(postCOVID_fileResults[[2]]$High))
adf.test(na.omit(postCOVID_fileResults[[3]]$High))


# Mean Residual Plot ----
library(extRemes)

# Load stationary data
load("image/StationaryData.RData")

mrp1 <- mrlplot(preCOVID_stationary[[1]]$diff_price, na.action = na.omit, main = "Bitcoin")
mrp2 <- mrlplot(preCOVID_stationary[[2]]$diff_price, na.action = na.omit, main = "Dodgecoin")
mrp3 <- mrlplot(preCOVID_stationary[[3]]$diff_price, na.action = na.omit, main = "Ethereum")
mrp4 <- mrlplot(postCOVID_stationary[[1]]$diff_price, na.action = na.omit, main = "Bitcoin")
mrp5 <- mrlplot(postCOVID_stationary[[2]]$diff_price, na.action = na.omit, main = "Dodgecoin")
mrp6 <- mrlplot(postCOVID_stationary[[3]]$diff_price, na.action = na.omit, main = "Ethereum")

legend("topright", title = expression(bold("Legend")), 
       legend = c("95% Confidence Interval", "Mean Excess"),
       lty = c("dashed", "solid"), lwd = c(1, 1.5), col = c("grey", "black"), bty = "n")

# ----
# Apply Gallant et al. (1992) techniques ----
# 1. Data transformation ----
# Create an empty list for results
pre_adjustment <- vector(mode = "list", length = length(preCOVID_stationary))
post_adjustment <- vector(mode = "list", length = length(postCOVID_stationary))

# pre-COVID19
for (i in 1:length(preCOVID_stationary)){
  
  # Create delta_Pt (See line 12 to 37)
  names(preCOVID_stationary[[i]])[names(preCOVID_stationary[[i]]) == 'diff_price'] <- "delta_Pt"
  
  # Extract day-of-the-week
  preCOVID_stationary[[i]]$Day <- weekdays(preCOVID_stationary[[i]]$Date)
  
  # Extract months
  preCOVID_stationary[[i]]$m <- as.character(month(preCOVID_stationary[[i]]$Date, label = TRUE, abbr = FALSE))
  
  # Extract date
  preCOVID_stationary[[i]]$Date_part <- as.numeric(format(as.Date(preCOVID_stationary[[i]]$Date,format="%Y-%m-%d"), 
                                                          format = "%d"))
  # Extract week of December
  preCOVID_stationary[[i]] <- preCOVID_stationary[[i]] %>%
    mutate(wk_dec = case_when((preCOVID_stationary[[i]]$m == "December" & preCOVID_stationary[[i]]$Date_part <= 7) ~ "December 1-7",
                              (preCOVID_stationary[[i]]$m == "December" & preCOVID_stationary[[i]]$Date_part > 7 & 
                                 preCOVID_stationary[[i]]$Date_part <= 14) ~ "December 8-14",
                              (preCOVID_stationary[[i]]$m == "December" & preCOVID_stationary[[i]]$Date_part > 14 & 
                                 preCOVID_stationary[[i]]$Date_part <= 21) ~ "December 15-21",
                              (preCOVID_stationary[[i]]$m == "December" & preCOVID_stationary[[i]]$Date_part > 21) ~ "December 22-31",
                              TRUE ~ ""))
  
  # Extract week of January
  preCOVID_stationary[[i]] <- preCOVID_stationary[[i]] %>%
    mutate(wk_jan = case_when((preCOVID_stationary[[i]]$m == "January" & preCOVID_stationary[[i]]$Date_part <= 7) ~ "January 1-7",
                              (preCOVID_stationary[[i]]$m == "January" & preCOVID_stationary[[i]]$Date_part > 7 & 
                                 preCOVID_stationary[[i]]$Date_part <= 14) ~ "January 8-14",
                              (preCOVID_stationary[[i]]$m == "January" & preCOVID_stationary[[i]]$Date_part > 14 & 
                                 preCOVID_stationary[[i]]$Date_part <= 21) ~ "January 15-21",
                              (preCOVID_stationary[[i]]$m == "January" & preCOVID_stationary[[i]]$Date_part > 21) ~ "January 22-31",
                              TRUE ~ ""))
  
  # Combine month + week of Dec/Jan
  preCOVID_stationary[[i]] <- preCOVID_stationary[[i]] %>%
    mutate(Month = ifelse(
      preCOVID_stationary[[i]]$wk_dec != "", preCOVID_stationary[[i]]$wk_dec,
      ifelse(preCOVID_stationary[[i]]$wk_jan != "", preCOVID_stationary[[i]]$wk_jan, preCOVID_stationary[[i]]$m)
    ))
  
  # Create the time trend
  preCOVID_stationary[[i]] <- preCOVID_stationary[[i]] %>%
    group_by(file) %>%
    mutate(t = rank(Date)/nrow(preCOVID_stationary[[i]]))
  preCOVID_stationary[[i]]$t2 <- (preCOVID_stationary[[i]]$t)^2
  
  # Select only required variables 
  preCOVID_stationary[[i]] <- preCOVID_stationary[[i]] %>%
    select(Date, Year, Close, file, delta_Pt, Day, Month, t, t2)
  
  # Store as transformed dataframes in a list
  pre_adjustment[[i]] <- as.data.frame(preCOVID_stationary[[i]])
}

# post-COVID19
for (i in 1:length(postCOVID_stationary)){
  
  # Create delta_Pt (See line 12 to 37)
  names(postCOVID_stationary[[i]])[names(postCOVID_stationary[[i]]) == 'diff_price'] <- "delta_Pt"
  
  # Extract day-of-the-week
  postCOVID_stationary[[i]]$Day <- weekdays(postCOVID_stationary[[i]]$Date)
  
  # Extract months
  postCOVID_stationary[[i]]$m <- as.character(month(postCOVID_stationary[[i]]$Date, label = TRUE, abbr = FALSE))
  
  # Extract date
  postCOVID_stationary[[i]]$Date_part <- as.numeric(format(as.Date(postCOVID_stationary[[i]]$Date,format="%Y-%m-%d"), 
                                                           format = "%d"))
  # Extract week of December
  postCOVID_stationary[[i]] <- postCOVID_stationary[[i]] %>%
    mutate(wk_dec = case_when((postCOVID_stationary[[i]]$m == "December" & postCOVID_stationary[[i]]$Date_part <= 7) ~ "December 1-7",
                              (postCOVID_stationary[[i]]$m == "December" & postCOVID_stationary[[i]]$Date_part > 7 & 
                                 postCOVID_stationary[[i]]$Date_part <= 14) ~ "December 8-14",
                              (postCOVID_stationary[[i]]$m == "December" & postCOVID_stationary[[i]]$Date_part > 14 & 
                                 postCOVID_stationary[[i]]$Date_part <= 21) ~ "December 15-21",
                              (postCOVID_stationary[[i]]$m == "December" & postCOVID_stationary[[i]]$Date_part > 21) ~ "December 22-31",
                              TRUE ~ ""))
  
  # Extract week of January
  postCOVID_stationary[[i]] <- postCOVID_stationary[[i]] %>%
    mutate(wk_jan = case_when((postCOVID_stationary[[i]]$m == "January" & postCOVID_stationary[[i]]$Date_part <= 7) ~ "January 1-7",
                              (postCOVID_stationary[[i]]$m == "January" & postCOVID_stationary[[i]]$Date_part > 7 & 
                                 postCOVID_stationary[[i]]$Date_part <= 14) ~ "January 8-14",
                              (postCOVID_stationary[[i]]$m == "January" & postCOVID_stationary[[i]]$Date_part > 14 & 
                                 postCOVID_stationary[[i]]$Date_part <= 21) ~ "January 15-21",
                              (postCOVID_stationary[[i]]$m == "January" & postCOVID_stationary[[i]]$Date_part > 21) ~ "January 22-31",
                              TRUE ~ ""))
  
  # Combine month + week of Dec/Jan
  postCOVID_stationary[[i]] <- postCOVID_stationary[[i]] %>%
    mutate(Month = ifelse(
      postCOVID_stationary[[i]]$wk_dec != "", postCOVID_stationary[[i]]$wk_dec,
      ifelse(postCOVID_stationary[[i]]$wk_jan != "", postCOVID_stationary[[i]]$wk_jan, postCOVID_stationary[[i]]$m)
    ))
  
  # Create the time trend
  postCOVID_stationary[[i]] <- postCOVID_stationary[[i]] %>%
    group_by(file) %>%
    mutate(t = rank(Date)/nrow(postCOVID_stationary[[i]]))
  postCOVID_stationary[[i]]$t2 <- (postCOVID_stationary[[i]]$t)^2
  
  # Select only required variables 
  postCOVID_stationary[[i]] <- postCOVID_stationary[[i]] %>%
    select(Date, Year, Close, file, delta_Pt, Day, Month, t, t2)
  
  # Store as transformed dataframes in a list
  post_adjustment[[i]] <- as.data.frame(postCOVID_stationary[[i]])
}

# [IGNORE] 2. Dummify ----
pre_dummy <- pre_adjustment
post_dummy <- post_adjustment

# Create an empty list for results
pre_transformed <- vector(mode = "list", length = length(pre_adjustment))
post_transformed <- vector(mode = "list", length = length(post_adjustment))

# pre-COVID19
for (i in 1:length(pre_adjustment)){
  
  # Create the dummies
  pre_dummy[[i]] <- dummy_cols(pre_dummy[[i]], 
                               select_columns = c('Day', 'Month', 'Year'),
                               remove_selected_columns = TRUE)
  
  # Drop 1 dummy to avoid multicollinearity
  # Drop unnecessary variables
  pre_dummy[[i]] <- pre_dummy[[i]] %>%
    select(-c("Day_Monday", "Month_February", "Year_2019", "Date", "Close", "file"))
  
  # Store as transformed dataframes in a list
  pre_transformed[[i]] <- as.data.frame(pre_dummy[[i]])
}

# post-COVID19
for (i in 1:length(post_adjustment)){
  
  # Create the dummies
  post_dummy[[i]] <- dummy_cols(post_dummy[[i]], 
                                select_columns = c('Day', 'Month', 'Year'),
                                remove_selected_columns = TRUE)
  
  # Drop 1 dummy to avoid multicollinearity
  post_dummy[[i]] <- post_dummy[[i]] %>%
    select(-c("Day_Monday", "Month_February", "Year_2021", "Date", "Close", "file"))
  
  # Store as transformed dataframes in a list
  post_transformed[[i]] <- as.data.frame(post_dummy[[i]])
}

# 3. Mean equation
set.seed(123)

# Create intermediate empty list
pre_mean <- vector(mode = "list", length = length(pre_transformed))
post_mean <- vector(mode = "list", length = length(post_transformed))

# Create an empty list to store lm coeff and resid
pre.mean.lm <- vector(mode = "list", length = length(pre_transformed))
post.mean.lm <- vector(mode = "list", length = length(post_transformed))

# pre-COVID19
for (i in 1:length(pre_transformed)){
  
  # Remove time trend in the design matrix
  pre_mean[[i]] <- pre_transformed[[i]] %>%
    select(-c("t", "t2"))
  
  # Mean equation
  pre.mean.lm[[i]] <- lm(delta_Pt ~ .,
                         data = pre_mean[[i]])
}

# post-COVID19
for (i in 1:length(post_transformed)){
  
  # Remove time trend in the design matrix
  post_mean[[i]] <- post_transformed[[i]] %>%
    select(-c("t", "t2"))
  
  # Mean equation
  post.mean.lm[[i]] <- lm(delta_Pt ~ .,
                          data = post_mean[[i]])
}

# 4. Variance equation

set.seed(123)

# Create intermediate empty list
pre_var <- vector(mode = "list", length = length(pre_transformed))
post_var <- vector(mode = "list", length = length(post_transformed))

# Create an empty list to store lm coeff and resid
pre.var.lm <- vector(mode = "list", length = length(pre_transformed))
post.var.lm <- vector(mode = "list", length = length(post_transformed))

# pre-COVID19
for (i in 1:length(pre_transformed)){
  
  # Add the transformed dataframe
  pre_var[[i]] <- pre_transformed[[i]]
  
  # Add the column fo residual in mean equation, u
  pre_var[[i]]$mean.resid <- c(NA, pre.mean.lm[[i]]$residuals)
  
  # Create log(u^2)
  pre_var[[i]] <- pre_var[[i]] %>%
    mutate(mean.resid.trans = log(mean.resid ^ 2))
  
  # Remove delta_Pt and u (Not in design matrix)
  pre_var[[i]] <- subset(pre_var[[i]], select = -c(delta_Pt, mean.resid))
  
  # Variance equation
  pre.var.lm[[i]] <- lm(mean.resid.trans ~ .,
                        data = pre_var[[i]])
}

# post-COVID19
for (i in 1:length(post_transformed)){
  
  # Add the transformed dataframe
  post_var[[i]] <- post_transformed[[i]]
  
  # Add the column fo residual in mean equation, u
  post_var[[i]]$mean.resid <- c(NA, post.mean.lm[[i]]$residuals)
  
  # Create log(u^2)
  post_var[[i]] <- post_var[[i]] %>%
    mutate(mean.resid.trans = log(mean.resid ^ 2))
  
  # Remove delta_Pt and u (Not in design matrix)
  post_var[[i]] <- subset(post_var[[i]], select = -c(delta_Pt, mean.resid))
  
  # Variance equation
  post.var.lm[[i]] <- lm(mean.resid.trans ~ .,
                         data = post_var[[i]])
}

# 5. Final linear transformation
# 5(a) Create alpha
pre.alpha <- vector(mode = "list", length = length(pre_transformed))
post.alpha <- vector(mode = "list", length = length(post_transformed))

# pre-COVID
for (i in 1:length(pre_transformed)){
  
  # Create dataframe with u and x_gamma
  pre.alpha[[i]] <- do.call(rbind, Map(data.frame,
                                       u = pre.mean.lm[[i]]$residuals,
                                       x_gamma = pre.var.lm[[i]]$fitted.values))
  
  # Create exp(x_gamma/2)
  pre.alpha[[i]] <- pre.alpha[[i]] %>%
    mutate(exp_x_gamma = exp(x_gamma/2))
  
  # Create alpha
  pre.alpha[[i]] <- pre.alpha[[i]] %>%
    mutate(alpha = u/exp_x_gamma)
  
  # Create alpha^2
  pre.alpha[[i]] <- pre.alpha[[i]] %>%
    mutate(alpha2 = alpha^2)
}

# post-COVID
for (i in 1:length(post_transformed)){
  
  # Create dataframe with u and x_gamma
  post.alpha[[i]] <- do.call(rbind, Map(data.frame,
                                        u = post.mean.lm[[i]]$residuals,
                                        x_gamma = post.var.lm[[i]]$fitted.values))
  
  # Create exp(x_gamma/2)
  post.alpha[[i]] <- post.alpha[[i]] %>%
    mutate(exp_x_gamma = exp(x_gamma/2))
  
  # Create alpha
  post.alpha[[i]] <- post.alpha[[i]] %>%
    mutate(alpha = u/exp_x_gamma)
  
  # Create alpha^2
  post.alpha[[i]] <- post.alpha[[i]] %>%
    mutate(alpha2 = alpha^2)
}

# 5(b) Create transformation coeff
pre.linear.trans.var <- vector(mode = "list", length = length(pre_transformed))
post.linear.trans.var <- vector(mode = "list", length = length(post_transformed))

# pre-COVID
for (i in 1:length(pre_transformed)){
  
  # cbind delta_Pt and alpha
  pre.linear.trans.var[[i]] <- cbind(
    mean(pre_adjustment[[i]]$delta_Pt, na.rm = TRUE),
    var(pre_adjustment[[i]]$delta_Pt, na.rm = TRUE),
    sum(pre.alpha[[i]]$alpha),
    sum(pre.alpha[[i]]$alpha2),
    nrow(pre.alpha[[i]])
  )
  
  # Change to dataframe type
  pre.linear.trans.var[[i]] <- as.data.frame(pre.linear.trans.var[[i]])
  
  # Rename columns
  colnames(pre.linear.trans.var[[i]]) <- c("e_w", "v_w", "sum_alpha", "sum_alpha2", "n")
  
  # Compute a
  pre.linear.trans.var[[i]] <- pre.linear.trans.var[[i]] %>%
    mutate(a = e_w - ((sum_alpha/n)*sqrt(((n-1)*v_w)/(sum_alpha2 - sum_alpha/n))))
  
  # Compute b
  pre.linear.trans.var[[i]] <- pre.linear.trans.var[[i]] %>%
    mutate(b = sqrt(((n-1)*v_w)/(sum_alpha2 - sum_alpha/n)))
}

# post-COVID
for (i in 1:length(post_transformed)){
  
  # cbind delta_Pt and alpha
  post.linear.trans.var[[i]] <- cbind(
    mean(post_adjustment[[i]]$delta_Pt, na.rm = TRUE),
    var(post_adjustment[[i]]$delta_Pt, na.rm = TRUE),
    sum(post.alpha[[i]]$alpha),
    sum(post.alpha[[i]]$alpha2),
    nrow(post.alpha[[i]])
  )
  
  # Change to dataframe type
  post.linear.trans.var[[i]] <- as.data.frame(post.linear.trans.var[[i]])
  
  # Rename columns
  colnames(post.linear.trans.var[[i]]) <- c("e_w", "v_w", "sum_alpha", "sum_alpha2", "n")
  
  # Compute a
  post.linear.trans.var[[i]] <- post.linear.trans.var[[i]] %>%
    mutate(a = e_w - ((sum_alpha/n)*sqrt(((n-1)*v_w)/(sum_alpha2 - sum_alpha/n))))
  
  # Compute b
  post.linear.trans.var[[i]] <- post.linear.trans.var[[i]] %>%
    mutate(b = sqrt(((n-1)*v_w)/(sum_alpha2 - sum_alpha/n)))
}

# 6. Apply transformation
# Create an empty list for results
preCOVID_finalTransformed <- vector(mode = "list", length = length(preCOVID_stationary))
postCOVID_finalTransformed <- vector(mode = "list", length = length(postCOVID_stationary))

# pre-COVID
for (i in 1:length(preCOVID_stationary)){
  
  # Create dataframe
  preCOVID_finalTransformed[[i]] <- preCOVID_stationary[[i]]
  
  # cbind alpha
  preCOVID_finalTransformed[[i]]$alpha <- c(NA, pre.alpha[[i]]$alpha)
  
  # Apply transformation
  preCOVID_finalTransformed[[i]] <- preCOVID_finalTransformed[[i]] %>%
    mutate(adj_delta_Pt =  pre.linear.trans.var[[i]]$a + pre.linear.trans.var[[i]]$b * alpha)
}

# post-COVID
for (i in 1:length(postCOVID_stationary)){
  
  # Create dataframe
  postCOVID_finalTransformed[[i]] <- postCOVID_stationary[[i]]
  
  # cbind alpha
  postCOVID_finalTransformed[[i]]$alpha <- c(NA, post.alpha[[i]]$alpha)
  
  # Apply transformation
  postCOVID_finalTransformed[[i]] <- postCOVID_finalTransformed[[i]] %>%
    mutate(adj_delta_Pt =  post.linear.trans.var[[i]]$a + post.linear.trans.var[[i]]$b * alpha)
}

# Save image
save.image(file = "image/AdjustmentVar.RData")

# [IGNORE] Plot ----
# Panel 1 - pre-COVID19
lst1 <- c(
  which(preCOVID_finalTransformed[[1]]$Date == "2018-07-01"),
  which(preCOVID_finalTransformed[[1]]$Date == "2019-01-01"),
  which(preCOVID_finalTransformed[[1]]$Date == "2019-07-01"),
  which(preCOVID_finalTransformed[[1]]$Date == "2019-12-31"))

p1 <- ggplot(data = preCOVID_finalTransformed[[1]], aes(x = Date, y = adj_delta_Pt)) +
  geom_line() +
  geom_vline(xintercept = as.numeric(preCOVID_finalTransformed[[1]]$Date[lst1]), 
             linetype = 2) +
  theme_bw() + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  scale_x_date(date_labels = "%b-%Y") +
  labs(y = "Adjusted delta(Pt), Bitcoin")


lst2 <- c(
  which(preCOVID_finalTransformed[[2]]$Date == "2018-07-01"),
  which(preCOVID_finalTransformed[[2]]$Date == "2019-01-01"),
  which(preCOVID_finalTransformed[[2]]$Date == "2019-07-01"),
  which(preCOVID_finalTransformed[[2]]$Date == "2019-12-31"))

p2 <- ggplot(data = preCOVID_finalTransformed[[1]], aes(x = Date, y = adj_delta_Pt)) +
  geom_line() +
  geom_vline(xintercept = as.numeric(preCOVID_finalTransformed[[1]]$Date[lst2]), 
             linetype = 2) +
  theme_bw() + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  scale_x_date(date_labels = "%b-%Y") +
  labs(y = "Adjusted delta(Pt), Dodgecoin")


lst3 <- c(
  which(preCOVID_finalTransformed[[3]]$Date == "2018-07-01"),
  which(preCOVID_finalTransformed[[3]]$Date == "2019-01-01"),
  which(preCOVID_finalTransformed[[3]]$Date == "2019-07-01"),
  which(preCOVID_finalTransformed[[3]]$Date == "2019-12-31"))

p3 <- ggplot(data = preCOVID_finalTransformed[[1]], aes(x = Date, y = adj_delta_Pt)) +
  geom_line() +
  geom_vline(xintercept = as.numeric(preCOVID_finalTransformed[[1]]$Date[lst3]), 
             linetype = 2) +
  theme_bw() + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  scale_x_date(date_labels = "%b-%Y") +
  labs(y = "Adjusted delta(Pt), Ethereum")

grid.arrange(p1, p2, p3, ncol = 3,
             top = textGrob("Pre-COVID19 - Adjusted Closing Price movement",gp = 
                              gpar(fontsize = 12,font = 1)))

# Panel 2 - post-COVID19

lst4 <- c(
  which(postCOVID_finalTransformed[[1]]$Date == "2020-01-01"),
  which(postCOVID_finalTransformed[[1]]$Date == "2020-07-01"),
  which(postCOVID_finalTransformed[[1]]$Date == "2021-01-01"),
  which(postCOVID_finalTransformed[[1]]$Date == "2021-06-30"))

p4 <- ggplot(data = postCOVID_finalTransformed[[1]], aes(x = Date, y = adj_delta_Pt)) +
  geom_line() +
  geom_vline(xintercept = as.numeric(postCOVID_finalTransformed[[1]]$Date[lst4]), 
             linetype = 2) +
  theme_bw() + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  scale_x_date(date_labels = "%b-%Y") +
  labs(y = "Adjusted delta(Pt), Bitcoin")


lst5 <- c(
  which(postCOVID_finalTransformed[[1]]$Date == "2020-01-01"),
  which(postCOVID_finalTransformed[[1]]$Date == "2020-07-01"),
  which(postCOVID_finalTransformed[[1]]$Date == "2021-01-01"),
  which(postCOVID_finalTransformed[[1]]$Date == "2021-06-30"))

p5 <- ggplot(data = postCOVID_finalTransformed[[1]], aes(x = Date, y = adj_delta_Pt)) +
  geom_line() +
  geom_vline(xintercept = as.numeric(postCOVID_finalTransformed[[1]]$Date[lst5]), 
             linetype = 2) +
  theme_bw() + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  scale_x_date(date_labels = "%b-%Y") +
  labs(y = "Adjusted delta(Pt), Dodgecoin")


lst6 <- c(
  which(postCOVID_finalTransformed[[1]]$Date == "2020-01-01"),
  which(postCOVID_finalTransformed[[1]]$Date == "2020-07-01"),
  which(postCOVID_finalTransformed[[1]]$Date == "2021-01-01"),
  which(postCOVID_finalTransformed[[1]]$Date == "2021-06-30"))

p6 <- ggplot(data = postCOVID_finalTransformed[[1]], aes(x = Date, y = adj_delta_Pt)) +
  geom_line() +
  geom_vline(xintercept = as.numeric(postCOVID_finalTransformed[[1]]$Date[lst6]), 
             linetype = 2) +
  theme_bw() + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  scale_x_date(date_labels = "%b-%Y") +
  labs(y = "Adjusted delta(Pt), Ethereum")

grid.arrange(p4, p5, p6, ncol = 3,
             top = textGrob("Post-COVID19 - Adjusted Closing Price movement", gp = 
                              gpar(fontsize = 12,font = 1)))

# Load image from script Section_3_3_stationary
load("image/StationaryData.RData")

# Separate positives and negatives ----
# Create a new list for stationary dataframes
preCOVID_stationary_pos <- preCOVID_stationary
preCOVID_stationary_neg <- preCOVID_stationary

for (i in 1:length(preCOVID_stationary_pos)){
  preCOVID_stationary_pos[[i]] <- preCOVID_stationary_pos[[i]][preCOVID_stationary_pos[[i]]$diff_price>0,]
}

for (i in 1:length(preCOVID_stationary_neg)){
  preCOVID_stationary_neg[[i]] <- preCOVID_stationary_neg[[i]][preCOVID_stationary_neg[[i]]$diff_price<0,]
}

postCOVID_stationary_pos <- postCOVID_stationary
postCOVID_stationary_neg <- postCOVID_stationary

for (i in 1:length(postCOVID_stationary_pos)){
  postCOVID_stationary_pos[[i]] <- postCOVID_stationary_pos[[i]][postCOVID_stationary_pos[[i]]$diff_price>0,]
}

for (i in 1:length(postCOVID_stationary_neg)){
  postCOVID_stationary_neg[[i]] <- postCOVID_stationary_neg[[i]][postCOVID_stationary_neg[[i]]$diff_price<0,]
}

preCOVID_stationary_neg[[1]]$diff_price <- abs(preCOVID_stationary_neg[[1]]$diff_price)
preCOVID_stationary_neg[[2]]$diff_price <- abs(preCOVID_stationary_neg[[2]]$diff_price)
preCOVID_stationary_neg[[3]]$diff_price <- abs(preCOVID_stationary_neg[[3]]$diff_price)

postCOVID_stationary_neg[[1]]$diff_price <- abs(postCOVID_stationary_neg[[1]]$diff_price)
postCOVID_stationary_neg[[2]]$diff_price <- abs(postCOVID_stationary_neg[[2]]$diff_price)
postCOVID_stationary_neg[[3]]$diff_price <- abs(postCOVID_stationary_neg[[3]]$diff_price)

# Extremal Index ----
u <- 3
i <- 3
evd::exi(na.omit(postCOVID_stationary_pos[[i]]$diff_price), u = u)
n <- nrow(postCOVID_stationary_pos[[i]])
e <- nrow(postCOVID_stationary_pos[[i]][postCOVID_stationary_pos[[i]]$diff_price > u,])
(n-e)/n

# Vector of u values
# Neg log diff ----
tlim_neg_pre1 <- quantile(na.omit(preCOVID_stationary_neg[[1]]$diff_price), 
                          probs = c(0.4,0.99))
tlim_neg_pre2 <- quantile(na.omit(preCOVID_stationary_neg[[2]]$diff_price), 
                          probs = c(0.4,0.99))
tlim_neg_pre3 <- quantile(na.omit(preCOVID_stationary_neg[[3]]$diff_price), 
                          probs = c(0.4,0.99))

tlim_neg_post1 <- quantile(na.omit(postCOVID_stationary_neg[[1]]$diff_price), 
                           probs = c(0.4,0.99))
tlim_neg_post2 <- quantile(na.omit(postCOVID_stationary_neg[[2]]$diff_price), 
                           probs = c(0.4,0.99))
tlim_neg_post3 <- quantile(na.omit(postCOVID_stationary_neg[[3]]$diff_price), 
                           probs = c(0.4,0.99))

# Positive log diff ----
tlim_pos_pre1 <- quantile(na.omit(preCOVID_stationary_pos[[1]]$diff_price), 
                          probs = c(0.4,0.99))
tlim_pos_pre2 <- quantile(na.omit(preCOVID_stationary_pos[[2]]$diff_price), 
                          probs = c(0.4,0.99))
tlim_pos_pre3 <- quantile(na.omit(preCOVID_stationary_pos[[3]]$diff_price), 
                          probs = c(0.4,0.99))

tlim_pos_post1 <- quantile(na.omit(postCOVID_stationary_pos[[1]]$diff_price), 
                           probs = c(0.4,0.99))
tlim_pos_post2 <- quantile(na.omit(postCOVID_stationary_pos[[2]]$diff_price), 
                           probs = c(0.4,0.99))
tlim_pos_post3 <- quantile(na.omit(postCOVID_stationary_pos[[3]]$diff_price), 
                           probs = c(0.4,0.99))

# Exiplots ----
evd::exiplot(na.omit(postCOVID_stationary_pos[[1]]$diff_price),
             tlim = tlim_pos_post1, 
             col = "blue",
             lwd = 2.0,
             xlab = "Threshold, u",
             ylab = "Extremal Index")
evd::exiplot(na.omit(postCOVID_stationary_pos[[2]]$diff_price),
             tlim = tlim_pos_post2, 
             add = TRUE,
             col = "red",
             lwd = 2.0)
evd::exiplot(na.omit(postCOVID_stationary_pos[[3]]$diff_price),
             tlim = tlim_pos_post3, 
             add = TRUE,
             col = "green",
             lwd = 2.0)
title("Plot Estimates of the Extremal Index - PostCOVID19")
legend(6, 0.45, 
       legend=c("Bitcoin", "Dodgecoin", "Ethereum"),
       col=c("blue", "red", "green"), 
       lty = 1,
       lwd = 2.0,
       cex=0.8,
       title="Legend", text.font=2, bg='lightgrey')

source('styleguide.R')
library(stringr)
library(reshape2)

df <- read.csv("survey-data-20-07-08.csv")

se <- function(x) sqrt(var(x) / length(x))

filter_nas <- function(x) x[!is.na(x) & x != ""]

# Basic bar chart of whether students are enrolling
ggplot(data=subset(df, (!is.na(enroll) & enroll != "")), aes(x=factor(enroll))) + 
  geom_bar(aes(fill = factor(enroll))) + 
  scale_fill_manual(values = c(primary[1], primary[4], primary[2], primary[3])) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10), 
                   limits = c("Extremely likely", "Somewhat likely", "Somewhat unlikely", "Extremely unlikely")) +
  geom_text(stat='count', aes(label=percent((..count..)/sum((..count..)))), vjust=-1) +
  ylim(c(0, 700)) + 
  xlab("Likelihood of enrolling for Fall 2020") + 
  ylab("Count") + 
  labs(title="Are students enrolling in the Fall?") +
  theme_hodp() +
  theme(legend.position = "none")
grid::grid.raster(logo, x = 0.01, y = 0.01, just = c('left', 'bottom'), width = unit(1.5, 'cm'))

# Students enrolling by class year
## X axis as class year and fill as enrollment or other way around?

# Predicted overall enrollment density
ggplot(data=subset(df, !is.na(df$enroll_percent_1)), aes(x = enroll_percent_1, color = primary[1])) +
  geom_density(bw=5, alpha=0.25, fill=primary[1]) + 
  geom_vline(aes(xintercept=mean(enroll_percent_1)),
             color=primary[4], linetype = "dashed", size=1) +
  ylab("Density") + 
  xlab("Predicted overall enrollment") + 
  labs(title="Predicted overall enrollment", subtitle=paste("Mean=", round(mean(df$enroll_percent_1, na.rm=T), 1)), collapse="") +
  theme_hodp() +
  theme(legend.position = "none")

# Predicted enrollment in same class density
ggplot(data=subset(df, year != "Other" & year != "" & !is.na(enroll_percent_1)), aes(x = enroll_percent_2, color=year)) +
  geom_density(bw=10, size=2, alpha = 0.2) + 
  scale_color_manual(values = primary) +
  ylab("Density") + 
  xlab("Predicted enrollment in same class") + 
  labs(title="Predicted enrollment by class", color = "Year") +
  theme_hodp()

likelihoods <- c("Extremely likely", "Somewhat likely", "Somewhat unlikely", "Extremely unlikely")

enroll_means_df <- data.frame(
  "enroll_label"=factor(likelihoods),
  "enroll_mean_1"=Vectorize((function(x) mean(filter_nas(df$enroll_percent_1[df$enroll == x]))))(likelihoods),
  "enroll_se_1"=Vectorize(function(x) se(filter_nas(df$enroll_percent_1[df$enroll == x])))(likelihoods),
  "enroll_mean_2"=Vectorize((function(x) mean(filter_nas(df$enroll_percent_2[df$enroll == x]))))(likelihoods),
  "enroll_se_2"=Vectorize(function(x) se(filter_nas(df$enroll_percent_2[df$enroll == x])))(likelihoods)
)
# Mean expected proportion returning overall
ggplot(data=enroll_means_df, aes(x=enroll_label, y=enroll_mean_1)) + 
  geom_bar(stat = "identity", aes(fill=enroll_label)) +
  scale_fill_manual(values = c(primary[1], primary[4], primary[2], primary[3])) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10), 
                   limits = c("Extremely likely", "Somewhat likely", "Somewhat unlikely", "Extremely unlikely")) +
  geom_text(stat='identity', aes(label=percent(enroll_mean_1/100)), vjust=-2) +
  geom_errorbar(aes(ymin=enroll_mean_1 - 1.96 * enroll_se_1, ymax=enroll_mean_1 + 1.96 * enroll_se_1),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  ylim(c(0, 80)) + 
  xlab("Likelihood of enrolling for Fall 2020") + 
  ylab("Mean predicted overall enrollment") + 
  labs(title="Students enrollment vs predicted overall enrollment") +
  theme_hodp() +
  theme(legend.position = "none")
grid::grid.raster(logo, x = 0.01, y = 0.01, just = c('left', 'bottom'), width = unit(1.5, 'cm'))

# Mean expected proportion returning in class year
ggplot(data=enroll_means_df, aes(x=enroll_label, y=enroll_mean_2)) + 
  geom_bar(stat = "identity", aes(fill=enroll_label)) +
  scale_fill_manual(values = c(primary[1], primary[4], primary[2], primary[3])) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10), 
                   limits = c("Extremely likely", "Somewhat likely", "Somewhat unlikely", "Extremely unlikely")) +
  geom_text(stat='identity', aes(label=percent(enroll_mean_2/100)), vjust=-2) +
  geom_errorbar(aes(ymin=enroll_mean_2 - 1.96 * enroll_se_2, ymax=enroll_mean_2 + 1.96 * enroll_se_2),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  ylim(c(0, 80)) + 
  xlab("Likelihood of enrolling for Fall 2020") + 
  ylab("Mean predicted enrollment in class year") + 
  labs(title="Students enrollment vs predicted class enrollment") +
  theme_hodp() +
  theme(legend.position = "none")
grid::grid.raster(logo, x = 0.01, y = 0.01, just = c('left', 'bottom'), width = unit(1.5, 'cm'))

  
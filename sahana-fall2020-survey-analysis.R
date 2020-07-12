source('styleguide.R')
library(scales)
library(stringr)
library(reshape2)
library(plyr); library(dplyr)
# Install
# install.packages("tm")  # for text mining
# install.packages("SnowballC") # for text stemming
# install.packages("wordcloud") # word-cloud generator 
# install.packages("RColorBrewer") # color palettes
# http://www.sthda.com/english/wiki/text-mining-and-word-cloud-fundamentals-in-r-5-simple-steps-you-should-know#:~:text=The%20text%20mining%20package%20(tm,keywords%20as%20a%20word%20cloud.
# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

df <- read.csv("survey-data-20-07-08.csv")

se <- function(x) sqrt(var(x) / length(x))

filter_nas <- function(x) x[!is.na(x) & x != ""]

likelihoods <- c("Extremely likely", "Somewhat likely", "Somewhat unlikely", "Extremely unlikely")

####################
#### ENROLLMENT ####
####################


# Enrolling by year with percentages
hi <- select(df, year, enroll)
hi <- hi[hi$year != "" & hi$year != "Other" & !is.na(hi$enroll) & hi$enroll != "" & !is.na(hi$year), ]
table(hi)

year <- c(rep(c("2021", "2022", "2023", "2024"), each = 4))
cat <- c(rep(c("Extremely unlikely", "Somewhat unlikely", "Somewhat likely", "Extremely likely"), times =4))
a <- 230; b <- 228; c <- 242; d <- 376;
freq <- c(55/a, 30/a, 31/a, 114/a, 60/b, 26/b, 43/b, 99/b, 36/c, 40/c, 56/c, 110/c, 8/d, 24/d, 75/d, 269/d)
enroll_clean_data <- data.frame(year, cat, freq)

ggplot(enroll_clean_data, aes(x=year, y=freq, fill=cat, label=percent(freq))) + 
  geom_bar(stat = "identity") + 
  geom_text(size = 3, position = position_stack(vjust = 0.5)) + 
  scale_fill_manual(values = primary) +
  theme_hodp() + 
  xlab("Class Year") + 
  ylab("Proportion returning") + 
  labs(title="Students returning by year", fill = "Class Year")
grid::grid.raster(logo, x = 0.01, y = 0.01, just = c('left', 'bottom'), width = unit(1.5, 'cm'))

# Students enrolling by class year
## would someone like to take a shot at labeling this?
ggplot(data=subset(df, (!is.na(enroll) & enroll != "" & year != "Other" & year != "" & !is.na(year))), aes(x=factor(year))) + 
  geom_bar(aes(fill = factor(enroll, levels = likelihoods), y = ..count../tapply(..count.., ..x.. ,sum)[..x..])) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_fill_manual(values = primary) +
  theme_hodp() + 
  xlab("Class Year") + 
  ylab("Proportion returning") + 
  labs(title="Students returning by year", fill = "Class Year")
grid::grid.raster(logo, x = 0.01, y = 0.01, just = c('left', 'bottom'), width = unit(1.5, 'cm'))

likely_enrolled_prop <- sum(df$enroll == "Extremely likely" | df$enroll == "Somewhat likely") / (sum(df$enroll != ""))


# Where would people be taking classes from if off-campus? location_off_campus
ggplot(data=subset(df, (!is.na(location_off_campus) & location_off_campus != "")), aes(x=factor(location_off_campus))) + 
  geom_bar(aes(fill = factor(location_off_campus))) + 
  scale_fill_manual(values = c(primary[3], primary[1], primary[2], primary[4])) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20), 
                   limits = c("Family Home", "Personal Apartment/Home (not with Harvard friends)", "Living with Harvard friends", "Other")) +
  geom_text(stat='count', aes(label=percent((..count..)/sum((..count..)))), vjust=-1) +
  xlab("Location") + 
  ylab("Count") + 
  labs(title="Where would off-campus students live?") +
  theme_hodp() +
  theme(legend.position = "none")
grid::grid.raster(logo, x = 0.01, y = 0.01, just = c('left', 'bottom'), width = unit(1.5, 'cm'))


# What do you think of the number of students allowed on campus in Fall 2020? opinion_eligibility
ggplot(data=subset(df, (!is.na(opinion_eligibility) & opinion_eligibility != "")), aes(x=factor(opinion_eligibility))) + 
  geom_bar(aes(fill = factor(opinion_eligibility))) + 
  scale_fill_manual(values = c(primary[3], primary[1], primary[2], primary[4], primary[5])) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20), 
                   limits = c("Allows too few students back on campus", "Just right", "Number is just right, but it should be a different cohort in the Fall", "Allows too many students back on campus", "Don't know enough to say")) +
  geom_text(stat='count', aes(label=percent((..count..)/sum((..count..)))), vjust=-1) +
  xlab("Opinion") + 
  ylab("Count") + 
  labs(title="Opinion on Number of Students Allowed Back") +
  theme_hodp() +
  theme(legend.position = "none")
grid::grid.raster(logo, x = 0.01, y = 0.01, just = c('left', 'bottom'), width = unit(1.5, 'cm'))

# opinion_eligibility by year
opinions <- c("Allows too few students back on campus", "Just right", "Number is just right, but it should be a different cohort in the Fall", "Allows too many students back on campus", "Don't know enough to say")
opinions
ggplot(data=subset(df, (!is.na(opinion_eligibility) & opinion_eligibility != " " & opinion_eligibility != "" & opinion_eligibility != "NA" & year != "Other" & year != "" & !is.na(year))), aes(x=factor(year))) + 
  geom_bar(aes(fill = factor(opinion_eligibility, levels = opinions), y = ..count../tapply(..count.., ..x.. ,sum)[..x..])) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_fill_manual(values = primary) +
  theme_hodp() + 
  xlab("Class Year") + 
  ylab("Percentage with Opinion") + 
  theme(legend.position="bottom", legend.direction="vertical") + 
  labs(title="Student opinion by year", fill="Opinion on Fall 2020 Campus Policy")
grid::grid.raster(logo, x = 0.01, y = 0.01, just = c('left', 'bottom'), width = unit(1.5, 'cm'))


classyears <- c("2021", "2022", "2023", "2024")
ratings <- data.frame(
  "years"=factor(classyears),
  "rating_mean"=Vectorize((function(x) mean(filter_nas(df$rating_overall[df$year == x]))))(classyears),
  "rating_se"=Vectorize(function(x) se(filter_nas(df$rating_overall[df$year == x])))(classyears)
)

# average decision rating by year
ggplot(data=ratings, aes(x=years, y=rating_mean)) + 
  geom_bar(stat = "identity", aes(fill=years)) +
  scale_fill_manual(values = c(primary[1], primary[4], primary[2], primary[3])) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10), 
                   limits = classyears) +
  geom_text(stat='identity', aes(label=round(rating_mean, digits=2)), vjust=-2.3) +
  geom_errorbar(aes(ymin=rating_mean - 1.96 * rating_se, ymax=rating_mean + 1.96 * rating_se),
                width=.2,                    # Width of the error bars
                position=position_dodge(0.9)) +
  ylim(c(0, 10)) + 
  xlab("Class Year") + 
  ylab("Mean rating of Fall 2020 Plan") + 
  labs(title="Rating of Harvard's Plan by Class Year") +
  theme_hodp() +
  theme(legend.position = "none")
grid::grid.raster(logo, x = 0.01, y = 0.01, just = c('left', 'bottom'), width = unit(1.5, 'cm'))

# university's plans do you prefer
ggplot(data=subset(df, (!is.na(other_school) & other_school != "Other (please specify)" & other_school != "Don't know enough to say" & other_school != "")), aes(x=factor(other_school))) + 
  geom_bar(aes(fill = factor(other_school))) + 
  scale_fill_manual(values = c("#B31B1B", "#00693e", "#012169", "#a31f34", "#D41B2C", "#e77500", "#C41E3A", "#00356B")) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  geom_text(stat='count', aes(label=percent((..count..)/sum((..count..)))), vjust=-1) +
  xlab("University") + 
  ylab("Count") + 
  labs(title="Preferred University Fall 2020 Plan") +
  theme_hodp() +
  theme(legend.position = "none")
grid::grid.raster(logo, x = 0.01, y = 0.01, just = c('left', 'bottom'), width = unit(1.5, 'cm'))

# sentiment analysis of free text

# breaking down opinion by international students

# breaking down opinions by financial aid

# financial aid question support_financial_ai
nrow(df)
finaidf <- df[!is.na(df$support_financial_ai) & df$support_financial_ai != "", ]; nrow(finaidf)
notenough <- nrow(finaidf[grepl("Not enough money provided to off-campus students", finaidf$support_financial_ai, fixed = TRUE), ])
individ <- nrow(finaidf[grepl("Policy is not individualized enough", finaidf$support_financial_ai, fixed = TRUE),])
agree <- nrow(finaidf[grepl("Agree with the policy", finaidf$support_financial_ai),])
nrow(finaidf); notenough; individ; agree
notenough

finaid_op <- c(notenough, individ, agree)
finaid_op

finaid_ops <- c("Not enough money provided to off-campus students", "Policy is not individualized enough", "Agree with the policy")
finaid_data = data.frame(finaid_ops, finaid_op)
finaid_data
ggplot(data=finaid_data, aes(x=finaid_ops, y = finaid_op)) + 
  geom_bar(stat = 'identity', aes(fill = finaid_ops)) + 
  scale_fill_manual(values = c(primary[3], primary[1], primary[2])) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20), 
                   limits = finaid_ops) +
  geom_text(aes(label=percent(finaid_op / nrow(finaidf))), vjust = -1.5) +
  xlab("Opinion") + 
  ylab("Count") + 
  labs(title="Financial Aid Policy Opinions") +
  theme_hodp() +
  theme(legend.position = "none")
grid::grid.raster(logo, x = 0.01, y = 0.01, just = c('left', 'bottom'), width = unit(1.5, 'cm'))

# finaid opinions among those on finaid
nrow(df)
fdf <- df[!is.na(df$support_financial_ai) & df$support_financial_ai != "" & df$financial_aid == "Yes", ]; nrow(fdf)
notenough <- nrow(fdf[grepl("Not enough money provided to off-campus students", fdf$support_financial_ai, fixed = TRUE), ])
individ <- nrow(fdf[grepl("Policy is not individualized enough", fdf$support_financial_ai, fixed = TRUE),])
agree <- nrow(fdf[grepl("Agree with the policy", fdf$support_financial_ai),])
nrow(fdf); notenough; individ; agree
notenough

finaid_op <- c(notenough, individ, agree)
finaid_op

finaid_ops <- c("Not enough money provided to off-campus students", "Policy is not individualized enough", "Agree with the policy")
finaid_data = data.frame(finaid_ops, finaid_op)
finaid_data
ggplot(data=finaid_data, aes(x=finaid_ops, y = finaid_op)) + 
  geom_bar(stat = 'identity', aes(fill = finaid_ops)) + 
  scale_fill_manual(values = c(primary[3], primary[1], primary[2])) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20), 
                   limits = finaid_ops) +
  geom_text(aes(label=percent(finaid_op / nrow(fdf))), vjust = -1.5) +
  xlab("Opinion") + 
  ylab("Count") + 
  labs(title="Financial Aid Policy Opinions") +
  theme_hodp() +
  theme(legend.position = "none")
grid::grid.raster(logo, x = 0.01, y = 0.01, just = c('left', 'bottom'), width = unit(1.5, 'cm'))

# most frequent words and word cloud of free text


  
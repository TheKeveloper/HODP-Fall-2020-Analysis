source('styleguide.R')
library(scales)
library(stringr)
library(reshape2)
library(plyr); library(dplyr)

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
  ylim(c(0, 450)) + 
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
  ylim(c(0, 700)) + 
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


# average decision rating by year


# university's plans do you prefer
ggplot(data=subset(df, (!is.na(other_school) & other_school != "Other (please specify)" & other_school != "Don't know enough to say" & other_school != "")), aes(x=factor(other_school))) + 
  geom_bar(aes(fill = factor(other_school))) + 
  scale_fill_manual(values = c(primary[1], primary[2], primary[4], primary[6],primary[3], "#D84742", primary[5], "#760000")) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  geom_text(stat='count', aes(label=percent((..count..)/sum((..count..)))), vjust=-1) +
  ylim(c(0, 250)) + 
  xlab("University") + 
  ylab("Count") + 
  labs(title="Preferred University Fall 2020 Plan") +
  theme_hodp() +
  theme(legend.position = "none")
grid::grid.raster(logo, x = 0.01, y = 0.01, just = c('left', 'bottom'), width = unit(1.5, 'cm'))

# sentiment analysis of free text

# most frequent words and word cloud of free text
  
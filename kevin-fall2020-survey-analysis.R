source('styleguide.R')
library(scales)
library(stringr)
library(reshape2)
library(plyr)

df <- read.csv("survey-data-20-07-10.csv")

se <- function(x) sqrt(var(x) / length(x))

filter_nas <- function(x) x[!is.na(x) & x != ""]

likelihoods <- c("Extremely likely", "Somewhat likely", "Somewhat unlikely", "Extremely unlikely")

####################
#### ENROLLMENT ####
####################

# Basic bar chart of whether students are enrolling
ggplot(data=subset(df, (!is.na(enroll) & enroll != "")), aes(x=factor(enroll))) + 
  geom_bar(aes(fill = factor(enroll))) + 
  scale_fill_manual(values = c(primary[1], primary[4], primary[2], primary[3])) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10), 
                   limits = c("Extremely likely", "Somewhat likely", "Somewhat unlikely", "Extremely unlikely")) +
  geom_text(stat='count', aes(label=percent((..count..)/sum((..count..)))), vjust=-1) +
  ylim(c(0, 850)) + 
  xlab("Likelihood of enrolling for Fall 2020") + 
  ylab("Count") + 
  labs(title="Are students enrolling in the Fall?") +
  theme_hodp() +
  theme(legend.position = "none")
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
# Predicted overall enrollment density
ggplot(data=subset(df, !is.na(df$enroll_percent_1)), aes(x = enroll_percent_1, color = primary[1])) +
  geom_density(bw=6, alpha=0.25, fill=primary[1], size = 2) + 
  scale_x_continuous(breaks = 0:10 * 10) + 
  geom_vline(aes(xintercept=likely_enrolled_prop * 100),
             color=primary[4], linetype = "dashed", size=1) +
  ylab("Density") + 
  xlab("Predicted overall enrollment") + 
  labs(title="Predicted overall enrollment", subtitle=paste("Mean=", round(mean(df$enroll_percent_1, na.rm=T), 1), 
                                                            ", Median=", round(median(df$enroll_percent_1, na.rm=T), 1),
                                                            ", Reported likely enroll=", round(likely_enrolled_prop, 2) * 100, collapse="")) +
  theme_hodp() +
  theme(legend.position = "none")
grid::grid.raster(logo, x = 0.01, y = 0.01, just = c('left', 'bottom'), width = unit(1.5, 'cm'))

# Predicted enrollment in same class density
ggplot(data=subset(df, year != "Other" & year != "" & !is.na(enroll_percent_1)), aes(x = enroll_percent_2, color=year)) +
  geom_density(bw=10, size=2, alpha = 0.2) + 
  scale_color_manual(values = primary) +
  ylab("Density") + 
  xlab("Predicted enrollment in same class") + 
  labs(title="Predicted enrollment by class", color = "Year") +
  theme_hodp()

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

# How many semesters will people take off
ggplot(data=subset(df, (!is.na(semesters_off) & semesters_off != "")), aes(x=factor(semesters_off))) + 
  geom_bar(aes(fill = factor(semesters_off))) + 
  scale_fill_manual(values = primary) + 
  scale_x_discrete(labels = str_wrap(c("Fall only", "Fall, maybe Spring", "Fall and Spring", "Longer"), width = 20),
                   limits = c("Fall only", "Fall, and maybe Spring depending on policy", "Fall and Spring", "Longer")) +
  geom_text(stat='count', aes(label=percent((..count..)/sum((..count..)))), vjust=-1) +
  ylim(c(0, 400)) + 
  xlab("Semesters off") + 
  ylab("Count") + 
  labs(title="How many semesters off?") +
  theme_hodp() +
  theme(legend.position = "none")
grid::grid.raster(logo, x = 0.01, y = 0.01, just = c('left', 'bottom'), width = unit(1.5, 'cm'))

# What are people doing?
ggplot(data=subset(df, (!is.na(semester_off_plans) & semester_off_plans != "")), aes(x=factor(semester_off_plans))) + 
  geom_bar(aes(fill = factor(semester_off_plans))) + 
  scale_fill_manual(values = c(primary[5], primary[1], primary[6], primary[2], primary[4], primary[3])) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20), 
                   limits = c("Internship/Industry Work", "Research", "Volunteering", "Travel", "Chill at home", "Other")) +
  geom_text(stat='count', aes(label=percent((..count..)/sum((..count..)))), vjust=-1) +
  ylim(c(0, 450)) + 
  xlab("Plan") + 
  ylab("Count") + 
  labs(title="What would students do during semester off?") +
  theme_hodp() +
  theme(legend.position = "none")
grid::grid.raster(logo, x = 0.01, y = 0.01, just = c('left', 'bottom'), width = unit(1.5, 'cm'))



#############################
#### RETURNING TO CAMPUS ####
#############################

# Applying to return to campus
ggplot(data=subset(df, (!is.na(petition) & petition != "")), aes(x=factor(petition))) + 
  geom_bar(aes(fill = factor(petition))) + 
  scale_fill_manual(values = c(rev(sidebysidebarplot))) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10), 
                   limits = c("Yes", "No")) +
  geom_text(stat='count', aes(label=percent((..count..)/sum((..count..)))), vjust=-1) +
  ylim(c(0,700)) + 
  xlab("Petitioning") + 
  ylab("Count") + 
  labs(title="How many students are petitioning?") +
  theme_hodp() +
  theme(legend.position = "none")
grid::grid.raster(logo, x = 0.01, y = 0.01, just = c('left', 'bottom'), width = unit(1.5, 'cm'))

# Categories satisfied
criteria_labels <- c(
  "Lack of good computer",
  "Internet speed below 5 Mbps",
  "Lack of a quiet place to work",
  "Lack of dedicated time to devote to academics",
  "Living in a distant time zone",
  "Home circumstances that deeply affect your mental well-being",
  "Shelter and/or food insecurities",
  "Need for accessible learning resources on campus not available remotely",
  "Planned Senior thesis that cannot be done remotely", 
  "Enrollment in a joint program with another Boston-area institution"
)

criteria_options <- paste("criteria_", 1:10, sep = "")

criteria_df <-data.frame("criteria" = rep(criteria_options, times=2), 
                         "satisfied" = c(rep("No", times=length(criteria_options)),
                                        rep("Yes", times=length(criteria_options))),
                         "prop" = rep(0, times=2*length(criteria_options)))

for (row in 1:nrow(criteria_df)) {
  current_criteria <- criteria_df[row, "criteria"]
  current_satisfied <- criteria_df[row, "satisfied"]
  v <- df[current_criteria]
  criteria_df[row, "count"] <- table(v)[current_satisfied]
  criteria_df[row, "prop"] <- criteria_df[row, "count"] / length(v[!is.na(v) & v != "" & v != "Unsure or prefer not to say"])
}

ggplot(criteria_df, aes(x=factor(criteria, levels=rev(criteria_options)), y=prop, fill=factor(satisfied, levels=c("No", "Yes", "Unsure or prefer not to say")))) +
  geom_bar(stat="identity") +
  xlab('') +
  ylab('Percent') +
  ggtitle("Which criteria do students satisfy") +
  scale_x_discrete(labels=str_wrap(rev(criteria_labels), width=30)) +
  scale_fill_manual(values=primary[1:2]) +
  guides(fill = guide_legend(reverse = TRUE)) +
  coord_flip() +
  geom_text(aes(label=scales::percent(prop, accuracy=1)), position="stack", hjust=1, size=4) +
  theme_hodp() +
  theme(legend.title = element_blank(), 
       axis.text.y =element_text(size=10,  family="Helvetica"))
       # plot.title = element_text(size=20,  family="Helvetica", face = "bold", margin = margin(t = 0, r = 0, b = 10, l = 0)))

df$criteria_count <- (df$criteria_1 == "Yes") + (df$criteria_2  == "Yes") + (df$criteria_3 == "Yes") + (df$criteria_4 == "Yes") + (df$criteria_5 == "Yes") + 
                      (df$criteria_6 == "Yes") + (df$criteria_7 == "Yes") + (df$criteria_8 == "Yes") + (df$criteria_9 == "Yes") + (df$criteria_10 == "Yes")

# How likely allowed to return
ggplot(data=subset(df, (!is.na(allowed_return) & allowed_return != "")), aes(x=factor(allowed_return))) + 
  geom_bar(aes(fill = factor(allowed_return))) + 
  scale_fill_manual(values = c(primary[1], primary[4], primary[2], primary[3])) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20), 
                   limits = likelihoods) +
  geom_text(stat='count', aes(label=percent((..count..)/sum((..count..)))), vjust=-1) +
  ylim(c(0, 600)) + 
  xlab("Likelihood allowed on-campus") + 
  ylab("Count") + 
  labs(title="How likely are upperclassmen to be allowed on-campus?") +
  theme_hodp() +
  theme(legend.position = "none")
grid::grid.raster(logo, x = 0.01, y = 0.01, just = c('left', 'bottom'), width = unit(1.5, 'cm'))

# How likely to return if allowed
ggplot(data=subset(df, (!is.na(on_campus) & on_campus != "")), aes(x=factor(on_campus))) + 
  geom_bar(aes(fill = factor(on_campus))) + 
  scale_fill_manual(values = c(primary[1], primary[4], primary[2], primary[3])) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20), 
                   limits = likelihoods) +
  geom_text(stat='count', aes(label=percent((..count..)/sum((..count..)))), vjust=-1) +
  ylim(c(0, 500)) + 
  xlab("Likelihood to return if allowed") + 
  ylab("Count") + 
  labs(title="How likely are students to return if allowed?") +
  theme_hodp() +
  theme(legend.position = "none")
grid::grid.raster(logo, x = 0.01, y = 0.01, just = c('left', 'bottom'), width = unit(1.5, 'cm'))

# Percent infected by likelihood to return
infected_means_df <- data.frame(
  "on_campus_label"=factor(likelihoods),
  "infected_mean"=Vectorize((function(x) mean(filter_nas(df$percent_infected_1[df$on_campus == x]))))(likelihoods),
  "infected_se"=Vectorize(function(x) se(filter_nas(df$percent_infected_1[df$on_campus == x])))(likelihoods)
)
# Mean expected proportion returning overall
ggplot(data=infected_means_df, aes(x=on_campus_label, y=infected_mean)) + 
  geom_bar(stat = "identity", aes(fill=on_campus_label)) +
  scale_fill_manual(values = c(primary[1], primary[4], primary[2], primary[3])) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20), 
                   limits = likelihoods) +
  geom_text(stat='identity', aes(label=percent(infected_mean/100)), vjust=-1, hjust=-0.7) +
  geom_errorbar(aes(ymin=infected_mean - 1.96 * infected_se, ymax=infected_mean + 1.96 * infected_se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  ylim(c(0, 40)) + 
  xlab("Likelihood of returning to campus if allowed") + 
  ylab("Mean predicted proportion on-campus infected") + 
  labs(title="Return to campus vs predicted proportion infected") +
  theme_hodp() +
  theme(legend.position = "none")
grid::grid.raster(logo, x = 0.01, y = 0.01, just = c('left', 'bottom'), width = unit(1.5, 'cm'))

likely_on_campus_prop <- sum((df$on_campus == "Extremely likely" | df$on_campus == "Somewhat likely")) / (sum(df$enroll != "" & df$enroll != "Extremely unlikely"))
# Percent of students on-campus
ggplot(data=subset(df, !is.na(on_campus_percent_1)), aes(x = on_campus_percent_1, color = primary[1])) +
  geom_density(bw=6, alpha=0.25, fill=primary[1], size = 2) + 
  scale_x_continuous(breaks = 0:10 * 10) + 
  geom_vline(aes(xintercept=likely_on_campus_prop * 100),
             color=primary[4], linetype = "dashed", size=1) +
  ylab("Density") + 
  xlab("Predicted proportion of enrolled students on-campus") + 
  labs(title="What proportion of enrolled students will be on-campus?", subtitle=paste("Mean=", round(mean(df$on_campus_percent_1, na.rm=T), 1), 
                                                                                       ", Median=", round(median(df$on_campus_percent_1, na.rm=T), 1),
                                                                                       ", Reported likely on-campus=", round(likely_on_campus_prop, 2) * 100,
                                                                                       collapse=""))  +
  theme_hodp() +
  theme(legend.position = "none")

#############################
#### EVALUATING POLICIES ####
#############################
ggplot(data=subset(df, !is.na(df$percent_infected_1)), aes(x = percent_infected_1, color = primary[1])) +
  geom_density(bw=6, alpha=0.25, fill=primary[1], size = 2) + 
  scale_x_continuous(breaks = 0:10 * 10) + 
  # geom_vline(aes(xintercept=mean(percent_infected_1)),
  #           color=primary[4], linetype = "dashed", size=1) +
  ylab("Density") + 
  xlab("Predicted proportion of students on-campus infected") + 
  labs(title="What proportion of students on-campus will be infected?", subtitle=paste("Mean=", round(mean(df$percent_infected_1, na.rm=T), 1), 
                                                                                       ", Median=", round(median(df$percent_infected_1, na.rm=T), 1), collapse=""))  +
  theme_hodp() +
  theme(legend.position = "none")
grid::grid.raster(logo, x = 0.01, y = 0.01, just = c('left', 'bottom'), width = unit(1.5, 'cm'))
  
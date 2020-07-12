source('styleguide.R')
library(scales)
library(stringr)
library(reshape2)
library(plyr)

df <- read.csv("survey-data-20-07-11.csv")

se <- function(x) sqrt(var(x) / length(x))

filter_nas <- function(x) x[!is.na(x) & x != ""]

likelihoods <- c("Extremely likely", "Somewhat likely", "Somewhat unlikely", "Extremely unlikely")

####################
#### ENROLLMENT ####
####################

# Basic bar chart of whether students are enrolling
# ggplot(data=subset(df, (!is.na(enroll) & enroll != "")), aes(x=factor(enroll))) + 
#   geom_bar(aes(fill = factor(enroll))) + 
#   scale_fill_manual(values = c(primary[1], primary[4], primary[2], primary[3])) + 
#   scale_x_discrete(labels = function(x) str_wrap(x, width = 10), 
#                    limits = c("Extremely likely", "Somewhat likely", "Somewhat unlikely", "Extremely unlikely")) +
#   geom_text(stat='count', aes(label=percent((..count..)/sum((..count..)))), vjust=-1) +
#   xlab("Likelihood of enrolling for Fall 2020") + 
#   ylab("Count") + 
#   labs(title="Are students enrolling in the Fall?") +
#   theme_hodp() +
#   theme(legend.position = "none")
# grid::grid.raster(logo, x = 0.01, y = 0.01, just = c('left', 'bottom'), width = unit(1.5, 'cm'))

# Students enrolling by class year

enroll_df <- data.frame(
  enroll=rep(likelihoods, 5),
  year=rep(c("Overall", "2024", "2023", "2022", "2021"), 4),
  prop=rep(0, 20)
)
for (row in 1:nrow(enroll_df)) {
  current_enroll <- enroll_df[row, "enroll"]
  current_year <- enroll_df[row, "year"]
  if(current_year == "Overall") {
    enroll_df[row, "prop"] <- sum(df$enroll == current_enroll) / sum(df$enroll != "")
  }
  else{
    enroll_df[row, "prop"] <- sum(df$year == current_year & df$enroll == current_enroll) / sum(df$year==current_year & df$enroll != "")
  }
}

ggplot(enroll_df, aes(x = factor(year, levels = c("Overall", "2024", "2023", "2022", "2021")), 
                                 y = prop, 
                                fill = factor(enroll, levels = rev(likelihoods)), 
                                    label = percent(prop))) +
  geom_bar(stat = "identity") +
  geom_text(size = 3, position = position_stack(vjust = 0.5)) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_fill_manual(values = c(primary[5], primary[3], primary[2], primary[1]), guide = guide_legend(reverse = T)) +
  theme_hodp() + 
  xlab("Year") + 
  ylab("Proportion returning") + 
  labs(title="Students returning by year", fill = "Likelihood of enrolling")
grid::grid.raster(logo, x = 0.01, y = 0.01, just = c('left', 'bottom'), width = unit(1.5, 'cm'))

# Simulating the enrollment
## ASSUMPTIONS:
## Within each class year, coming back proportion is representative
## Number of people on college facebook in each class year is correct
## Assumptions on what "Likely" and "Very likely" mean specified below
sim_enrollment_prop <- function(df, enroll_probs = c(1, 0.75, 0.25, 0)) {
  sample_size <- nrow(df)
  years <- c("2024", "2023", "2022", "2021")
  year_props <- c(0.2386706949, 0.2536253776, 0.2527190332, 0.2549848943)
  names(year_props) <- years
  ns <- round(year_props * sample_size)
  names(ns) <- years
  names(enroll_probs) <- likelihoods
  year_responses <- list(
    "2024" = df$enroll[df$year == 2024], 
    "2023" = df$enroll[df$year == 2023],
    "2022" = df$enroll[df$year == 2022],
    "2021" = df$enroll[df$year == 2021]
  )
  predicted_enrollments <- c(
    "2024" = 0,
    "2023" = 0,
    "2022" = 0,
    "2021" = 0
  )
  sim_individual <- Vectorize(function(x) rbinom(1, 1, enroll_probs[[x]]))
  for (year in years) {
    sample_responses <- sample(year_responses[[year]], ns[[year]], replace = T)
    predicted_enrollments[[year]] <- sum(sim_individual(sample_responses))
  }
  return(sum(predicted_enrollments) / sum(ns))
}

sim_props <- replicate(1000, sim_enrollment_prop(df, enroll_probs=c(0.99, 0.75, 0.25, 0.01)))


ggplot(data=data.frame("sim_enroll" = sim_props), aes(x = sim_enroll, color = primary[1])) +
  geom_density(bw = 0.01, alpha=0.25, fill=primary[1], size = 2) + 
  ylab("Density") + 
  xlab("Simulated overall enrollment") + 
  labs(title="Simulated overall enrollment", subtitle=paste("Sims=", length(sim_props), ", Mean=", round(mean(sim_props) * 100, 1), sep = "")) +
  theme_hodp() +
  theme(legend.position = "none")

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

groups_enroll_df <- data.frame(
  enroll=rep(likelihoods, 3),
  category=rep(c("Overall", "Financial Aid", "International"), 4),
  prop=rep(0, 12)
)
for (row in 1:nrow(groups_enroll_df)) {
  current_enroll <- groups_enroll_df[row, "enroll"]
  current_category <- groups_enroll_df[row, "category"]
  current_label <- tolower(str_replace(current_category, " ", "_" ))
  if(current_category == "Overall") {
    groups_enroll_df[row, "prop"] <- sum(df$enroll == current_enroll) / sum(df$enroll != "")
  }
  else{
    groups_enroll_df[row, "prop"] <- sum(df[current_label] == "Yes" & df$enroll == current_enroll) / sum(df[current_label] == "Yes" & df$enroll != "")
  }
}

ggplot(groups_enroll_df, aes(x = factor(category, levels = c("Overall", "Financial Aid", "International")), 
                      y = prop, 
                      fill = factor(enroll, levels = rev(likelihoods)), 
                      label = percent(prop))) +
  geom_bar(stat = "identity") +
  geom_text(size = 3, position = position_stack(vjust = 0.5)) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_fill_manual(values = c(primary[5], primary[3], primary[2], primary[1]), guide = guide_legend(reverse = T)) +
  theme_hodp() + 
  xlab("Status") + 
  ylab("Proportion returning") + 
  labs(title="Enrollment by international and financial aid status", fill = "Likelihood of enrolling")

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

criteria_df <-data.frame("criteria" = criteria_options, 
                         "satisfied" = rep("Yes", times=length(criteria_options)),
                         "prop" = rep(0, times=length(criteria_options)))

for (row in 1:nrow(criteria_df)) {
  current_criteria <- criteria_df[row, "criteria"]
  current_satisfied <- criteria_df[row, "satisfied"]
  v <- df[current_criteria]
  criteria_df[row, "count"] <- table(v)[current_satisfied]
  criteria_df[row, "prop"] <- criteria_df[row, "count"] / length(v[!is.na(v) & v != "" & v != "Unsure or prefer not to say"])
}

ggplot(criteria_df, aes(x=factor(criteria, levels=rev(criteria_options)), y=prop, fill=factor(satisfied))) +
  geom_bar(stat="identity") +
  xlab('') +
  ylab('Percent Yes') +
  ggtitle("Which criteria do students satisfy") +
  scale_x_discrete(labels=str_wrap(rev(criteria_labels), width=30)) +
  scale_fill_manual(values=primary[1]) +
  coord_flip() +
  geom_text(aes(label=percent(prop, accuracy=1)), hjust = -0.25) +
  theme_hodp() +
  theme(legend.position = "none") +
  theme(legend.title = element_blank(), 
       axis.text.y =element_text(size=10,  family="Helvetica"))
       # plot.title = element_text(size=20,  family="Helvetica", face = "bold", margin = margin(t = 0, r = 0, b = 10, l = 0)))

df$criteria_count <- (df$criteria_1 == "Yes") + (df$criteria_2  == "Yes") + (df$criteria_3 == "Yes") + (df$criteria_4 == "Yes") + (df$criteria_5 == "Yes") + 
                      (df$criteria_6 == "Yes") + (df$criteria_7 == "Yes") + (df$criteria_8 == "Yes") + (df$criteria_9 == "Yes") + (df$criteria_10 == "Yes")

sum(df$criteria_count == 0 & df$criteria_1 != "") / sum(df$criteria_1 != "")

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

# where live if not on campus
ggplot(data=subset(df, (!is.na(location_off_campus) & location_off_campus != "")), aes(x=factor(location_off_campus))) + 
  geom_bar(aes(fill = factor(location_off_campus))) + 
  scale_fill_manual(values = c(primary[1], primary[4], primary[2], primary[3])) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20), 
                   limits = c("Family Home", "Living with Harvard friends", "Personal Apartment/Home (not with Harvard friends)", "Other")) +
  geom_text(stat='count', aes(label=percent((..count..)/sum((..count..)))), vjust=-1) +
  xlab("Off-campus location") + 
  ylab("Count") + 
  labs(title="Where will students live off campus?") +
  theme_hodp() +
  theme(legend.position = "none")
grid::grid.raster(logo, x = 0.01, y = 0.01, just = c('left', 'bottom'), width = unit(1.5, 'cm'))

#############################
#### EVALUATING POLICIES ####
#############################
# Overall opinion on Harvard policy
ggplot(data=subset(df, !is.na(rating_overall_1)), aes(x=rating_overall_1)) +
  geom_histogram(bins = 10, fill = primary[1]) + 
  theme_hodp() +
  labs(title="Overall rating of Fall 2020 policy", subtitle=paste("Mean:", round(mean(df$rating_overall_1, na.rm = T), 2), collapse="")) +
  scale_x_continuous(breaks = 1:10) +
  ylab("Count") +
  xlab("Rating")
grid::grid.raster(logo, x = 0.01, y = 0.01, just = c('left', 'bottom'), width = unit(1.3, 'cm'))

# Opinion on number of students eligible
ggplot(data=subset(df, (!is.na(opinion_eligibility) & opinion_eligibility != "" & opinion_eligibility != "Don't know enough to say")), 
       aes(x=factor(opinion_eligibility))) + 
  geom_bar(aes(fill = factor(opinion_eligibility))) + 
  scale_fill_manual(values = c(primary[1], primary[4], primary[2], primary[3])) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20), 
                   limits = c("Allows too few students back on campus", "Just right", "Number is just right, but it should be a different cohort in the Fall", "Allows too many students back on campus")) +
  geom_text(stat='count', aes(label=percent((..count..)/sum((..count..)))), vjust=-1) +
  ylim(c(0, 800)) + 
  xlab("Opinion on number of students eligible for on-campus") + 
  ylab("Count") + 
  labs(title="Are enough students eligible to return to campus?") +
  theme_hodp() +
  theme(legend.position = "none")
grid::grid.raster(logo, x = 0.01, y = 0.01, just = c('left', 'bottom'), width = unit(1.5, 'cm'))

# opinion of precautions taken
ggplot(data=subset(df, (!is.na(opinion_precautions) & opinion_precautions != "" & opinion_precautions != "Don't know enough to say")), 
       aes(x=factor(opinion_precautions))) + 
  geom_bar(aes(fill = factor(opinion_precautions))) + 
  scale_fill_manual(values = c(primary[2], primary[3], primary[1])) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 30), 
                   limits = c("Too many precautionary measures taken", "Just right", "Too few precautionary measures taken")) +
  geom_text(stat='count', aes(label=percent((..count..)/sum((..count..)))), vjust=-1) +
  xlab("Opinion on number of precautions taken on-campus") + 
  ylab("Count") + 
  labs(title="Are enough precautions taken on-campus?") +
  theme_hodp() +
  theme(legend.position = "none")
grid::grid.raster(logo, x = 0.01, y = 0.01, just = c('left', 'bottom'), width = unit(1.5, 'cm'))

# density plot of predicted infection proportion
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

# opinions on specific concerns
concern_labels <- c(
  "Quality of remote course instruction",
  "Student mental health",
  "On-campus social interactions",
  "Off-campus social interactions",
  "Extracurriculars",
  "Equity of access to resources for school"
)

concern_options <- paste("address_concerns_", 1:6, sep = "")

concerns_df <-data.frame("concern" = rep(concern_options, times=4), 
                         "quality" = c(rep("Very poorly ", times=length(concern_options)),
                                       rep("Somewhat poorly ", times=length(concern_options)),
                                       rep("Somewhat well", times=length(concern_options)),
                                       rep("Very well", times=length(concern_options))),
                         "prop" = rep(0, times=4*length(concern_options)))

for (row in 1:nrow(concerns_df)) {
  current_concern <- concerns_df[row, "concern"]
  current_quality <- concerns_df[row, "quality"]
  v <- df[current_concern]
  concerns_df[row, "count"] <- table(v)[current_quality]
  concerns_df[row, "prop"] <- concerns_df[row, "count"] / length(v[!is.na(v) & v != ""])
}

# Do we need percentage labels? There's not a good way of making them look nice
ggplot(concerns_df, aes(x=factor(concern, levels=rev(concern_options)), y=prop, 
                        fill=factor(quality, levels=c("Very poorly ", "Somewhat poorly ", "Somewhat well", "Very well")))) +
  geom_bar(stat="identity") +
  xlab('') +
  ylab('Percent') +
  ggtitle("How well did Harvard address specific concerns?") +
  scale_x_discrete(labels=str_wrap(rev(concern_labels), width=20)) +
  scale_fill_manual(values=c(primary[5], primary[3], primary[2], primary[1])) +
  guides(fill = guide_legend(reverse = TRUE)) +
  coord_flip() +
  geom_text(aes(label=scales::percent(prop, accuracy=1)), position="stack", hjust=-0.1, size=4) +
  theme_hodp() +
  theme(legend.title = element_blank(), 
        axis.text.y =element_text(size=10,  family="Helvetica"))
# plot.title = element_text(size=20,  family="Helvetica", face = "bold", margin = margin(t = 0, r = 0, b = 10, l = 0)))
  
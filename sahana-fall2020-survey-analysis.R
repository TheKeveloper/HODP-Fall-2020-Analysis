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

# opinion_eligibility by year - doean't have labels but can add
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

# rating by international student status

intlfinaid_means <- c(mean(df$rating_overall_1[df$international == "Yes"], na.rm = TRUE),
                      mean(df$rating_overall_1[df$international == "No"], na.rm = TRUE),
                      mean(df$rating_overall_1[df$financial_aid == "Yes"], na.rm = TRUE),
                      mean(df$rating_overall_1[df$financial_aid == "No"], na.rm = TRUE))

intlfinaid_se <- c(se(filter_nas(df$rating_overall_1[df$international == "Yes"])),
                      se(filter_nas(df$rating_overall_1[df$international == "No"])),
                      se(filter_nas(df$rating_overall_1[df$financial_aid == "Yes"])),
                      se(filter_nas(df$rating_overall_1[df$financial_aid == "No"])))


intlfinaidstatus <- c("International student", "Not international", "On financial aid", "Not on financial aid")

intlfinaid_ratings <- data.frame(
  "group"=factor(intlfinaidstatus),
  "rating_mean"=intlfinaid_means,
  "rating_se"=intlfinaid_se
)

# average decision rating by finaid / intl status
ggplot(data=intlfinaid_ratings , aes(x=group, y=rating_mean)) + 
  geom_bar(stat = "identity", aes(fill=group)) +
  scale_fill_manual(values = c(primary[1], primary[4], primary[2], primary[3])) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10), 
                   limits = intlfinaidstatus) +
  geom_text(stat='identity', aes(label=round(rating_mean, digits=2)), vjust=-2.3) +
  geom_errorbar(aes(ymin=rating_mean - 1.96 * rating_se, ymax=rating_mean + 1.96 * rating_se),
                width=.2,                    # Width of the error bars
                position=position_dodge(0.9)) +
  ylim(c(0, 10)) + 
  xlab("Group") + 
  ylab("Mean rating of Fall 2020 Plan") + 
  labs(title="Rating of Plan by Intl, Fin Aid Status") +
  theme_hodp() +
  theme(legend.position = "none")
grid::grid.raster(logo, x = 0.01, y = 0.01, just = c('left', 'bottom'), width = unit(1.5, 'cm'))

# breaking down enrollment by financial aid
table(df$enroll[df$financial_aid == "Yes"]) / nrow(df[df$financial_aid == "Yes", ])
table(df$enroll[df$financial_aid == "No"]) / nrow(df[df$financial_aid == "No", ])

# breaking down enrollment by international students
table(df$enroll[df$international == "Yes"]) / nrow(df[df$international == "Yes", ])
table(df$enroll[df$international == "No"]) / nrow(df[df$international == "No", ])


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

# finaid opinions among those on finaid - almost exactly the same as among overall population, don't need it
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
  labs(title="Financial Aid Policy Opinions (among studdents on aid)") +
  theme_hodp() +
  theme(legend.position = "none")
grid::grid.raster(logo, x = 0.01, y = 0.01, just = c('left', 'bottom'), width = unit(1.5, 'cm'))

# most frequent words and word cloud of free text
dfCorpus <- Corpus(VectorSource(df$Q22)) 
# inspect(dfCorpus)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
dfCorpus <- tm_map(dfCorpus, toSpace, "/")
dfCorpus <- tm_map(dfCorpus, toSpace, "@")
dfCorpus <- tm_map(dfCorpus, toSpace, "\\|")

# Convert the text to lower case
dfCorpus <- tm_map(dfCorpus, content_transformer(tolower))
# Remove numbers
dfCorpus <- tm_map(dfCorpus, removeNumbers)
# Remove punctuations
dfCorpus <- tm_map(dfCorpus, removePunctuation)
# Remove your own stop word
# specify your stopwords as a character vector
dfCorpus <- tm_map(dfCorpus, removeWords, c("harvard", "student", "students", "'s", "'ve", "also", "'m", "'re", "many", "though", "one", "can", "seem", "will","think", "will", "plan", "like", "make", "person"))
# Remove english common stopwords
dfCorpus <- tm_map(dfCorpus, removeWords, stopwords("english"))
# Eliminate extra white spaces
dfCorpus <- tm_map(dfCorpus, stripWhitespace)
# Text stemming
# dfCorpus <- tm_map(dfCorpus, stemDocument)
dtm <- TermDocumentMatrix(dfCorpus)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 30)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 10,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


# sentiment analysis

  
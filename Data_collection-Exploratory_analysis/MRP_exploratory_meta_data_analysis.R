
library(ggplot2)
library(lubridate)
library(tidyr)
library(httr)
library(rjson)
library(tm)
library(gridExtra)
library(NLP)
library(SnowballC)
library(dplyr)
library(scales)
library(stringr)
library(wordcloud)
library(ggthemes)

###############################################################################################################
#                                          Meta Data Analysis
###############################################################################################################

######################################## Canadian Tire #########################################################
#Set the working directory
can_tire_tweets <- read.csv("can_tire_dd2.csv",header= TRUE, sep=",", na.strings=c(""," ","NA"))
can_tire_tweets$Unnamed..0 <- NULL
can_tire_tweets$isOfficialAccount <- can_tire_tweets$screenName == "CanadianTire"
can_tire_tweets$isRetweet<-as.logical(can_tire_tweets$isRetweet)

#Combine all reply to a single column
can_tire_tweets$isReply <- ifelse((!is.na(can_tire_tweets$replyToSID) |
                                     !is.na(can_tire_tweets$replyToSN) |
                                     !is.na(can_tire_tweets$replyToUID)), TRUE, FALSE)

#Find out retweet, reply and official tweet rate to understand social media activity
can_tire_retweet_rate <- sum(as.numeric(can_tire_tweets$isRetweet)) / nrow(can_tire_tweets)
can_tire_reply_rate <- sum(as.numeric(can_tire_tweets$isReply)) / nrow(can_tire_tweets)
can_tire_own_tweet_rate <- sum(as.numeric(can_tire_tweets$isOfficialAccount)) / nrow(can_tire_tweets)


#Create dataframe
can_tire_summary <- data.frame(c("Retweets", "Replies","Official Account"), 
                               c(can_tire_retweet_rate, can_tire_reply_rate, can_tire_own_tweet_rate))
#Assign column name and create a new column contains company name. 
colnames(can_tire_summary) <- c("Criteria", "Value")
can_tire_summary$Retail_Name = "Canadian Tire"
can_tire_summary

str(can_tire_tweets)

summary(walmart_ca_tweets)

str(walmart_ca_tweets)

########################################### Walmart Metadata ###################################################

walmart_ca_tweets <- read.csv("walmart_dd2.csv",header= TRUE, sep=",", na.strings=c(""," ","NA"))
walmart_ca_tweets$Unnamed..0 <- NULL
walmart_ca_tweets$isOfficialAccount <- walmart_ca_tweets$screenName == "WalmartCanada"
walmart_ca_tweets$isRetweet <- as.logical(walmart_ca_tweets$isRetweet)
walmart_ca_tweets$isReply <- ifelse((!is.na(walmart_ca_tweets$replyToSID)| !is.na(walmart_ca_tweets$replyToSN) | !is.na(walmart_ca_tweets$replyToUID)), TRUE, FALSE)
sum(as.numeric(can_tire_tweets$isRetweet)) / nrow(can_tire_tweets)
nrow(can_tire_tweets)
sum(as.numeric(can_tire_tweets$isRetweet))

walmart_ca_retweet_rate <- sum(as.numeric(walmart_ca_tweets$isRetweet)) / nrow(walmart_ca_tweets)
walmart_ca_reply_rate <- sum(as.numeric(walmart_ca_tweets$isReply)) / nrow(walmart_ca_tweets)
walmart_ca_own_tweet_rate <- sum(as.numeric(walmart_ca_tweets$isOfficialAccount)) / nrow(walmart_ca_tweets)



walmart_ca_summary <- data.frame(c("Retweets", "Replies","Official Account"), 
                                 c(walmart_ca_retweet_rate, walmart_ca_reply_rate, walmart_ca_own_tweet_rate))
colnames(walmart_ca_summary) <- c("Criteria", "Value")
walmart_ca_summary$Retail_Name = "Walmart"
walmart_ca_summary

############################################## Costco ##########################################################
costco_ca_tweets <- read.csv("costco_dd2.csv",header= TRUE, sep=",", na.strings=c(""," ","NA"))
costco_ca_tweets$Unnamed..0 <- NULL
costco_ca_tweets$isOfficialAccount <- costco_ca_tweets$screenName == "costcocanada"
costco_ca_tweets$isRetweet <-as.logical(costco_ca_tweets$isRetweet)
costco_ca_tweets$isReply <- ifelse((!is.na(costco_ca_tweets$replyToSID) |
                                      !is.na(costco_ca_tweets$replyToSN) |
                                      !is.na(costco_ca_tweets$replyToUID)), TRUE, FALSE)
costco_ca_retweet_rate <- sum(as.numeric(costco_ca_tweets$isRetweet)) / nrow(costco_ca_tweets)
costco_ca_reply_rate <- sum(as.numeric(costco_ca_tweets$isReply)) / nrow(costco_ca_tweets)
costco_ca_own_tweet_rate <- sum(as.numeric(costco_ca_tweets$isOfficialAccount)) / nrow(costco_ca_tweets)



costco_ca_summary <- data.frame(c("Retweets", "Replies","Official Account"), 
                                c(costco_ca_retweet_rate, costco_ca_reply_rate, costco_ca_own_tweet_rate))
colnames(costco_ca_summary) <- c("Criteria", "Value")
costco_ca_summary$Retail_Name = "Costco"
costco_ca_summary


############################################## Sobeys ##########################################################

sobeys_tweets <- read.csv("sob_dd2.csv",header= TRUE, sep=",", na.strings=c(""," ","NA"))
sobeys_tweets$Unnamed..0 <- NULL
sobeys_tweets$isOfficialAccount <- sobeys_tweets$screenName == "sobeys"
sobeys_tweets$isRetweet<-as.logical(sobeys_tweets$isRetweet)

sobeys_tweets$isReply <- ifelse((!is.na(sobeys_tweets$replyToSID) |
                                   !is.na(sobeys_tweets$replyToSN) |
                                   !is.na(sobeys_tweets$replyToUID)), TRUE, FALSE)
sobeys_retweet_rate <- sum(as.numeric(sobeys_tweets$isRetweet)) / nrow(sobeys_tweets)
sobeys_reply_rate <- sum(as.numeric(sobeys_tweets$isReply)) / nrow(sobeys_tweets)
sobeys_own_tweet_rate <- sum(as.numeric(sobeys_tweets$isOfficialAccount)) / nrow(sobeys_tweets)



sobeys_summary <- data.frame(c("Retweets", "Replies","Official Account"), 
                             c(sobeys_retweet_rate, sobeys_reply_rate, sobeys_own_tweet_rate))
colnames(sobeys_summary) <- c("Criteria", "Value")
sobeys_summary$Retail_Name = "Sobeys"
sobeys_summary


########################################### Loblaws ############################################################

loblaws_tweets <- read.csv("lob_dd2.csv",header= TRUE, sep=",", na.strings=c(""," ","NA"))
loblaws_tweets$Unnamed..0 <- NULL
loblaws_tweets$isOfficialAccount <- loblaws_tweets$screenName == "LoblawsON"
loblaws_tweets$isRetweet <-as.logical(loblaws_tweets$isRetweet)

loblaws_tweets$isReply <- ifelse((!is.na(loblaws_tweets$replyToSID) |
                                    !is.na(loblaws_tweets$replyToSN) |
                                    !is.na(loblaws_tweets$replyToUID)), TRUE, FALSE)
loblaws_retweet_rate <- sum(as.numeric(loblaws_tweets$isRetweet)) / nrow(loblaws_tweets)
loblaws_reply_rate <- sum(as.numeric(loblaws_tweets$isReply)) / nrow(loblaws_tweets)
loblaws_own_tweet_rate <- sum(as.numeric(loblaws_tweets$isOfficialAccount)) / nrow(loblaws_tweets)



loblaws_summary <- data.frame(c("Retweets", "Replies","Official Account"), 
                              c(loblaws_retweet_rate, loblaws_reply_rate, loblaws_own_tweet_rate))
colnames(loblaws_summary) <- c("Criteria", "Value")
loblaws_summary$Retail_Name = "Loblaws"
loblaws_summary


############################################## ALL Retails ######################################################

all_retail_tweets <- rbind(walmart_ca_tweets,
                           can_tire_tweets,
                           costco_ca_tweets,
                           sobeys_tweets,
                           loblaws_tweets)

#Found difference in isRetweet column, changed all to common lable.
#levels(can_tire_tweets$isRetweet) <- c('True', 'False')





all_retail_retweet_rate <- sum(as.numeric(all_retail_tweets$isRetweet)) / nrow(all_retail_tweets)
all_retail_reply_rate <- sum(as.numeric(all_retail_tweets$isReply)) / nrow(all_retail_tweets)
all_retail_offacct_rate <- sum(as.numeric(all_retail_tweets$isOfficialAccount)) / nrow(all_retail_tweets)

all_retail_summary <- data.frame(c("Retweets", "Replies", "Official Account"), 
                                 c(all_retail_retweet_rate, all_retail_reply_rate, all_retail_offacct_rate))



colnames(all_retail_summary) <- c("Criteria", "Value")
all_retail_summary$Retail_Name = "All Retail"

retail_data_summary <- rbind(loblaws_summary,
                             can_tire_summary, 
                             costco_ca_summary, 
                             walmart_ca_summary,
                             sobeys_summary,
                             all_retail_summary)

all_retail_summary
retail_data_summary
windowsFonts(sans=windowsFont("TT Arial"))

ggplot(data=retail_data_summary, aes(y=Value, x=Retail_Name, label=Value)) + 
  geom_bar(aes(fill=Retail_Name),data=retail_data_summary, stat="identity")  +
  coord_flip() + 
  facet_grid(Criteria ~ .) +
  scale_y_continuous(labels=percent, limits=c(0,1)) + 
  labs(title="Metadata Analysis") + 
  geom_text(aes( y = Value, label = paste0(round(Value * 100,1),"%")), size=4, hjust=-0.1) + 
  theme_economist(base_family="sans") + 
  scale_colour_economist() + 
  scale_fill_economist() + 
  theme(plot.title=element_text(family="sans"),
        text=element_text(family="sans"), legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank()) 

#To choose Other Fonts
windowsFonts()


#################################################################################################################
                                      #   Exploratory Data Analysis   #
#################################################################################################################


################################################Canadian Tire ###################################################
 
#Import tweet data 
can_tire_tweets1 <- read.csv("can_tire_dd2.csv",header= TRUE, sep=",", na.strings=c(""," ","NA")) 
can_tire_tweets1$Unnamed..0 <- NULL

summary(is.na(can_tire_tweets1$created))
#as.Date(can_tire_met$created, format = "%m/%d/%y %h%m")
can_tire_tweets1$created <- parse_date_time(can_tire_tweets1$created, "%m%d%Y %H%M") 
#can_tire_tweets$created <- ymd_hms(can_tire_tweets$created, tz="America/Toronto") #<-
#can_tire_tweets1$created <- with_tz(can_tire_tweets1$created, "America/Toronto")

#dmy(date_english)
#parse_date_time("04/18/1950 0130", "%m%d%Y %H%M")
#
#tweets by time of the day
can_tire_tweets1$timeonly <- strftime(can_tire_tweets1$created, format="%H:%M:%S")
can_tire_tweets1$timeonly <- as.POSIXct(can_tire_tweets1$timeonly, format="%H:%M:%S")
#Check NA's
can_tire_tweets1[(minute(can_tire_tweets1$created) == 0 & second(can_tire_tweets1$created) == 0),5] <- NA
mean(is.na(can_tire_tweets1$timeonly))

#isReply
can_tire_tweets1$isReply <- ifelse((!is.na(can_tire_tweets1$replyToSID) |
                                   !is.na(can_tire_tweets1$replyToSN) |
                                   !is.na(can_tire_tweets1$replyToUID)), TRUE, FALSE)

Can_tire_topten_user <- can_tire_tweets1 %>% count(screenName) %>% top_n(n=10)
qplot(created, data=can_tire_tweets1, fill=isRetweet, facets = isReply~.)

ggplot(data = can_tire_tweets1, aes(x = created)) +
  geom_histogram(aes(fill = ..count..)) +
  theme(legend.position = "none") +
  labs(title="Daywise Tweet Count - Canadian Tire", x = "Time", y="Tweets count") + 
  scale_fill_gradient(low = "midnightblue", high = "aquamarine4") +
  geom_density()

# Tweets count on weekdays
ggplot(data = can_tire_tweets1, aes(x = wday(created, label = TRUE))) +
  stat_count(width = .5, aes(fill = ..count..)) +
  theme(legend.position = "none") +
  labs(title = "Canadian Tire", x ="Day of the Week", y="Number of tweets") +
  scale_fill_gradient(low = "midnightblue", high = "aquamarine4") 

# Tweets count in three hour intrvl
class(can_tire_tweets1$timeonly) <- "POSIXct"
class(can_tire_tweets1$created)

ggplot(data = can_tire_tweets1, aes(x = timeonly)) +
  geom_histogram(aes(fill = ..count..)) +
  theme(legend.position = "none") +
  scale_x_datetime(breaks = date_breaks("3 hour")
                   ,labels = date_format("%H:00")) +
  labs(title = "Canadian Tire", x = "Time", y="Number of Tweets") +
  scale_fill_gradient(low = "midnightblue", high = "aquamarine4")

# AM Tweets
latenight_can_tire_met <- can_tire_tweets1[(hour(can_tire_tweets1$created) < 6),]
ggplot(data = latenight_can_tire_met, aes(x = created)) +
  geom_histogram(aes(fill = ..count..)) +
  theme(legend.position = "none") +
  labs(title = "Late Night Tweets - Canadian Tire", x="Time", y = "Number of tweets") +
  scale_fill_gradient(low = "midnightblue", high = "aquamarine4")

# Retweets
ggplot(can_tire_tweets1, aes(x= factor(isRetweet))) +
  geom_bar(fill = "midnightblue") + 
  theme(legend.position="none", axis.title.x = element_blank()) +
  ylab("Tweets Count") + 
  ggtitle("Retweeted vs Not Retweeted(Count)") +
  labs(title = "Canadian Tire") + 
  scale_x_discrete(labels=c("Not retweeted", "Retweeted tweets"))

# Retweets in percentage
ggplot(can_tire_tweets1, aes(x = can_tire_tweets1$isRetweet)) +
  geom_bar(aes(y=(..count..) / sum(..count..)), fill = "midnightblue") + 
  theme(legend.position="none", axis.title.x = element_blank()) +
  scale_y_continuous(labels=percent) +
  labs(title = "Retweeted vs Not Retweeted(Percentage) - Canadian Tire", y="Percentage of tweets") + 
  scale_x_discrete(labels=c("Other", "Retweets"))

# Replies by count
ggplot(can_tire_tweets1, aes(can_tire_tweets1$isReply)) +
  geom_bar(fill = "midnightblue") + 
  theme(legend.position="none", axis.title.x = element_blank()) +
  ylab("Tweets Count") + 
  ggtitle("Replied vs Not Replied(Count) - Canadian Tire") +
  scale_x_discrete(labels=c("Not in Reply", "Replied Tweets"))

# Replies by percentage
ggplot(can_tire_tweets1, aes(x = can_tire_tweets1$isReply)) +
  geom_bar(aes(y=(..count..) / sum(..count..)), fill = "midnightblue") + 
  theme(legend.position="none", axis.title.x = element_blank()) +
  scale_y_continuous(labels=percent) +
  labs(title = "Replied vs Not Replied(Percentage) - Canadian Tire", y="Percentage of tweets") + 
  scale_x_discrete(labels=c("Not in Reply", "Replied Tweets"))

# Top 10 users
ggplot(data= Can_tire_topten_user, aes(reorder(screenName, n),n)) +
  geom_bar(stat="identity", fill="midnightblue") +
  coord_flip() +
  labs(x = "User Name", y = "Tweet Count", title = "Top Ten Users - Canadian Tire")

Can_tire_topten_user

################################################ Walmart ###################################################

#Import tweet data 
walmart_tweets1 <- read.csv("walmart_dd2.csv",header= TRUE, sep=",", na.strings=c(""," ","NA")) 
walmart_tweets1$Unnamed..0 <- NULL

summary(is.na(walmart_tweets1$created))
#as.Date(can_tire_met$created, format = "%m/%d/%y %h%m")
#walmart_tweets1$created <- parse_date_time(walmart_tweets1$created, "%m%d%Y %H%M") 
walmart_tweets1$created <- ymd_hms(walmart_tweets1$created, tz="America/Toronto") #<-
#walmart_tweets1$created <- with_tz(walmart_tweets1$created, "America/Toronto")

#dmy(date_english)
#parse_date_time("04/18/1950 0130", "%m%d%Y %H%M")
#
#tweets by time of the day
walmart_tweets1$timeonly <- strftime(walmart_tweets1$created, format="%H:%M:%S")
walmart_tweets1$timeonly <- as.POSIXct(walmart_tweets1$timeonly, format="%H:%M:%S")
#Check NA's
walmart_tweets1[(minute(walmart_tweets1$created) == 0 & second(walmart_tweets1$created) == 0),5] <- NA
mean(is.na(walmart_tweets1$timeonly))

#isReply
walmart_tweets1$isReply <- ifelse((!is.na(walmart_tweets1$replyToSID) |
                                      !is.na(walmart_tweets1$replyToSN) |
                                      !is.na(walmart_tweets1$replyToUID)), TRUE, FALSE)

walmart_topten_user <- walmart_tweets1 %>% count(screenName) %>% top_n(n=10)
qplot(created, data=walmart_tweets1, fill=isRetweet, facets = isReply~.)

ggplot(data = walmart_tweets1, aes(x = created)) +
  geom_histogram(aes(fill = ..count..)) +
  theme(legend.position = "none") +
  labs(title="Daywise Tweet Count - Walmart", x = "Time", y="Tweets count") + 
  scale_fill_gradient(low = "midnightblue", high = "aquamarine4") +
  geom_density()

# Tweets count on weekdays
ggplot(data = walmart_tweets1, aes(x = wday(created, label = TRUE))) +
  stat_count(width = .5, aes(fill = ..count..)) +
  theme(legend.position = "none") +
  labs(title = "Walmart", x ="Day of the Week", y="Number of tweets") +
  scale_fill_gradient(low = "midnightblue", high = "aquamarine4") 

# Tweets count in three hour intrvl
class(walmart_tweets1$timeonly) <- "POSIXct"
class(walmart_tweets1$created)

ggplot(data = walmart_tweets1, aes(x = timeonly)) +
  geom_histogram(aes(fill = ..count..)) +
  theme(legend.position = "none") +
  scale_x_datetime(breaks = date_breaks("3 hour")
                   ,labels = date_format("%H:00")) +
  labs(title = "Walmart", x = "Time", y="Number of Tweets") +
  scale_fill_gradient(low = "midnightblue", high = "aquamarine4")

# AM Tweets
latenight_can_tire_met <- walmart_tweets1[(hour(walmart_tweets1$created) < 6),]
ggplot(data = latenight_can_tire_met, aes(x = created)) +
  geom_histogram(aes(fill = ..count..)) +
  theme(legend.position = "none") +
  labs(title = "Late Night Tweets - Walmart", x="Time", y = "Number of tweets") +
  scale_fill_gradient(low = "midnightblue", high = "aquamarine4")

# Retweets
ggplot(walmart_tweets1, aes(x= factor(isRetweet))) +
  geom_bar(fill = "midnightblue") + 
  theme(legend.position="none", axis.title.x = element_blank()) +
  ylab("Tweets Count") + 
  ggtitle("Retweeted vs Not Retweeted(Count)") +
  labs(title = "Walmart") + 
  scale_x_discrete(labels=c("Not retweeted", "Retweeted tweets"))

# Retweets in percentage
ggplot(walmart_tweets1, aes(x = walmart_tweets1$isRetweet)) +
  geom_bar(aes(y=(..count..) / sum(..count..)), fill = "midnightblue") + 
  theme(legend.position="none", axis.title.x = element_blank()) +
  scale_y_continuous(labels=percent) +
  labs(title = "Retweeted vs Not Retweeted(Percentage) - Walmart", y="Percentage of tweets") + 
  scale_x_discrete(labels=c("Other", "Retweets"))

# Replies by count
ggplot(walmart_tweets1, aes(walmart_tweets1$isReply)) +
  geom_bar(fill = "midnightblue") + 
  theme(legend.position="none", axis.title.x = element_blank()) +
  ylab("Tweets Count") + 
  ggtitle("Replied vs Not Replied(Count) - Walmart") +
  scale_x_discrete(labels=c("Not in Reply", "Replied Tweets"))

# Replies by percentage
ggplot(walmart_tweets1, aes(x = walmart_tweets1$isReply)) +
  geom_bar(aes(y=(..count..) / sum(..count..)), fill = "midnightblue") + 
  theme(legend.position="none", axis.title.x = element_blank()) +
  scale_y_continuous(labels=percent) +
  labs(title = "Replied vs Not Replied(Percentage) - Walmart", y="Percentage of tweets") + 
  scale_x_discrete(labels=c("Not in Reply", "Replied Tweets"))

# Top 10 users
ggplot(data= walmart_topten_user, aes(reorder(screenName, n),n)) +
  geom_bar(stat="identity", fill="midnightblue") +
  coord_flip() +
  labs(x = "User Name", y = "Tweet Count", title = "Top Ten Users - Walmart")

walmart_topten_user



################################################ Loblaws ###################################################

#Import tweet data 
loblaws_tweets1 <- read.csv("lob_dd2.csv",header= TRUE, sep=",", na.strings=c(""," ","NA")) 
loblaws_tweets1$Unnamed..0 <- NULL

summary(is.na(loblaws_tweets1$created))
#as.Date(can_tire_met$created, format = "%m/%d/%y %h%m")
#loblaws_tweets1$created <- parse_date_time(loblaws_tweets1$created, "%m%d%Y %H%M") 
loblaws_tweets1$created <- ymd_hms(loblaws_tweets1$created, tz="America/Toronto") #<-
#loblaws_tweets1$created <- with_tz(loblaws_tweets1$created, "America/Toronto")

#dmy(date_english)
#parse_date_time("04/18/1950 0130", "%m%d%Y %H%M")
#
#tweets by time of the day
loblaws_tweets1$timeonly <- strftime(loblaws_tweets1$created, format="%H:%M:%S")
loblaws_tweets1$timeonly <- as.POSIXct(loblaws_tweets1$timeonly, format="%H:%M:%S")
#Check NA's
loblaws_tweets1[(minute(loblaws_tweets1$created) == 0 & second(loblaws_tweets1$created) == 0),5] <- NA
mean(is.na(loblaws_tweets1$timeonly))

#isReply
loblaws_tweets1$isReply <- ifelse((!is.na(loblaws_tweets1$replyToSID) |
                                     !is.na(loblaws_tweets1$replyToSN) |
                                     !is.na(loblaws_tweets1$replyToUID)), TRUE, FALSE)

loblaws_topten_user <- loblaws_tweets1 %>% count(screenName) %>% top_n(n=10)
qplot(created, data=loblaws_tweets1, fill=isRetweet, facets = isReply~.)

ggplot(data = loblaws_tweets1, aes(x = created)) +
  geom_histogram(aes(fill = ..count..)) +
  theme(legend.position = "none") +
  labs(title="Daywise Tweet Count - Loblaws", x = "Time", y="Tweets count") + 
  scale_fill_gradient(low = "midnightblue", high = "aquamarine4") +
  geom_density()

# Tweets count on weekdays
ggplot(data = loblaws_tweets1, aes(x = wday(created, label = TRUE))) +
  stat_count(width = .5, aes(fill = ..count..)) +
  theme(legend.position = "none") +
  labs(title = "Loblaws", x ="Day of the Week", y="Number of tweets") +
  scale_fill_gradient(low = "midnightblue", high = "aquamarine4") 

# Tweets count in three hour intrvl
class(loblaws_tweets1$timeonly) <- "POSIXct"
class(loblaws_tweets1$created)

ggplot(data = loblaws_tweets1, aes(x = timeonly)) +
  geom_histogram(aes(fill = ..count..)) +
  theme(legend.position = "none") +
  scale_x_datetime(breaks = date_breaks("3 hour")
                   ,labels = date_format("%H:00")) +
  labs(title = "Loblaws", x = "Time", y="Number of Tweets") +
  scale_fill_gradient(low = "midnightblue", high = "aquamarine4")

# AM Tweets
latenight_can_tire_met <- loblaws_tweets1[(hour(loblaws_tweets1$created) < 6),]
ggplot(data = latenight_can_tire_met, aes(x = created)) +
  geom_histogram(aes(fill = ..count..)) +
  theme(legend.position = "none") +
  labs(title = "Late Night Tweets - Loblaws", x="Time", y = "Number of tweets") +
  scale_fill_gradient(low = "midnightblue", high = "aquamarine4")

# Retweets
ggplot(loblaws_tweets1, aes(x= factor(isRetweet))) +
  geom_bar(fill = "midnightblue") + 
  theme(legend.position="none", axis.title.x = element_blank()) +
  ylab("Tweets Count") + 
  ggtitle("Retweeted vs Not Retweeted(Count)") +
  labs(title = "Loblaws") + 
  scale_x_discrete(labels=c("Not retweeted", "Retweeted tweets"))

# Retweets in percentage
ggplot(loblaws_tweets1, aes(x = loblaws_tweets1$isRetweet)) +
  geom_bar(aes(y=(..count..) / sum(..count..)), fill = "midnightblue") + 
  theme(legend.position="none", axis.title.x = element_blank()) +
  scale_y_continuous(labels=percent) +
  labs(title = "Retweeted vs Not Retweeted(Percentage) - Loblaws", y="Percentage of tweets") + 
  scale_x_discrete(labels=c("Other", "Retweets"))

# Replies by count
ggplot(loblaws_tweets1, aes(loblaws_tweets1$isReply)) +
  geom_bar(fill = "midnightblue") + 
  theme(legend.position="none", axis.title.x = element_blank()) +
  ylab("Tweets Count") + 
  ggtitle("Replied vs Not Replied(Count) - Loblaws") +
  scale_x_discrete(labels=c("Not in Reply", "Replied Tweets"))

# Replies by percentage
ggplot(loblaws_tweets1, aes(x = loblaws_tweets1$isReply)) +
  geom_bar(aes(y=(..count..) / sum(..count..)), fill = "midnightblue") + 
  theme(legend.position="none", axis.title.x = element_blank()) +
  scale_y_continuous(labels=percent) +
  labs(title = "Replied vs Not Replied(Percentage) - Loblaws", y="Percentage of tweets") + 
  scale_x_discrete(labels=c("Not in Reply", "Replied Tweets"))

# Top 10 users
ggplot(data= loblaws_topten_user, aes(reorder(screenName, n),n)) +
  geom_bar(stat="identity", fill="midnightblue") +
  coord_flip() +
  labs(x = "User Name", y = "Tweet Count", title = "Top Ten Users - Loblaws")

loblaws_topten_user



###########################################################################################################
################################################ Costco ###################################################

#Import tweet data 
costco_tweets1 <- read.csv("costco_dd2.csv",header= TRUE, sep=",", na.strings=c(""," ","NA")) 
costco_tweets1$Unnamed..0 <- NULL

summary(is.na(costco_tweets1$created))
#as.Date(can_tire_met$created, format = "%m/%d/%y %h%m")
#costco_tweets1$created <- parse_date_time(costco_tweets1$created, "%m%d%Y %H%M") 
costco_tweets1$created <- ymd_hms(costco_tweets1$created, tz="America/Toronto") #<-
#costco_tweets1$created <- with_tz(costco_tweets1$created, "America/Toronto")

#dmy(date_english)
#parse_date_time("04/18/1950 0130", "%m%d%Y %H%M")
#
#tweets by time of the day
costco_tweets1$timeonly <- strftime(costco_tweets1$created, format="%H:%M:%S")
costco_tweets1$timeonly <- as.POSIXct(costco_tweets1$timeonly, format="%H:%M:%S")
#Check NA's
costco_tweets1[(minute(costco_tweets1$created) == 0 & second(costco_tweets1$created) == 0),5] <- NA
mean(is.na(costco_tweets1$timeonly))

#isReply
costco_tweets1$isReply <- ifelse((!is.na(costco_tweets1$replyToSID) |
                                     !is.na(costco_tweets1$replyToSN) |
                                     !is.na(costco_tweets1$replyToUID)), TRUE, FALSE)

costco_topten_user <- costco_tweets1 %>% count(screenName) %>% top_n(n=10)
qplot(created, data=costco_tweets1, fill=isRetweet, facets = isReply~.)

ggplot(data = costco_tweets1, aes(x = created)) +
  geom_histogram(aes(fill = ..count..)) +
  theme(legend.position = "none") +
  labs(title="Daywise Tweet Count - Costco", x = "Time", y="Tweets count") + 
  scale_fill_gradient(low = "midnightblue", high = "aquamarine4") +
  geom_density()

# Tweets count on weekdays
ggplot(data = costco_tweets1, aes(x = wday(created, label = TRUE))) +
  stat_count(width = .5, aes(fill = ..count..)) +
  theme(legend.position = "none") +
  labs(title = "Costco", x ="Day of the Week", y="Number of tweets") +
  scale_fill_gradient(low = "midnightblue", high = "aquamarine4") 

# Tweets count in three hour intrvl
class(costco_tweets1$timeonly) <- "POSIXct"
class(costco_tweets1$created)

ggplot(data = costco_tweets1, aes(x = timeonly)) +
  geom_histogram(aes(fill = ..count..)) +
  theme(legend.position = "none") +
  scale_x_datetime(breaks = date_breaks("3 hour")
                   ,labels = date_format("%H:00")) +
  labs(title = "Costco", x = "Time", y="Number of Tweets") +
  scale_fill_gradient(low = "midnightblue", high = "aquamarine4")

# AM Tweets
latenight_can_tire_met <- costco_tweets1[(hour(costco_tweets1$created) < 6),]
ggplot(data = latenight_can_tire_met, aes(x = created)) +
  geom_histogram(aes(fill = ..count..)) +
  theme(legend.position = "none") +
  labs(title = "Late Night Tweets - Costco", x="Time", y = "Number of tweets") +
  scale_fill_gradient(low = "midnightblue", high = "aquamarine4")

# Retweets
ggplot(costco_tweets1, aes(x= factor(isRetweet))) +
  geom_bar(fill = "midnightblue") + 
  theme(legend.position="none", axis.title.x = element_blank()) +
  ylab("Tweets Count") + 
  ggtitle("Retweeted vs Not Retweeted(Count)") +
  labs(title = "Costco") + 
  scale_x_discrete(labels=c("Not retweeted", "Retweeted tweets"))

# Retweets in percentage
ggplot(costco_tweets1, aes(x = costco_tweets1$isRetweet)) +
  geom_bar(aes(y=(..count..) / sum(..count..)), fill = "midnightblue") + 
  theme(legend.position="none", axis.title.x = element_blank()) +
  scale_y_continuous(labels=percent) +
  labs(title = "Retweeted vs Not Retweeted(Percentage) - Costco", y="Percentage of tweets") + 
  scale_x_discrete(labels=c("Other", "Retweets"))

# Replies by count
ggplot(costco_tweets1, aes(costco_tweets1$isReply)) +
  geom_bar(fill = "midnightblue") + 
  theme(legend.position="none", axis.title.x = element_blank()) +
  ylab("Tweets Count") + 
  ggtitle("Replied vs Not Replied(Count) - Costco") +
  scale_x_discrete(labels=c("Not in Reply", "Replied Tweets"))

# Replies by percentage
ggplot(costco_tweets1, aes(x = costco_tweets1$isReply)) +
  geom_bar(aes(y=(..count..) / sum(..count..)), fill = "midnightblue") + 
  theme(legend.position="none", axis.title.x = element_blank()) +
  scale_y_continuous(labels=percent) +
  labs(title = "Replied vs Not Replied(Percentage) - Costco", y="Percentage of tweets") + 
  scale_x_discrete(labels=c("Not in Reply", "Replied Tweets"))

# Top 10 users
ggplot(data= costco_topten_user, aes(reorder(screenName, n),n)) +
  geom_bar(stat="identity", fill="midnightblue") +
  coord_flip() +
  labs(x = "User Name", y = "Tweet Count", title = "Top Ten Users - Costco")

costco_topten_user


###########################################################################################################
################################################ Sobeys ###################################################

#Import tweet data 
sobeys_tweets1 <- read.csv("sob_dd2.csv",header= TRUE, sep=",", na.strings=c(""," ","NA")) 
sobeys_tweets1$Unnamed..0 <- NULL

summary(is.na(sobeys_tweets1$created))
#as.Date(can_tire_met$created, format = "%m/%d/%y %h%m")
#sobeys_tweets1$created <- parse_date_time(sobeys_tweets1$created, "%m%d%Y %H%M") 
sobeys_tweets1$created <- ymd_hms(sobeys_tweets1$created, tz="America/Toronto") #<-
#sobeys_tweets1$created <- with_tz(sobeys_tweets1$created, "America/Toronto")

#dmy(date_english)
#parse_date_time("04/18/1950 0130", "%m%d%Y %H%M")
#
#tweets by time of the day
sobeys_tweets1$timeonly <- strftime(sobeys_tweets1$created, format="%H:%M:%S")
sobeys_tweets1$timeonly <- as.POSIXct(sobeys_tweets1$timeonly, format="%H:%M:%S")
#Check NA's
sobeys_tweets1[(minute(sobeys_tweets1$created) == 0 & second(sobeys_tweets1$created) == 0),5] <- NA
mean(is.na(sobeys_tweets1$timeonly))

#isReply
sobeys_tweets1$isReply <- ifelse((!is.na(sobeys_tweets1$replyToSID) |
                                    !is.na(sobeys_tweets1$replyToSN) |
                                    !is.na(sobeys_tweets1$replyToUID)), TRUE, FALSE)

sobeys_topten_user <- sobeys_tweets1 %>% count(screenName) %>% top_n(n=10)
qplot(created, data=sobeys_tweets1, fill=isRetweet, facets = isReply~.)

ggplot(data = sobeys_tweets1, aes(x = created)) +
  geom_histogram(aes(fill = ..count..)) +
  theme(legend.position = "none") +
  labs(title="Daywise Tweet Count - Sobeys", x = "Time", y="Tweets count") + 
  scale_fill_gradient(low = "midnightblue", high = "aquamarine4") +
  geom_density()

# Tweets count on weekdays
ggplot(data = sobeys_tweets1, aes(x = wday(created, label = TRUE))) +
  stat_count(width = .5, aes(fill = ..count..)) +
  theme(legend.position = "none") +
  labs(title = "Sobeys", x ="Day of the Week", y="Number of tweets") +
  scale_fill_gradient(low = "midnightblue", high = "aquamarine4") 

# Tweets count in three hour intrvl
class(sobeys_tweets1$timeonly) <- "POSIXct"
class(sobeys_tweets1$created)

ggplot(data = sobeys_tweets1, aes(x = timeonly)) +
  geom_histogram(aes(fill = ..count..)) +
  theme(legend.position = "none") +
  scale_x_datetime(breaks = date_breaks("3 hour")
                   ,labels = date_format("%H:00")) +
  labs(title = "Sobeys", x = "Time", y="Number of Tweets") +
  scale_fill_gradient(low = "midnightblue", high = "aquamarine4")

# AM Tweets
latenight_can_tire_met <- sobeys_tweets1[(hour(sobeys_tweets1$created) < 6),]
ggplot(data = latenight_can_tire_met, aes(x = created)) +
  geom_histogram(aes(fill = ..count..)) +
  theme(legend.position = "none") +
  labs(title = "Late Night Tweets - Sobeys", x="Time", y = "Number of tweets") +
  scale_fill_gradient(low = "midnightblue", high = "aquamarine4")

# Retweets
ggplot(sobeys_tweets1, aes(x= factor(isRetweet))) +
  geom_bar(fill = "midnightblue") + 
  theme(legend.position="none", axis.title.x = element_blank()) +
  ylab("Tweets Count") + 
  ggtitle("Retweeted vs Not Retweeted(Count)") +
  labs(title = "Sobeys") + 
  scale_x_discrete(labels=c("Not retweeted", "Retweeted tweets"))

# Retweets in percentage
ggplot(sobeys_tweets1, aes(x = sobeys_tweets1$isRetweet)) +
  geom_bar(aes(y=(..count..) / sum(..count..)), fill = "midnightblue") + 
  theme(legend.position="none", axis.title.x = element_blank()) +
  scale_y_continuous(labels=percent) +
  labs(title = "Retweeted vs Not Retweeted(Percentage) - Sobeys", y="Percentage of tweets") + 
  scale_x_discrete(labels=c("Other", "Retweets"))

# Replies by count
ggplot(sobeys_tweets1, aes(sobeys_tweets1$isReply)) +
  geom_bar(fill = "midnightblue") + 
  theme(legend.position="none", axis.title.x = element_blank()) +
  ylab("Tweets Count") + 
  ggtitle("Replied vs Not Replied(Count) - Sobeys") +
  scale_x_discrete(labels=c("Not in Reply", "Replied Tweets"))

# Replies by percentage
ggplot(sobeys_tweets1, aes(x = sobeys_tweets1$isReply)) +
  geom_bar(aes(y=(..count..) / sum(..count..)), fill = "midnightblue") + 
  theme(legend.position="none", axis.title.x = element_blank()) +
  scale_y_continuous(labels=percent) +
  labs(title = "Replied vs Not Replied(Percentage) - Sobeys", y="Percentage of tweets") + 
  scale_x_discrete(labels=c("Not in Reply", "Replied Tweets"))

# Top 10 users
ggplot(data= sobeys_topten_user, aes(reorder(screenName, n),n)) +
  geom_bar(stat="identity", fill="midnightblue") +
  coord_flip() +
  labs(x = "User Name", y = "Tweet Count", title = "Top Ten Users - Sobeys")






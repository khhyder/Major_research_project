### installing/loading libraries
library(twitteR)


#####################################
# Data Collection and preprocessing #
#####################################

# Oauth authentication with API key and token
consumer_key <- "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
consumer_secret <- "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
access_token <- "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
access_secret <- "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"

collection_date <- format(Sys.time(), "%d%b%Y")
# Twitter oauth
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

#Canadian Tire
can_tire <- twListToDF(userTimeline(user="@CanadianTire", n=3200, includeRts = TRUE, retryOnRateLimit=2000))
write.csv(file = paste("can_tire_usertimeline_", current_date, ".csv"), x= can_tire, quote = TRUE, col.names = TRUE)

can_tire2 <- twListToDF(searchTwitter("canadiantire", n=1000, retryOnRateLimit=2000))
write.csv(file = paste("can_tire_mentions_", current_date,".csv"), x= can_tire2, quote = TRUE, col.names = TRUE)
can_tire3 <- twListToDF(searchTwitter("#canadiantire", n=2000, retryOnRateLimit=2000))
write.csv(file = paste("can_tire_hashtag_", current_date,".csv"), x= can_tire3, quote = TRUE, col.names = TRUE)
can_tire4 <- twListToDF(searchTwitter("@CanadianTire", n=2000, retryOnRateLimit=2000))
write.csv(file = paste("can_tire_atmention_", current_date,".csv"), x= can_tire4, quote = TRUE, col.names = TRUE)
can_tire5 <- twListToDF(searchTwitter("to:CanadianTire", n=1000, retryOnRateLimit=2000))
write.csv(file = paste("can_tire_touser_", current_date,".csv"), x= can_tire5, quote = TRUE, col.names = TRUE)
can_tire6 <- twListToDF(searchTwitter("from:CanadianTire", n=1000, retryOnRateLimit=2000))
write.csv(file = paste("can_tire_fromuser_", current_date,".csv"), x= can_tire6, quote = TRUE, col.names = TRUE)



#Walmart Canada

walmart1 <- twListToDF(userTimeline(user="@WalmartCanada", n=3200, includeRts = TRUE, retryOnRateLimit=2000))
write.csv(file = paste("walmart_usertimeline_", current_date, ".csv"), x= walmart1, quote = TRUE, col.names = TRUE)
walmart2 <- twListToDF(searchTwitter("walmartcanada", n=1000, retryOnRateLimit=2000))
write.csv(file = paste("walmart_mentions_", current_date,".csv"), x= walmart2, quote = TRUE, col.names = TRUE)
walmart3 <- twListToDF(searchTwitter("#walmartcanada", n=2000, retryOnRateLimit=2000))
write.csv(file = paste("walmart_hashtag_", current_date,".csv"), x= walmart3, quote = TRUE, col.names = TRUE)
walmart4 <- twListToDF(searchTwitter("@WalmartCanada", n=2000, retryOnRateLimit=2000))
write.csv(file = paste("walmart_atmention_", current_date,".csv"), x= walmart4, quote = TRUE, col.names = TRUE)
walmart5 <- twListToDF(searchTwitter("to:WalmartCanada", n=1000, retryOnRateLimit=2000))
write.csv(file = paste("walmart_touser_", current_date,".csv"), x= walmart5, quote = TRUE, col.names = TRUE)
walmart6 <- twListToDF(searchTwitter("from:WalmartCanada", n=1000, retryOnRateLimit=2000))
write.csv(file = paste("walmart_fromuser_", current_date,".csv"), x= walmart6, quote = TRUE, col.names = TRUE)




#Loblaws Canada

loblaws1 <- twListToDF(userTimeline(user="@LoblawsON", n=3200, includeRts = TRUE, retryOnRateLimit=2000))
write.csv(file = paste("loblaw_usertimeline_", current_date, ".csv"), x= loblaws1, quote = TRUE, col.names = TRUE)
loblaw2 <- twListToDF(searchTwitter("loblaws", n=1000, retryOnRateLimit=2000))
write.csv(file = paste("loblaw_mentions_", current_date,".csv"), x= loblaw2, quote = TRUE, col.names = TRUE)
loblaw3 <- twListToDF(searchTwitter("#loblaws", n=2000, retryOnRateLimit=2000))
write.csv(file = paste("loblaw_hashtag_", current_date,".csv"), x= loblaw3, quote = TRUE, col.names = TRUE)
loblaw4 <- twListToDF(searchTwitter("@LoblawsON", n=2000, retryOnRateLimit=2000))
write.csv(file = paste("loblaw_atmention_", current_date,".csv"), x= loblaw4, quote = TRUE, col.names = TRUE)
loblaw5 <- twListToDF(searchTwitter("to:@LoblawsON", n=1000, retryOnRateLimit=2000))
write.csv(file = paste("loblaw_touser_", current_date,".csv"), x= loblaw5, quote = TRUE, col.names = TRUE)
loblaw6 <- twListToDF(searchTwitter("from:@LoblawsON", n=1000, retryOnRateLimit=2000))
write.csv(file = paste("loblaw_fromuser_", current_date,".csv"), x= loblaw6, quote = TRUE, col.names = TRUE)


#Sobeys Canada

sobeys1 <- twListToDF(userTimeline(user="@sobeys", n=3200, includeRts = TRUE, retryOnRateLimit=2000))
write.csv(file = paste("sobeys_usertimeline_", current_date, ".csv"), x= sobeys1, quote = TRUE, col.names = TRUE)
sobeys2 <- twListToDF(searchTwitter("sobeys", n=1000, retryOnRateLimit=2000))
write.csv(file = paste("sobeys_mentions_", current_date,".csv"), x= sobeys2, quote = TRUE, col.names = TRUE)
sobeys3 <- twListToDF(searchTwitter("#sobeys", n=2000, retryOnRateLimit=2000))
write.csv(file = paste("sobeys_hashtag_", current_date,".csv"), x= sobeys3, quote = TRUE, col.names = TRUE)
sobeys4 <- twListToDF(searchTwitter("@sobeys", n=2000, retryOnRateLimit=2000))
write.csv(file = paste("sobeys_atmention_", current_date,".csv"), x= sobeys4, quote = TRUE, col.names = TRUE)
sobeys5 <- twListToDF(searchTwitter("to:sobeys", n=1000, retryOnRateLimit=2000))
write.csv(file = paste("sobeys_touser_", current_date,".csv"), x= sobeys5, quote = TRUE, col.names = TRUE)
sobeys6 <- twListToDF(searchTwitter("from:@sobeys", n=1000, retryOnRateLimit=2000))
write.csv(file = paste("sobeys_fromuser_", current_date,".csv"), x= sobeys6, quote = TRUE, col.names = TRUE)


#Costco Canada

costco1 <- twListToDF(userTimeline(user="@costcocanada", n=3200, includeRts = TRUE, retryOnRateLimit=2000))
write.csv(file = paste("costco_usertimeline_", current_date, ".csv"), x= costco1, quote = TRUE, col.names = TRUE)
costco2 <- twListToDF(searchTwitter("costcocanada", n=1000, retryOnRateLimit=2000))
write.csv(file = paste("costco_mentions_", current_date,".csv"), x= costco2, quote = TRUE, col.names = TRUE)
costco3 <- twListToDF(searchTwitter("#costcocanada", n=2000, retryOnRateLimit=2000))
write.csv(file = paste("costco_hashtag_", current_date,".csv"), x= costco3, quote = TRUE, col.names = TRUE)
costco4 <- twListToDF(searchTwitter("@costcocanada", n=2000, retryOnRateLimit=2000))
write.csv(file = paste("costco_atmention_", current_date,".csv"), x= costco4, quote = TRUE, col.names = TRUE)
costco5 <- twListToDF(searchTwitter("to:costcocanada", n=1000, retryOnRateLimit=2000))
write.csv(file = paste("costco_touser_", current_date,".csv"), x= costco5, quote = TRUE, col.names = TRUE)
costco6 <- twListToDF(searchTwitter("from:@costcocanada", n=1000, retryOnRateLimit=2000))
write.csv(file = paste("costco_fromuser_", current_date,".csv"), x= costco6, quote = TRUE, col.names = TRUE)


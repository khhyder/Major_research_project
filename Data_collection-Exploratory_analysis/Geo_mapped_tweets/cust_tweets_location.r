install.packages("leaflet") #leaflet package is a popular java script library for interactive map
install.packages("maps") 
library(leaflet) 
library(maps)
all_ret<-read.csv("all_ret.csv", stringsAsFactors = FALSE) #
m1 <- leaflet(all_ret) %>% addTiles()
m %>% addCircles(lng = ~longitude, lat = ~latitude, popup = all_ret$type, weight = 8, radius = 40, color = "#fb3004", stroke = TRUE, fillOpacity = 0.8)
summary(all_ret)


wal<-read.csv("walmart.csv", stringsAsFactors = FALSE)
m <- leaflet(wal) %>% addTiles()
m %>% addCircles(lng = ~longitude, lat = ~latitude, popup = wal$type, weight = 8, radius = 40, color = "#fb3004", stroke = TRUE, fillOpacity = 0.8)
summary(wal)

cos<-read.csv("costco.csv", stringsAsFactors = FALSE)
m1 <- leaflet(cos) %>% addTiles()
m1 %>% addCircles(lng = ~longitude, lat = ~latitude, popup = cos$type, weight = 8, radius = 40, color = "#fb3004", stroke = TRUE, fillOpacity = 0.8)
summary(cos)

sob<-read.csv("sobeys.csv", stringsAsFactors = FALSE)
m2 <- leaflet(sob) %>% addTiles()
m2 %>% addCircles(lng = ~longitude, lat = ~latitude, popup = sob$type, weight = 8, radius = 40, color = "#fb3004", stroke = TRUE, fillOpacity = 0.8)
summary(sob)

lob<-read.csv("loblaw.csv", stringsAsFactors = FALSE)
m3 <- leaflet(lob) %>% addTiles()
m3 %>% addCircles(lng = ~longitude, lat = ~latitude, popup = lob$type, weight = 8, radius = 40, color = "#fb3004", stroke = TRUE, fillOpacity = 0.8)
summary(lob)


ct<-read.csv("ct.csv", stringsAsFactors = FALSE)
m4 <- leaflet(ct) %>% addTiles()
m4 %>% addCircles(lng = ~longitude, lat = ~latitude, popup = ct$type, weight = 8, radius = 40, color = "#fb3004", stroke = TRUE, fillOpacity = 0.8)
summary(ct)

#ref: https://opensource.com/article/17/6/collecting-and-mapping-twitter-data-using-r

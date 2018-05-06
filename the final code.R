##loading libraries 
library(dplyr)
library(readr)
library(ggplot2)
library(ggmap)
##reading the dataset 
cat = read_csv("~/Desktop/categorized dictionary with city name.csv")

#city and category 
citycat = cat%>%
  group_by(city_name, lex_cat)%>%
  summarize(n())

#changing column names 
citycat$city = citycat$city_name
citycat$emotion = citycat$lex_cat
citycat$word = citycat$`n()`

citycat = citycat%>%
  dplyr::select(c("city", "emotion", "word"))


city_words = citycat%>%
  group_by(city)%>%
  summarise(sum(word))


city_words_emotions = full_join(citycat, city_words, by = "city")

city_words_emotions = mutate(city_words_emotions, rel.emotion = word/sum(word))

##different emotions
#anger
city_words_emotions_ANGER = filter(city_words_emotions, emotion == "ANGER")
#anxiety
city_words_emotions_ANXIETY = filter(city_words_emotions, emotion == "ANX")
#positive emotions
city_words_emotions_POSEMO = filter(city_words_emotions, emotion == "E+")
#negative emotions
city_words_emotions_NEGEMO = filter(city_words_emotions, emotion == "E-")
#engagment 
city_words_emotions_ENGAGE = filter(city_words_emotions, emotion == "P+")
#disengagement 
city_words_emotions_DISENGAGE = filter(city_words_emotions, emotion == "P-")
#positive relationships
city_words_emotions_POSREL = filter(city_words_emotions, emotion == "R+")
#negative relationships
city_words_emotions_NEGREL = filter(city_words_emotions, emotion == "R-")



#getting map data 

USmap = map_data("county")
USmap$city = USmap$subregion

#merging 

data.ANGER = full_join(USmap, city_words_emotions_ANGER, by = "city")
data.ANX = full_join(USmap, city_words_emotions_ANXIETY, by = "city")
data.POSEMO = full_join(USmap, city_words_emotions_POSEMO, by = "city")
data.NEGEMO = full_join(USmap, city_words_emotions_NEGEMO, by = "city")
data.ENGAGE = full_join(USmap, city_words_emotions_ENGAGE, by = "city")
data.DISENGAGE = full_join(USmap, city_words_emotions_DISENGAGE, by = "city")
data.POSREL = full_join(USmap, city_words_emotions_POSREL, by = "city")
data.NEGREL = full_join(USmap, city_words_emotions_NEGREL, by = "city")

## Plots
#anger
ggplot(data.ANGER, aes(x = long, y = lat, group = group, fill = rel.emotion))+
  geom_polygon(color = "black", size = .06)+
  scale_fill_gradient(low = "white", high = "darkred", na.value = "gray", 
                      guide_legend("Anger"))+
  theme_void()

#anxiety
ggplot(data.ANX, aes(x = long, y = lat, group = group, fill = rel.emotion))+
  geom_polygon(color = "black", size = .06)+
  scale_fill_gradient(low = "white", high = "darkred", na.value = "gray", 
                      guide_legend("Anxiety"))+
  theme_void()

#positive emotions
ggplot(data.POSEMO, aes(x = long, y = lat, group = group, fill = rel.emotion))+
  geom_polygon(color = "black", size = .06)+
  scale_fill_gradient(low = "white", high = "darkred", na.value = "gray", 
                      guide_legend("Positive Emotions"))+
  theme_void()

#negative emotions
ggplot(data.NEGEMO, aes(x = long, y = lat, group = group, fill = rel.emotion))+
  geom_polygon(color = "black", size = .06)+
  scale_fill_gradient(low = "white", high = "darkred", na.value = "white", 
                      guide_legend("Negative Emotions"))+
  theme_void()

#engagment 
ggplot(data.ENGAGE, aes(x = long, y = lat, group = group, fill = rel.emotion))+
  geom_polygon(color = "black", size = .06)+
  scale_fill_gradient(low = "white", high = "darkred", na.value = "gray", 
                      guide_legend("Engagement"))+
  theme_void()

#disengagement 
ggplot(data.DISENGAGE, aes(x = long, y = lat, group = group, fill = rel.emotion))+
  geom_polygon(color = "black", size = .06)+
  scale_fill_gradient(low = "white", high = "darkred", na.value = "gray", 
                      guide_legend("Disengagement"))+
  theme_void()

#positive relationships
ggplot(data.POSREL, aes(x = long, y = lat, group = group, fill = rel.emotion))+
  geom_polygon(color = "black", size = .06)+
  scale_fill_gradient(low = "white", high = "darkred", na.value = "gray", 
                      guide_legend("Positive Relationships"))+
  theme_void()

#negative relationships
ggplot(data.NEGREL, aes(x = long, y = lat, group = group, fill = rel.emotion))+
  geom_polygon(color = "black", size = .06)+
  scale_fill_gradient(low = "white", high = "darkred", na.value = "gray", 
                      guide_legend("Negative Relationships"))+
  theme_void()


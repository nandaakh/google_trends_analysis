library(gtrendsR)
library(tidyverse)
library(ggforce)

# Pulling data with keyword "Disney+" and "Netflix" for the last 12 months
# For searches in Indonesia

res <- gtrends(keyword = c("Netflix"), time = "today 12-m", geo = "ID")
iot2022 <- res$interest_over_time

# generate "iot2022" to indicate interest over time to see weekly query most search
# then visualize it into line chart to gain insights

iot2022 %>% 
  ggplot(aes(x = date,
             y = hits,
             group=keyword,
             color = keyword))  +
  theme_dark()+
  labs(title = "Google Indonesia Web Searches for Netflix in 2022",
       subtitle = "Google Trends Report",
       caption = "Obs: 1/30 the day where Netflix was the most searches",
       x= NULL, y = "Interest")+
  ggforce::facet_zoom(xlim = c(as.POSIXct(as.Date("2021-12-26")),as.POSIXct(as.Date("2022-12-11")))) +
  geom_smooth(span=0.1,se=FALSE) +
  geom_point(color="orange")

# From script above we've seen that there is 100 value, that means the value 
# indicate as the most searched query.
# Next, we try to idnetify related queries most searched

res$related_queries %>%
  filter(related_queries=="top") %>%
  mutate(value=factor(value,levels=rev(as.character(value))),
         subject=as.numeric(subject)) %>%
  top_n(10,value) %>%
  ggplot(aes(x=value,y=subject,fill="orange")) + 
  geom_bar(stat='identity',show.legend = F) + 
  coord_flip() + labs(title="Queries most related with Netflix")

# Script below is to show most topics have been searched for the last 12 months

res$related_topics %>%
  filter(related_topics == "top") %>%
  mutate(value=factor(value,levels=rev(as.character(value))),
         subject=as.numeric(subject)) %>%
  top_n(10,value) %>%
  ggplot(aes(x=value,y=subject,fill="red")) + 
  geom_bar(stat='identity',show.legend = F) + 
  coord_flip() + labs(title="Most Searched Topics Related with Netflix")


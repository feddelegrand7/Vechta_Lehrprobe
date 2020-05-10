library(tidyverse)
library(data.table)


# loading and cleaning ----------------------------------------------------


df <- read_csv("donations.csv", col_types = cols(Dollars = col_number()), guess_max = 69800)

df <- janitor::clean_names(df)



# Analysis ----------------------------------------------------------------


sum(df$dollars)

max(df$date_reported, na.rm = T)

df$date_reported <- lubridate::mdy(df$date_reported)

sum(df$dollars)


names(df)


df %>% group_by(recipient) %>% 
  summarise(sum = sum(dollars)) %>% 
  arrange(desc(sum)) %>% 
  top_n(10)



View(df %>% arrange(desc(dollars)))

nrow(df)

df

df2 <- df %>% mutate(Amount = case_when(
  
  dollars > 1000000000 ~ "> $1 Bill", 
  
  TRUE ~ "< $1 Bill"
  
  
))

p1 <- ggplot(df2, aes(year, dollars)) +
  geom_col(aes(fill = Amount)) +
  theme_minimal() +
  scale_fill_brewer(type = "qual") +
  labs(title = "The amount of gifts declined sharply from 2006", caption = "by Mohamed El Fodil Ihaddaden based on the Million Dollar List Data Set")

plotly::ggplotly(p1)



names(df)


df3 <- df2 %>% arrange(desc(dollars))

ggplot(filter(df2, dollars < 1000000000 &
                !is.na(recipient_continent)),
       aes(recipient_continent, dollars)) +
  geom_jitter(alpha = 0.2, na.rm = T, col = "#4E5C68") +
  theme_minimal() +
  coord_flip() +
  labs(x = "", subtitle  = "US charity donations (less than $1 Bill) per Continent") 







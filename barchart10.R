library(tidyverse)
library(readr)
df1 <- read_csv("merit list 2021.csv", col_types = "ci")


df1 <-  df1 %>% 
  mutate(Tenths = ntile(Merit_order,10))
df1$Tenths <- as.integer(df1$Tenths)

df1 <-  df1 %>% 
  group_by(University) %>% 
  mutate(utotal = n())

df1 <-  df1 %>% 
  group_by(University, Tenths) %>% 
  mutate(utenths = n())

df1 <-  df1 %>% 
  group_by(University, Tenths) %>% 
  mutate(utenths_p = utenths*100/utotal)

df10 <- df1 %>% 
  distinct(University, Tenths, .keep_all = TRUE)

ggplot(data = df10, mapping = aes(x = University, y = utenths_p, fill = Tenths)) + 
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = seq(0, 100, 20)) +
  labs(y = "Percent of Students")


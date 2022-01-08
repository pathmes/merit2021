# Load required packages
library(tidyverse)
library(readr)
library(colorspace)

# Import data
df1 <- read_csv("merit list 2021.csv", col_types = "fi")


# Create 10 ordered groups (Tenths) based on 'Merit_order'
df1 <-  df1 %>% 
  mutate(Tenths = desc(ntile(Merit_order,10)))
# df1$Tenths <- as.integer(df1$Tenths)

# Create the variable - 'utotal' - indicating the total number of students 
# per university
df1 <-  df1 %>% 
  group_by(University) %>% 
  mutate(utotal = n())

# Create the variable - 'utenths' indicating the number of students in
# each category of Tenths per university
df1 <-  df1 %>% 
  group_by(University, Tenths) %>% 
  mutate(utenths = n())

# Create the variable - 'utenths_p' indicating the percentage of students 
# within each university in each category of Tenths
df1 <-  df1 %>% 
  group_by(University, Tenths) %>% 
  mutate(utenths_p = utenths*100/utotal)




# Keep just one record for each unique combination of university & tenths
df10 <- df1 %>% 
  distinct(University, Tenths, .keep_all = TRUE)


# Identify the merit position of the middle student of each university - 'medrank'
df1 <-  df1 %>% 
  group_by(University) %>% 
  mutate(medrank = median(Merit_order))

# Create a data frame of University ranked by median merit position 'mr'
df8 <- df1 %>% ungroup() %>%
  distinct(University,  .keep_all = TRUE)
df8$mr <- rank(df8$medrank)
df8 <- df8 %>% select(University, mr)

# Add the variable 'mr' to the dataframe used to make the graphs
df10 <- inner_join(df8, df10, by = "University")



ggplot(data = df10, mapping =
         aes(x = fct_reorder(University, mr), y = utenths_p,
             fill = Tenths)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = seq(0, 100, 10)) +
  labs(y = "Cumulative Percent of Students",
       x = "University",
       title = "Common Merit List Positions by University - 2021",
       caption = "Data from the UGC",
       fill = "Merit Order")  +
  scale_fill_continuous_divergingx(palette = "Geyser",
                                   mid = -5, rev = TRUE,
                                   breaks=c(-1.5,-5, -9.5),
                                   labels=c("Top","Middle","Bottom")) + 
  theme(panel.background = element_rect(fill = "gray70"))


# Add boundary lines between components of the bars
ggplot(data = df10, mapping =
aes(x = fct_reorder(University, mr), y = utenths_p,
fill = Tenths)) +
geom_bar(stat = "identity", color = "gray60") +
scale_y_continuous(breaks = seq(0, 100, 10)) +
labs(y = "Cumulative Percent of Students",
x = "University",
title = "Common Merit List Positions by University - 2021",
caption = "Data from the UGC",
fill = "Merit Order")  +
scale_fill_continuous_divergingx(palette = "Geyser",
mid = -5, rev = TRUE,
breaks=c(-1.5,-5, -9.5),
labels=c("Top","Middle","Bottom")) +
theme(panel.background = element_rect(fill = "gray60"))


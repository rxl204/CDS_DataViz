library(babynames)
library(ggplot2)
library(magrittr) #for chain operators %>% e.g. to pipe a value forward
library(dplyr) #for data manipulation

#check data
str(babynames)
head(babynames)
tail(babynames)

#number of unique names
length(unique(babynames$name))

sum(babynames$n)/10^6

#plot a name over time
ggplot(babynames, aes(year, n)) + 
  geom_line(data = filter(babynames, name == "Elizabeth"))

#check data
head(filter(babynames, name == "Elizabeth"))
#there are female and male enteries for some names

#plot a single name over time 
ggplot(babynames, aes(year, n)) + 
  geom_line(data = filter(babynames, name == "Elizabeth"), aes(color=sex))

#top 10 names of all time 
top10 <- babynames %>%
  group_by(sex, name) %>%
  summarize(total = sum(n)) %>%
  arrange(desc(total)) %>%
  group_by(sex) %>%
  mutate(rank=row_number()) %>%
  filter(rank<=10) %>%
  arrange(sex, rank)
top10f <- top10 %>% filter(sex=="F")
top10m <- top10 %>% filter(sex=="M")

#plot 10 most common names for boys and girls
babynames %>%
  filter(sex=="F") %>%
  filter(name %in% top10f$name) %>%
  ggplot(., aes(year, n)) +
  geom_line(aes(color=name, group=name))

babynames %>%
  filter(sex=="M") %>%
  filter(name %in% top10m$name) %>%
  ggplot(., aes(year, n)) +
  geom_line(aes(color=name, group=name))

#plot 10 most common names in 2013 
babynames %>%
  filter(year == 2013) %>%
  group_by(name) %>%
  summarize(total = sum(n)) %>%
  arrange(desc(total)) %>%
  mutate(rank=row_number()) %>%
  filter(rank<=10) %>%
  arrange(rank)

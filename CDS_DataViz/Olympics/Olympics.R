# Install the required packages
# install.packages("ggplot2")

# Libraries
library("ggplot2")
library(dplyr)  
library(magrittr)
library(lattice)
library(plotly)

#read in data
wdf = read.csv("winter.csv")
head(wdf)

ddf = read.csv("dictionary.csv")
head(ddf)

#groupby country for medal tally
df <- wdf %>%
  group_by(Country, Medal) %>%
  summarise(Medal_count = length(Medal))

#identify countries that are defunct(?)
defunct <- setdiff(df$Country,ddf$Code)

#drop records for these countries
df1 <-df[ ! df$Country %in% defunct, ]

#plot medal tally for all countries
barchart(Country ~ Medal_count, groups=Medal, data=df1, stack=TRUE)

#filter for top 10
df10 <- aggregate(df1$Medal_count, by=list(Category=df1$Country), FUN=sum)
dfsum <- head(df10[order(df10$x, decreasing=TRUE), ], 10)
dftop10 <- df1[df1$Country %in% df$Category, ]

#plot total medal count for top 10
barchart(Category ~ x, data=dfsum, col="blue")

#grouping by gender
dfg <- wdf %>%
  group_by(Country, Medal, Gender) %>%
  summarise(Medal_count = length(Medal))
#country-wise medal sum by gender
dfg <- ddply(dfg, .(Country, Gender), numcolwise(sum))
dfg10 <- dfg[dfg$Country %in% dfsum$Category, ]
#plot medal count by gender
barchart(Country ~ Medal_count, groups=Gender, data=dfg10, stack=TRUE)


## QUESTION 2 ##
#using sum of number of medals (df10)

#first merge winter and dictionary
m1 <- merge(df10, ddf, by.x = "Category", by.y = "Code")
#recalculate scoring
m1$gdp_medal <- m1$GDP.per.Capita / m1$x
m1$pop_medal <- m1$Population / m1$x
#re-rank GDP
m1$gdp_rank <- NA
order.gdp_rankmedal <- order(m1$gdp_medal)
m1$gdp_rank[order.gdp_rankmedal] <- 1:nrow(m1)

#re-rank Population
m1$pop_rank <- NA
order.pop_rankmedal <- order(m1$pop_medal)
m1$pop_rank[order.pop_rankmedal] <- 1:nrow(m1)

#normal ranking
m1$count_rank <- NA
order.count_rankmedal <- order(m1$x)
m1$count_rank[order.count_rankmedal] <- 1:nrow(m1)

head(m1)

#plotting ranking
plot_ly(
  x = m1$Category,
  y = m1$gdp_medal,
  name = "gdp",
  type = "bar"
)

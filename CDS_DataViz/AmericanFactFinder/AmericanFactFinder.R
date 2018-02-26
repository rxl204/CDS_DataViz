library(tidyverse)
library(ggplot2)
library(stringr)
library(dplyr)
# read in data
income <- read.csv("/Users/rachellim/CUSP/CDS_DataViz/ACS_16_5YR_S1903_with_ann.csv", stringsAsFactors = FALSE)
head(income)

#The first row of the data frame contains the description of what we want to see, 
#so lets create another object with variable names - first row of our data.
get_names <- function(input_data) {
  # Function to retrive the row names and first column of any given data frame
  # Returns a data frame out_data, where the first column are the column names of 
  #     input data and the second column is the first row of column names
  
  out_data <- as.data.frame(names(input_data))
  names(out_data) <- "original_names"
  out_data$explanation <- t(input_data[1,]) 
  # We're transposing the first column of input_data, using matrix notation to handle data.frames
  return(out_data)
}

names_income <- get_names(income)

education <- read.csv("/Users/rachellim/CUSP/CDS_DataViz/ACS_16_5YR_S1401_with_ann.csv", stringsAsFactors = FALSE)
names_education <- get_names(education)

#clean data
data_cleaner <- function(input_data){
  
  # Function to clean the data frame from American Fact Finder WITH annotations.
  
  # Drop the first row (which we already used):
  out_data <- input_data[2:nrow(input_data),]
  
  
  for(x in 4:ncol(out_data)){
    # First, remove all the commas, so numbers like "2,700" == "2700"
    out_data[,x] <- gsub(',', '', out_data[,x])
    # gsub is 
    
    # Coerce to numeric (will ignore characters and throw a warning)
    out_data[,x] <- suppressWarnings(as.numeric(out_data[,x]))
  }
  
  return(out_data)
  
}

income <- data_cleaner(income)
education <- data_cleaner(education)

#compare median income of hispanic vs non-hispanic
plot <- ggplot(income, aes(x= HC02_EST_VC12, y= HC02_EST_VC13)) +
  geom_point()
#plot
plot <- ggplot(income, aes(x= HC02_EST_VC12, y= HC02_EST_VC13)) +
  geom_point() + 
  xlab("Median income (dollars); \nHispanic or Latino origin (of any race)") + 
  ylab("Median income (dollars); \nWhite alone, not Hispanic or Latino") 
#use "\n" to break text into two lines
plot

#focus on counties inside NYS
#create a variable indicating which are NY counties
state <- str_split(income$GEO.display.label, ",") 
income$state<- do.call(rbind, state)[,2]
income$state <- str_trim(income$state, side="both")
rm(state)
table(income$state)

income$is_newYork <- "Rest of the country"
income$is_newYork[income$state=="New York"] <- "New York"
#default color
plot <- ggplot(income, aes(x= HC02_EST_VC12, y= HC02_EST_VC13)) +
  geom_point(aes(color=is_newYork)) + 
  xlab("Median income (dollars); \nHispanic or Latino origin (of any race)") + 
  ylab("Median income (dollars); \nWhite alone, not Hispanic or Latino") 

#changing color
plot <- ggplot(income, aes(x= HC02_EST_VC12, y= HC02_EST_VC13)) +
  geom_point(aes(color=is_newYork)) + 
  xlab("Median income (dollars); \nHispanic or Latino origin (of any race)") + 
  ylab("Median income (dollars); \nWhite alone, not Hispanic or Latino") +
  scale_color_manual(values=c("#7fcdbb", "#253494"))
plot
#bringing NY points above
plot <- ggplot() +
  geom_point(data=subset(income, income$is_newYork=="Rest of the country"), aes(x= HC02_EST_VC12, y= HC02_EST_VC13, color=is_newYork)) + 
  geom_point(data=subset(income, income$is_newYork=="New York"), aes(x= HC02_EST_VC12, y= HC02_EST_VC13, color=is_newYork)) +
  xlab("Median income (dollars); \nHispanic or Latino origin (of any race)") + 
  ylab("Median income (dollars); \nWhite alone, not Hispanic or Latino") + 
  scale_color_manual(values=c("#7fcdbb", "#253494"), name="")
plot
#plot 45 degree line for easier comparison
plot <- ggplot() +
  geom_point(data=subset(income, income$is_newYork=="Rest of the country"), aes(x= HC02_EST_VC12, y= HC02_EST_VC13, color=is_newYork)) + 
  geom_point(data=subset(income, income$is_newYork=="New York"), aes(x= HC02_EST_VC12, y= HC02_EST_VC13, color=is_newYork)) +
  geom_line(data=income, aes(x= HC02_EST_VC12, y= HC02_EST_VC12), color="red", linetype = 2) +
  xlab("Median income (dollars); \nHispanic or Latino origin (of any race)") + 
  ylab("Median income (dollars); \nWhite alone, not Hispanic or Latino") + 
  scale_color_manual(values=c("#7fcdbb", "#253494"), name="")

plot
#change theme
plot <- ggplot() +
  geom_point(data=subset(income, income$is_newYork=="Rest of the country"), aes(x= HC02_EST_VC12, y= HC02_EST_VC13, color=is_newYork)) + 
  geom_point(data=subset(income, income$is_newYork=="New York"), aes(x= HC02_EST_VC12, y= HC02_EST_VC13, color=is_newYork)) +
  geom_line(data=income, aes(x= HC02_EST_VC12, y= HC02_EST_VC12), color="red", linetype = 2) +
  xlab("Median income (dollars); \nHispanic or Latino origin (of any race)") + 
  ylab("Median income (dollars); \nWhite alone, not Hispanic or Latino") + 
  scale_color_manual(values=c("#7fcdbb", "#253494"), name="") + 
  theme_bw() + 
  ggtitle("Median Income in Hispanic Housholds VS Non-Hispanic white housholds", subtitle = "Last 5 years in 2016 adjusted USD.")

plot

#Income vs College Enrollment
education_2 <- education[c("GEO.id2", "GEO.display.label", "HC02_EST_VC11")]
income_2 <- income[c("GEO.id2", "HC02_EST_VC02")]

# Percent; Estimate; Population enrolled in college or graduate school
# Median income (dollars); Estimate; Households

data_plot <- merge(education_2, income_2, by="GEO.id2")
#rename headers
names(data_plot) <- c("id", "county", "education", "income")
rm(education_2, income_2)

# Almost the same as before!!!
state <- str_split(data_plot$county, ",") 
data_plot$state<- do.call(rbind, state)[,2]
data_plot$state <- str_trim(data_plot$state, side="both")
rm(state)

data_plot$is_newYork <- "Rest of the country"
data_plot$is_newYork[data_plot$state=="New York"] <- "New York"

# Almost the same as before!!!
state <- str_split(data_plot$county, ",") 
data_plot$state<- do.call(rbind, state)[,2]
data_plot$state <- str_trim(data_plot$state, side="both")
rm(state)

data_plot$is_newYork <- "Rest of the country"
data_plot$is_newYork[data_plot$state=="New York"] <- "New York"

plot <- ggplot() +
  geom_point(data=subset(data_plot, data_plot$is_newYork=="Rest of the country"), aes(x= education, y= income, color=is_newYork)) + 
  geom_point(data=subset(data_plot, data_plot$is_newYork=="New York"), aes(x= education, y= income, color=is_newYork)) +
  geom_smooth(data=data_plot, aes(x= education, y= income), color="red", linetype = 2, method="lm", formula= y ~ x) +
  xlab("Percent; Population enrolled in college or graduate school") + 
  ylab("Median income (dollars); Estimate; Households") + 
  scale_color_manual(values=c("#7fcdbb", "#253494"), name="") + 
  theme_bw() + 
  ggtitle("Median Income in Housholds VS % of people enrolled in college per county", subtitle = "Last 5 years. Income in 2016 adjusted USD.")

plot

#plot bar chart 
#compare the % of population in private school vs population in private school 
education$state <- do.call(rbind, state)[,2]
education$state <- str_trim(education$state, side="both")
rm(state)
#subset data for only NYS
data.private.education <- subset(education, education$state == "New York")
#keep only variables of interest
data.private.education <- data.private.education[c("GEO.id", "GEO.id2", "GEO.display.label", "HC06_EST_VC01", "HC04_EST_VC01")]
names(data.private.education) <- c("id", "id2", "name", "per_Private", "per_Public") # renaming vars
data.private.education <- reshape(data.private.education, direction="long", varying=c("per_Private", "per_Public"), sep="_", timevar="type")
# reshaping data is a very usefull command, but as all tricks, can be tricky. Check out how I named the variables as a trick. Another option is "spred()" or "gather()" commands from tidyverse (http://tidyr.tidyverse.org/)
# By default of reshape(), the row names are set to a concatennetion of the ids. I personally prefer to change them: 
row.names(data.private.education) <- seq(from=1, to=nrow(data.private.education))

#plot stacked bars
plot <- ggplot(data=data.private.education, aes(x=name, y=per, fill=type))+
  geom_bar(stat="identity")

# I'm going to start by modifying the data, so we don't have " County, New York" in the names of the countys.
data.private.education$name <- gsub(" County, New York", "", data.private.education$name)

# And we redefine the factor to change the labels. Remeber, we already have the data sorted from above ;-). 
data.private.education$name_factor = factor(data.private.education$name, unique(data.private.education$name))

plot <- ggplot(data=data.private.education, aes(x=name_factor, y=per, fill=type))+
  geom_bar(stat="identity") + xlab("") + ylab("Percent") + 
  theme(axis.text.x = element_text(angle=90, size=rel(0.8), hjust=1))

plot 

plot <- ggplot(data=data.private.education, aes(x=name_factor, y=per, fill=type))+
  geom_bar(stat="identity") + xlab("") + ylab("Percent") + 
  scale_fill_manual(values = c("#7fcdbb", "#2c7fb8"), name="Type of \n Education") + 
  coord_flip() +
  ggtitle("Public and Private School Enrollment", sub = "Population 3 years and over enrolled in school") +
  theme(axis.text.y = element_text(size=rel(0.6)))

# Check out how I modifies the theme. Now, we want to modify what's on axis y, since we flipped the coords before calling theme. 
plot 

#remove blank space at 0 and 100
# What did I do here?
# - Removed the fill_manual command. 
# - Added facet_wrap. 
# - fill is now outside of aes(). 
# - Added strip.background in theme to remove gray from plots subtitles. 
# - Added manually reference lines to x with panel.grid.major.x in theme. 

plot <- ggplot(data=data.private.education, aes(x=name_factor, y=per))+
  geom_bar(stat="identity", fill="#2c7fb8") + xlab("") + ylab("Percent") + 
  facet_wrap(~type) +
  scale_y_continuous(expand = c(0,0)) +
  coord_flip() +
  ggtitle("Public and Private School Enrollment", sub = "Population 3 years and over enrolled in school") +
  theme(axis.text.y = element_text(size=rel(0.6)), 
        strip.background = element_rect(fill="white"), 
        panel.background = element_blank(), 
        panel.grid.major.x = element_line(colour = "gray", size = rel(1))) 
plot 

rm(data.private.education, plot)

# First, let me subset the data, so we only have NY state
data.private.education <- subset(education, education$state=="New York") 

data.private.education <- data.private.education[c("GEO.id", "GEO.id2", "GEO.display.label", "HC06_EST_VC07", "HC06_MOE_VC07", "HC06_EST_VC11", "HC06_MOE_VC11")] 

names(data.private.education) <- c("id", "id2", "name", "per_HighSchool", "mar_HighSchool", "per_University", "mar_University")

data.private.education <- reshape(data.private.education, direction="long", varying=c("per_HighSchool", "mar_HighSchool", "per_University", "mar_University"), sep="_", timevar="type")

row.names(data.private.education) <- seq(from=1, to=nrow(data.private.education))

data.private.education$type[data.private.education$type=="HighSchool"] <- "High School"
data.private.education$type[data.private.education$type=="University"] <- "College or Graduate School"

data.private.education$name <- gsub(" County, New York", "", data.private.education$name)

data.private.education <- data.private.education %>%
  arrange(per, type) %>%
  mutate(name_factor = factor(name, unique(name))) 

head(data.private.education)

#scatterplot
plot <- ggplot(data=data.private.education) + 
  geom_point(aes(x=name_factor, y=per)) + 
  facet_wrap(~type)
plot

#add margins
data.private.education <- data.private.education %>% 
  mutate(margin_top = per + mar) %>%  
  mutate(margin_low = per - mar)
plot <- ggplot(data=data.private.education) + 
  geom_point(aes(x=name_factor, y=per)) + 
  geom_segment(aes(x=name_factor, xend=name_factor, y=margin_low, yend=margin_top)) + 
  facet_wrap(~type) + xlab("") + ylab("Percent") + 
  theme(axis.text.x = element_text(angle=90, size=rel(0.5), hjust=1)) 

plot

# What did I do? 
# - Changed colors using scale_color_manual()
# - Added a title with ggtitle()
# - Removed the panel background with panel.background inside theme
# - Added guides on X axis with panel.grid.major.x
# - Added thick X and Y axis lines axis.line inside theme. 
# - Cropped the extra space below zero with scale_y_continuous
# - Added a sign of "%" next to the yticks, specifying breaks and labels inside of  scale_y_continuous
# - Changed the position of the legend to be on top with legend.position inside theme
# - Changed the justification of the legend to the left with legend.justification inside theme. 

plot <- ggplot(data=data.private.education) + 
  geom_segment(aes(x=name_factor, xend=name_factor, y=margin_low, yend=margin_top, color=type)) + 
  geom_point(aes(x=name_factor, y=per, color=type)) + 
  scale_color_manual(values=c("#b2182b", "#2166ac"), name="") + 
  xlab("") + ylab("Percent") + 
  scale_y_continuous(expand = c(0,0), breaks=seq(20,80,20), labels = paste0(seq(20,80,20), "%")) + 
  ggtitle("Percent of population in private schools") + 
  theme(axis.text.x = element_text(angle=90, size=rel(0.8), hjust=1), 
        panel.background = element_blank(), 
        panel.grid.major.x = element_line(colour = "#DBDBDB", size = rel(1)), 
        axis.line = element_line(colour = "black", size=rel(1.5)), 
        legend.position = "top", 
        legend.justification = "left") 
plot

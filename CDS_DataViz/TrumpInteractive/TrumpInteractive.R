library(lubridate)
library(ggplot2)
library(plotly)
library(DT)

trump.data <- read.csv("/Users/rachellim/CUSP/CDS_DataViz/TrumpInteractive/approval_topline.csv", stringsAsFactors = F)
head(trump.data)

#modeldate variable as data 
trump.data$modeldate <- mdy(trump.data$modeldate)
head(trump.data)

# Create margins of errores in the data frame: 

plot <- ggplot(data=trump.data) + 
  geom_line(aes(x=modeldate, y=approve_estimate), colour="#006837") + 
  geom_line(aes(x=modeldate, y=approve_hi), linetype="dashed", colour="#bdbdbd") + 
  geom_line(aes(x=modeldate, y=approve_lo), linetype="dashed", colour="#bdbdbd") + 
  facet_wrap(~subgroup) + 
  ylab("Percentage") + 
  ggtitle("Trump Approval Ratings", sub="Estimations from FiveThirtyEight") + 
  xlab("") + 
  theme_bw() + 
  scale_y_continuous(breaks=seq(10,60,5), labels = paste0(seq(10,60,5), "%")) + 
  theme(axis.text.x = element_text(angle=45, size=rel(0.8), hjust=1), 
        panel.background = element_blank(), 
        strip.background = element_rect(fill="white"),
        panel.grid.major.x = element_line(colour = "#DBDBDB", size = rel(1)), 
        axis.line = element_line(colour = "black", size=rel(1.5))) 

plot

ggplotly(plot)

# This code was adapted from the tutorial from plotly. 
# Remember, checking documentation is ALWAYS a good idea ;-) 
#   Check: https://plot.ly/r/subplots/ & https://plot.ly/r/line-charts/ 

sub1 <- plot_ly(data=subset(trump.data, trump.data$sub=="Adults"), x = ~modeldate, y = ~approve_estimate, 
                name="Approval Estimate", type = 'scatter', mode = 'lines', 
                line = list(color = '#006837', width = 2)) %>% 
  add_trace(y = ~approve_lo, name = 'Low Margin of Error', 
            line = list(color = '#DBDBDB', width = 2, dash = 'dash')) %>%
  add_trace(y = ~approve_hi, name = 'High Margin of Error', 
            line = list(color = '#DBDBDB', width = 2, dash = 'dash')) 

sub2 <- plot_ly(data=subset(trump.data, trump.data$sub=="All polls"), x = ~modeldate, y = ~approve_estimate, 
                name="Approval Estimate", type = 'scatter', mode = 'lines', 
                line = list(color = '#006837', width = 2)) %>% 
  add_trace(y = ~approve_lo, name = 'Low Margin of Error', 
            line = list(color = '#DBDBDB', width = 2, dash = 'dash')) %>%
  add_trace(y = ~approve_hi, name = 'High Margin of Error', 
            line = list(color = '#DBDBDB', width = 2, dash = 'dash')) 

sub3 <- plot_ly(data=subset(trump.data, trump.data$sub=="Voters"), x = ~modeldate, y = ~approve_estimate, 
                name="Approval Estimate", type = 'scatter', mode = 'lines', 
                line = list(color = '#006837', width = 2)) %>% 
  add_trace(y = ~approve_lo, name = 'Low Margin of Error', 
            line = list(color = '#DBDBDB', width = 2, dash = 'dash')) %>%
  add_trace(y = ~approve_hi, name = 'High Margin of Error', 
            line = list(color = '#DBDBDB', width = 2, dash = 'dash')) 

plot <- subplot(sub1, sub2, sub3, shareY = T) %>% 
  layout(title = "Trump Approval Ratings \n Estimations from FiveThirtyEight",
         xaxis = list(title = ""),
         yaxis = list (title = "Percentage"), 
         showlegend=FALSE, 
         annotations = list(
           list(x = 0.15 , y = 1.05, text = "Adults", showarrow = F, xref='paper', yref='paper'),
           list(x = 0.5 , y = 0.95, text = "All polls", showarrow = F, xref='paper', yref='paper'), 
           list(x = 0.85 , y = 1.05, text = "Voters", showarrow = F, xref='paper', yref='paper')))

plot

# Yes! That's right, by default the xaxis was the other way around.  

cats <- c("Adults", "All polls", "Voters")
subplots <- list()

for(x in 1:length(cats)){
  
  data.temp <- trump.data %>%
    filter(subgroup==cats[x])  %>% 
    arrange(modeldate) 
  
  subplots[[x]] <- highchart() %>%
    hc_xAxis(categories = data.temp$modeldate) %>% 
    hc_title(text = "") %>% 
    hc_subtitle(text = cats[x]) %>% 
    hc_add_series(data = data.temp$approve_estimate, name = "Estimate", color='#006837')  %>% 
    hc_add_series(data = data.temp$approve_hi, name = "High Margin of Error", color='#DBDBDB', dashStyle="Dash")  %>% 
    hc_add_series(data = data.temp$approve_lo, name = "Low Margin of Error", color='#DBDBDB', dashStyle="Dash")  %>%
    hc_legend(enabled=FALSE)
  
  rm(data.temp)              
  
}
rm(x)

plot <- combineWidgets(subplots[[1]], subplots[[2]], subplots[[3]], ncol=3, 
                       title="Trump Approval Ratings", 
                       footer="Estimations from FiveThirtyEight")
plot

plot <- hchart(trump.data, "line", hcaes(x = modeldate, y = approve_estimate, group = subgroup)) %>%
  hc_title(text = "Trump Approval Ratings") %>% 
  hc_subtitle(text = "Estimations from FiveThirtyEight") %>%
  hc_colors(c("#c7e9b4", "#1d91c0", "#0c2c84")) %>%
  hc_xAxis(title = list(text = "")) %>%
  hc_yAxis(title = list(text = "Percentage")) 

plot

plot <- hchart(trump.data, "line", hcaes(x = modeldate, y = approve_estimate, group = subgroup)) %>%
  hc_title(text = "Trump Approval Ratings") %>% 
  hc_subtitle(text = "Estimations from FiveThirtyEight") %>%
  hc_xAxis(title = list(text = "")) %>%
  hc_yAxis(title = list(text = "Percentage")) %>%
  hc_add_theme(hc_theme_538())

plot
#data tables
table <- trump.data %>% 
  rename(approve_high = approve_hi, approve_low = approve_lo) %>%
  select(subgroup, modeldate, approve_estimate, approve_high, approve_low) %>%
  arrange(modeldate)  %>%
  datatable(rownames = FALSE,
            colnames = c("Subgroup", "Date", "Estimate", "Low Margin of Error", "High Margin of Error"), 
            caption =  "Trump Approval Ratings: Estimations from FiveThirtyEight", 
            filter = "top")

table

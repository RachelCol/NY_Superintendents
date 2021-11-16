# MAP of Survey Respondents by County

#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("usmap")
library("usmap")
library("ggplot2")
library("dplyr")

# -----

# CREATE TWO-COLUMN MAP DATA SET

# merge data sets by county_name
map_data <- NYS[, c("fips", "countn")]
# combine duplicate rows, adding the "countn" column for number of respondents
map_data <- aggregate(countn ~ fips, map_data, sum)
sum(map_data$countn) # test to make sure it adds up to 133

# -----

# CREATE map showing where respondents are from
respondent_map <- plot_usmap(data = map_data, values = "countn", regions = "counties", 
           include = c("NY"), color = "white") + 
  scale_fill_distiller(palette = "PiYG", name = "Number of \nRespondents") +
  theme(legend.position = "bottom") +
  labs(title = "Number of Respondents by County", 
       subtitle = "2016 NY Superintendents Survey") + 
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), 
        plot.subtitle = element_text(hjust = 0.5, size = 12), 
        legend.title = element_text(size = 10))
respondent_map

# SAVE map as PDF file in project folder
pdf(file="NY_state_map.pdf")
respondent_map
dev.off()

# END SCRIPT

library(tidyverse)
library(gvlma)

set.seed(123)
df1 <- as.data.frame(matrix(runif(1*50, min = 1, max = 10), ncol = 1)) %>%
    mutate(epsilon = rnorm(50, mean = 0, sd = 2)) %>%
    mutate(response = 3 - 2* V1  + epsilon) %>%
    mutate(group = ifelse(V1 <= 5, 1,2))

#We made a boxplot by using V1 on the y-axis and the group on the x-axis
ggplot(df1, aes(group=group ,x=group, y=V1)) + 
    geom_boxplot(fill = 'Red')


reg1 <- lm(data=df1, response ~ V1)
summary(reg1)

gvlma(reg1)

#We plotted the regression line, which is the downward sloping red line. We did this by using the Response for the y-axis and the Predictor for the x-axis
plot(x=df1$V1, y=df1$response,col= 'Blue', xlab= 'Predictor', ylab='Response') + abline(lm(response~ V1, data = df1), col='Red')


library(thematicmaps)
library(tidyverse)

#We uploded the nld_municipal map to get a table of different variables
map_municipal <- read.csv2('nld_municipal_map.csv', stringsAsFactors = FALSE, dec = '.')
head(map_municipal)

#we need to use Map plot to make a map of the Netherlands
AddMapLayer(MapPlot(), map_municipal)

#We got the X and Y as numerical variables for the pc4 locations
pc4_locations <- read.csv2('nld_pc4_locations.csv', stringsAsFactors = FALSE, dec = '.') 

head(pc4_locations)

#we needed to upload the schools file such that we could read this and get information such as the "Postcode"
schools <- read.csv2('schools.csv')
head(schools)

#We need to select the PC4 and see which Denominatie it has, we did this by using the folowing:
schools1 <- schools %>% mutate(PC4=as.numeric(substr(POSTCODE,1,4))) %>% 
select (PC4, DENOMINATIE)
schools1


school_loc <- merge(pc4_locations,schools1, by = 'PC4')
schools2 = filter(school_loc,DENOMINATIE %in% c('Rooms-Katholiek','Protestants-Christelijk'))
schools2 

#We need to create a map of the Netherlands and using dots to see where the Catholic an Protestant schools are in the Netherlands, so we add: AddPointsLayer
AddMapLayer(MapPlot(), map_municipal) %>%
AddPointsLayer(schools2)

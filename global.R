####libraries
library(dplyr)
library(ggplot2)
library(stringr)
library(readr)
library(jsonlite)
library(tufte)
library(ggthemes)
library(plotly)
library(DT)
library(shiny)
library(googleVis)


####get geo data
###for countries and continents
json_file <- 'https://datahub.io/JohnSnowLabs/country-and-continent-codes-list/datapackage.json'
json_data <- fromJSON(paste(readLines(json_file), collapse=""))
##list of files
print(json_data$resources$name)

# print all tabular data(if exists any)
for(i in 1:length(json_data$resources$datahub$type)){
  if(json_data$resources$datahub$type[i]=='derived/csv'){
    path_to_file = json_data$resources$path[i]
    countries_continents = read.csv(url(path_to_file))
  }
}

#first checking out players.csv, which shows player personal info, such as measurements and birth info.
players_df = read.csv("Players.csv", stringsAsFactors = F)
bball_info = read.csv("player_data.csv", stringsAsFactors = F)
season_stats = read.csv("Seasons_Stats.csv", stringsAsFactors = F)

#weight coversion == kg to lbs
kg_2_lbs = function(kg){
  kg * 2.205
}

#height coversion == cm to inches
cm_2_inches = function(cm){
  cm / 2.54
}

#### data cleanup
#change column collage to college

players_df %>%
  rename(college = collage) 

players_df %>% 
  distinct(birth_state)

####enhancements
###column enhancements to players_df
players_df = players_df %>% 
  rename(height_cm = height) %>% 
  rename(weight_kg = weight) %>% 
  mutate("weight_lbs" = kg_2_lbs(weight_kg)) %>% 
  mutate("height_inches" = cm_2_inches(height_cm))




####mutate continent code so North America == reg NA
#countries_continents %>% 
#  mutate(ifelse(Continent_Code == 'NA','N.A.',Continent_Code)) %>% 
#  View()


###USA states data
states_usa = read.csv("states_usa.csv")


#data subsets

stats_25plusPER = season_stats %>%
  select(Year,Player,PER,MP,PTS,G,Pos,DBPM,WS,BPM) %>%
  mutate(MPG = MP/G,
         PPG = PTS/G) %>% 
  filter(PER >= 25,
         MPG >= 25,
         Year >= 1978)

#Graphs
scat_PERvsDBPM = stats_25plusPER %>% 
  plot_ly(
  type = 'scatter',
  mode = 'markers',
  x = ~PER,
  y = ~DBPM,
  marker = list(size = ~WS, sizemode = 'area'),
  color = ~Pos,
  text = ~paste(Player," - ",Year),
  hovertemplate = paste(
    "<b>%{text}</b><br><br>",
    "%{yaxis.title.text}: %{y:.00}<br>",
    "%{xaxis.title.text}: %{x:.00}<br>",
    "Win-Shares: %{marker.size:.00}",
    "<extra></extra>"
  )) %>%
  layout(legend = list(orientation = 'h', y = -0.3))

plot_ly(
  type = 'scatter',
  mode = 'markers',
  x = ~DBPM,
  y = ~PER,
  marker = list(size = ~WS, sizemode = 'area'),
  color = ~Pos,
  text = ~paste(Player," - ",Year),
  hovertemplate = paste(
    "<b>%{text}</b><br><br>",
    "%{yaxis.title.text}: %{y:.00}<br>",
    "%{xaxis.title.text}: %{x:.00}<br>",
    "Win-Shares: %{marker.size:.00}",
    "<extra></extra>"
  )
) %>%
  layout(legend = list(orientation = 'h', y = -0.3))



# convert matrix to dataframe
state_stat <- data.frame(state.name = rownames(state.x77), state.x77)
# remove row names
rownames(state_stat) <- NULL
# create variable with colnames as choice
choice <- colnames(state_stat)[-1]


photo_me = "./DGA_linkedin.jpg"

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
library(data.table)

library(devtools)
library(shinydashboard)
library(dashboardthemes)
#install_github("nik01010/dashboardthemes")  #!! get rid of '#' in beginning of line to download theme


####get geo data (coming soon for future analysis)
###for countries and continents
#json_file <- 'https://datahub.io/JohnSnowLabs/country-and-continent-codes-list/datapackage.json'
#json_data <- fromJSON(paste(readLines(json_file), collapse=""))
##list of files
#print(json_data$resources$name)

# print all tabular data(if exists any)
#for(i in 1:length(json_data$resources$datahub$type)){
#  if(json_data$resources$datahub$type[i]=='derived/csv'){
#    path_to_file = json_data$resources$path[i]
#    countries_continents = read.csv(url(path_to_file))
#  }
#}

#NBA players historical data from Kaggle - primary source https://www.basketball-reference.com
players_df = read.csv("Players.csv", stringsAsFactors = F)
bball_info = read.csv("player_data.csv", stringsAsFactors = F)
season_stats = read.csv("Seasons_Stats.csv", stringsAsFactors = F)


###############place functions in the new helpers.R file
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

#get rid of blank rows in seasons data set
season_stats = season_stats %>% 
  filter(str_length(Pos)>0)


####enhancements
###column enhancements to players_df
players_df = players_df %>% 
  rename(height_cm = height) %>% 
  rename(weight_kg = weight) %>% 
  mutate("weight_lbs" = kg_2_lbs(weight_kg)) %>% 
  mutate("height_inches" = cm_2_inches(height_cm))



### position classifiers
season_stats = season_stats %>% 
  mutate(Pos_modern = 
           case_when(
             substr(Pos, 1, 2) == "PG" ~ "PG",
             substr(Pos, 1, 3) == "F-C" ~ "Big",
             substr(Pos, 1, 2) == "PF" ~ "Big",
             substr(Pos, 1, 2) == "C" ~ "Big",
             substr(Pos, 1, 2) == "C-" ~ "Big",
             TRUE ~ "Wing"
           ),
         PPG = PTS/G,
         MPG = MP/G,
         FGPG = FG/G,
         FGAPG = FGA/G,
         x3PPG = X3P/G,
         x3PAPG = X3PA/G,
         x2PPG = X2P/G,
         x2PAPG = X2PA/G,
         FTPG = FT/G,
         FTAPG = FTA/G,
         ORPG = ORB/G,
         DRPG = DRB/G,
         RPG = TRB/G,
         APG = AST/G,
         SPG = STL/G,
         BPG = BLK/G,
         TOPG = TOV/G,
         PFPG = PF/G
  ) 

#Have to account for players that switched teams during a given season.
#Rows with column value "TOT" in team column represents total stats, _
  # inclusive of each team individual played for.
#To keep rows for "TOT" for players who moved teams mid-season, I use inner join below, _
  # which identifies max games played by individual during any given year in data set.
  # we take 
maxG_forJoin = season_stats %>% 
  group_by(Year,Player) %>% 
  summarise(G = max(G))

season_stats = season_stats %>% 
  inner_join(maxG_forJoin, by = c("G"="G", "Player" = "Player","Year" = "Year"))


###USA states data ---> for geo analysis (coming soon)
states_usa = read.csv("states_usa.csv")


####data subsets
#seasons where players had at least 25 PER, 25 MPG, on or after year 1978
stats_25plusPER = season_stats %>%
  filter(PER >= 25,
         MPG >= 25,
         Year >= 1978) %>% 
  inner_join(players_df, by = "Player")

#all stats from 1978 (first year Turn-overs were tracked) to 2017 plus additional player data such as height
stats_all_1978to2017 = season_stats %>%
  filter(Year >= 1978) %>% 
  inner_join(players_df, by = "Player")

#summarizes total distibution of shots by year
seasons_distro_shots = season_stats %>% 
  group_by(Year) %>% 
  summarise(tot_3PM = sum(X3P),
            tot_3PA = sum(X3PA),
            tot_2PM = sum(X2P),
            tot_2PA = sum(X2PA),
            tot_FGA = sum(FGA),
            tot_FG = sum(FG),
            avg_3PAR = tot_3PA/tot_FGA,
            avg_2PAR = tot_2PA/tot_FGA,
            avg_3Prcnt = mean(X3P.))

#top 10 PER leaders from each year since 1978, plus height data. 
#Made sure to filter for players that played at least 1000 minutes. 
top10PER_PerSeason = season_stats %>% 
  arrange(desc(PER)) %>% 
  group_by(Year) %>%
  filter(MP > 1000) %>% 
  slice(1:10) %>% 
  inner_join(players_df, by = "Player")

#top 10 PER leaders grouped by position each year since 1978, plust height data.
#same filters as above apply.
top10PER_PerSeason_byPos = season_stats %>% 
  arrange(desc(PER)) %>% 
  group_by(Year,Pos_modern) %>%
  filter(MP > 1000) %>% 
  slice(1:10) %>% 
  inner_join(players_df, by = "Player")


#photo of me :)
photo_me = "./DannyMedium.jpeg"

#intro html text
intro_text = paste('<h2>The game is expanding, thanks to the evolution of smaller players and the 3 point shot</h2>',
                   '<br><h3>Since the introduction of the 3 point line in 1980, basketball has become more perimeter oriented. My R Shiny application contains graphics that illustrate the evolution of the 3 point shot and the increased efficiency of smaller perimeter oriented players over time.</h3>',
                   '<br><h3>The source of this dataset can be found in this <a href="https://www.kaggle.com/drgilermo/nba-players-stats/">kaggle link</a>.</h3>',
                   '<br><h3>If you want to look at my code, check out my GitHub link below.</h3>',
                   '<br><h3>Project Github Link: <a href="https://github.com/davila2345/NBA_R_Shiny_Project</h3>">https://github.com/davila2345/NBA_R_Shiny_Project</h3></a>')

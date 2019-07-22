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


#data subsets

stats_25plusPER = season_stats %>%
  filter(PER >= 25,
         MPG >= 25,
         Year >= 1978) %>% 
  inner_join(players_df, by = "Player")

stats_all_1978to2017 = season_stats %>%
  filter(Year >= 1978) %>% 
  inner_join(players_df, by = "Player")

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

top10PER_PerSeason = season_stats %>% 
  arrange(desc(PER)) %>% 
  group_by(Year) %>%
  filter(MP > 1000) %>% 
  slice(1:10) %>% 
  inner_join(players_df, by = "Player")


top10PER_PerSeason_byPos = season_stats %>% 
  arrange(desc(PER)) %>% 
  group_by(Year,Pos_modern) %>%
  filter(MP > 1000) %>% 
  slice(1:10) %>% 
  inner_join(players_df, by = "Player")



####Graphs

ggplotly(
  stats_all_1978to2017 %>% 
    filter(G >= 30,
           MPG >= 24,
           PER >= 20) %>%
    ggplot(aes(x = round(height_inches,0), y = PER, color = .$Pos_modern)) +
      geom_point() + theme_tufte() + 
      labs(title = "Players with 20+ PER by Height", x = 'Height (inches)') +
      theme(legend.title = element_blank(), legend.position = "bottom")
)

ggplotly(
  p = (stats_all_1978to2017 %>% 
      filter(G >= 30,
             MPG >= 24,
             PER >= 20) %>%
      ggplot(aes(x = round(height_inches,0), fill = .$Pos_modern)) +
      geom_bar() + theme_tufte() + labs(title = "Seasons with 20+ PER by Player Height"))#,
  #tooltip = paste(~Player," - ",~Year,"<br>","TEST")
)

ggplotly(
  top10PER_PerSeason %>%
    ggplot(aes(x = Year, fill = Pos_modern, order = Pos_modern)) + geom_bar() + theme_tufte() + 
    labs(title = "Distribution of Top 10 PER each season by Position") +
    theme(legend.title = element_blank())
)

ggplotly(
  seasons_distro_shots %>% 
  ggplot(aes(x = Year)) + 
    geom_line(aes(y = avg_3PAR, color = "TOT 3PA")) +
    geom_line(aes(y = avg_2PAR, color = "TOT 2PA")) +
    theme_tufte() + xlab("") + ylab("") +
    theme(legend.title = element_blank()) +
    labs(title = "The rate of 3 point attempts has been rising since 1980")
    
    
)

# convert matrix to dataframe
state_stat <- data.frame(state.name = rownames(state.x77), state.x77)
# remove row names
rownames(state_stat) <- NULL
# create variable with colnames as choice
choice <- colnames(state_stat)[-1]


photo_me = "./DannyMedium.jpeg"

intro_text = paste("<h1>The game is expanding, thanks to the evolution of smaller players and the 3 point shot</h1>",
                   "<br>Since the introduction of the 3 point line in 1980, basketball has become more perimeter oriented. My R Shiny application contains graphics that illustrate the evolution of the 3 point shot and the increased efficiency of smaller perimeter oriented players over time.",
                   "<br>I explored the dataset provided in this kaggle link(https://www.kaggle.com/drgilermo/nba-players-stats/), which shows individual player regular season statistics since 1950.",
                   "<br>See links below. GitHub link contains all R code files, which illustrates use of ggplot2, dplyr, plotly, and shiny libraries among others. R Shiny link is also provided below.",
                   "<br>R Shiny App Link: https://davila2345.shinyapps.io/shiny_nba/ Project Github Link: https://github.com/davila2345/NBA_R_Shiny_Project")

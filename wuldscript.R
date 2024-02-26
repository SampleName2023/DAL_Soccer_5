
# statsbombetc ------------------------------------------------------------


# libraries ---------------------------------------------------------------
## install statbomb halløj
#install.packages("devtools")
#devtools::install_github("statsbomb/SDMTools")
#devtools::install_github("statsbomb/StatsBombR")

library(tidyverse)
library(devtools)
library(SDMTools)
library(StatsBombR)
library(ggsoccer)
library(jsonlite)

# chatgpt code for calculating possible shots within cone -----------------

is_inside_shooting_cone <- function(shooting_player, target_player, cone_angle, cone_distance) {
  angle <- atan2(target_player[2] - shooting_player[2], target_player[1] - shooting_player[1])
  distance <- sqrt((target_player[1] - shooting_player[1])^2 + (target_player[2] - shooting_player[2])^2)
  
  return (0 <= angle && angle <= cone_angle) && (distance <= cone_distance)
}

# Example usage:
shooting_player <- c(x_shooting, y_shooting)
players <- matrix(c(x1, y1, x2, y2, ...), ncol = 2, byrow = TRUE)  # Matrix of all players' coordinates

cone_angle <- pi / 4  # Example angle in radians
cone_distance <- 10  # Example distance

potential_targets <- players[apply(players, 1, function(player) is_inside_shooting_cone(shooting_player, player, cone_angle, cone_distance)), ]

print("Potential targets:")
print(potential_targets)



# statbomb first ----------------------------------------------------------
source("util.R")


FComp <- FreeCompetitions() %>%
  filter(competition_id %in% c(72) ) #
Matches <- FreeMatches(FComp) 

# Kvindernes VM 2023, competitionid=107
VMMatches=Matches %>% filter(season.season_id==107)

StatsBombData <- free_allevents(MatchesDF = Matches, Parallel = T)
StatsBombData = allclean(StatsBombData) #


# find en kamp
dkmatch <- StatsBombData %>% filter(match_id==3893795)
dkmshot <- dkmatch %>% filter(type.name=="Shot")

# angle and dist to goal
#tl = unlist(dkmshot[1,'location'])
dkmshotsel <- dkmshot[,c(74,77,25,26,21,22,10,150,151,156,157)]

dkmshotsel <- dkmshotsel %>% rowwise() %>% mutate(angle=mangle(unlist(location)))
dkmshotsel <- dkmshotsel %>% rowwise() %>% mutate(dist=disttogoal(unlist(location)))
dkpshotsel <- dkmshotsel %>% filter(team.id==853)
chpshotsel <- dkmshotsel %>% filter(team.id==1207)

# plot a shot
dftest=dkpshotsel %>% filter(player.name=="Josefine Hasbo")
ggplot(dftest) +
  annotate_pitch(
    dimensions = pitch_statsbomb,
    colour = "white",
    fill = "#3ab54a")+
  geom_segment(aes(x = location.x, 
                   y = location.y, 
                   xend = shot.end_location.x,
                   yend=shot.end_location.y),
               colour = "yellow",
               size = 1) +
  theme_pitch() +
  coord_flip(xlim = c(49, 121)) +
  scale_y_reverse() +
  geom_text(aes(x=location.x,y=location.y,label=player.name), size=2.5,vjust=1)+
  geom_point(aes(x=location.x,y=location.y,color=team.name), size=2.5)

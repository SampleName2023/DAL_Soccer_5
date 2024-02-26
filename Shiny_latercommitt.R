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
source("C:/Users/Simon/Documents/DAL2sem/utilsourc.R")
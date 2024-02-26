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

# freeze frame ------------------------------------------------------------

freezdftemp1 <- dkpshotsel[8,]

str(freezedftemp2$location)
tempvect <- data.frame(unlist(freezedftemp2$location))
tempvect2 <- data.frame(t(freezedftemp2$location))


testdf <- rbind(freezedftemp2,tempvect)


freezedftemp2 <- freezdftemp1$shot.freeze_frame[[1]]
#use purr (og dplyr) til str extract af listen med coords

tempdf <- freezedftemp2 %>%
  mutate(
    xCorrd = map_dbl(location, 1),
    y_Coord = map_dbl(location, 2)
  ) %>%
  select(-location) #ikke sikker på hvorfor -

# shiny -------------------------------------------------------------------




ui <- fluidPage(
  titlePanel("Football Shots Visualization"),
  mainPanel(
    plotOutput("footballPlot")
  )
)
server <- function(input, output) {
  output$footballPlot <- renderPlot({
    ggplot(dftest) +
      annotate_pitch(
        dimensions = pitch_statsbomb,
        colour = "white",
        fill = "#3ab54a") +
      geom_segment(
        aes(
          x = location.x,
          y = location.y,
          xend = shot.end_location.x,
          yend = shot.end_location.y
        ),
        colour = "yellow",
        size = 1
      ) +
      theme_pitch() +
      coord_flip(xlim = c(49, 121)) +
      scale_y_reverse() +
      geom_text(
        aes(x = location.x, y = location.y, label = player.name),
        size = 2.5,
        vjust = 1
      ) +
      geom_point(
        aes(x = location.x, y = location.y, color = team.name),
        size = 2.5
      )
  })
}

shinyApp(ui, server)

###################################################################################################
#title: Shot Images
#description: Using the data aggregated in make-shots-data-script to create scatterplots of player data.
#input(s): shots-data.csv
#output(s): PDFs and one png with graphs of player data
###################################################################################################
library(ggplot2)
library(grid)
library(dplyr)
library(jpeg)

data_types = c("team_name"="character", "game_date"="character", "season" = "integer", "period"="integer",
               "minutes_remaining"="integer", "seconds_remaining"="integer", "shot_made_flag"="factor",
               "action_type"="factor", "shot_type"="factor", "shot_distance"="integer", "opponent"="character",
               "x"="integer", "y"="integer")
players <- read.csv('./data/shots-data.csv', stringsAsFactors = FALSE, colClasses=data_types)

# court image (to be used as background of plot)
court_file <- "./images/nba-court.jpg"
# create raste object
court_image <- rasterGrob(readJPEG(court_file), width = unit(1, "npc"), height = unit(1, "npc"))

andre <-players[players$name=="Andre Iguodala",]
andrea_shot_chart <- ggplot(data = andre) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Andre Iguodala (2016 season)') +
  theme_minimal()


draymond <-players[players$name=="Draymond Green",]
draymond_shot_chart <- ggplot(data = draymond) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Draymond Green (2016 season)') +
  theme_minimal()


kevin <-players[players$name=="Kevin Durant",]
kevin_shot_chart <- ggplot(data = kevin) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Kevin Durant (2016 season)') +
  theme_minimal()


klay <-players[players$name=="Klay Thompson",]
klay_shot_chart <- ggplot(data = klay) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Klay Thompson (2016 season)') +
  theme_minimal()


stephen <-players[players$name=="Stephen Curry",]
stephen_shot_chart <- ggplot(data = stephen) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Stephen Curry (2016 season)') +
  theme_minimal()


pdf('./images/andre-iguodala-shot-chart.pdf', width=6.5,height=5)
andrea_shot_chart
dev.off()


pdf('./images/draymond-green-shot-chart.pdf', width=6.5,height=5)
draymond_shot_chart
dev.off()


pdf('./images/kevin-durant-shot-chart.pdf', width=6.5,height=5)
kevin_shot_chart
dev.off()

pdf('./images/klay-thompson-shot-chart.pdf', width=6.5,height=5)
klay_shot_chart
dev.off()

pdf('./images/stephen-curry-shot-chart.pdf', width=6.5,height=5)
stephen_shot_chart
dev.off()


facetted_shot_chart <- ggplot(data = players) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Charts: GSW (2016 season)') +
  theme_minimal()+
  facet_grid(~ name)

pdf('./images/gsw-shot-charts.pdf', width=8,height=7)
facetted_shot_chart
dev.off()

png('./images/gsw-shot-charts.png', width=8,height=7, units="in", res = 72)
facetted_shot_chart
dev.off()

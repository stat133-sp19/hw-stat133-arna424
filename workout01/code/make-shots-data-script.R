###################################################################################################
#title: Agreggate player data
#description: Aggregating player data and adding specific columns (like name)
#input(s): stephen-curry.csv, klay-thompson.csv, kevin-durant.csv, draymond-green.csv, andre-iguodala.csv
#output(s): Summaries of player data and an aggregated csv file of player data.
###################################################################################################

data_types = c("team_name"="character", "game_date"="character", "season" = "integer", "period"="integer",
               "minutes_remaining"="integer", "seconds_remaining"="integer", "shot_made_flag"="factor",
               "action_type"="factor", "shot_type"="factor", "shot_distance"="integer", "opponent"="character",
               "x"="integer", "y"="integer")
curry <- read.csv("./data/stephen-curry.csv", stringsAsFactors = FALSE, colClasses=data_types)
thompson <- read.csv("./data/klay-thompson.csv", stringsAsFactors = FALSE, colClasses=data_types)
durant <- read.csv("./data/kevin-durant.csv", stringsAsFactors = FALSE, colClasses=data_types)
green <- read.csv("./data/draymond-green.csv", stringsAsFactors = FALSE, colClasses=data_types)
iguodala <- read.csv("./data/andre-iguodala.csv", stringsAsFactors = FALSE, colClasses=data_types)

curry$name <- "Stephen Curry"
thompson$name <- "Klay Thompson"
durant$name <- "Kevin Durant"
green$name <- "Draymond Green"
iguodala$name <- "Andre Iguodala"


curry$shot[curry$shot_made_flag=="n"]<-"shot no"
curry$shot[curry$shot_made_flag=="y"]<-"shot yes"

thompson$shot[thompson$shot_made_flag=="n"]<-"shot no"
thompson$shot[thompson$shot_made_flag=="y"]<-"shot yes"

durant$shot[durant$shot_made_flag=="n"]<-"shot no"
durant$shot[durant$shot_made_flag=="y"]<-"shot yes"

green$shot[green$shot_made_flag=="n"]<-"shot no"
green$shot[green$shot_made_flag=="y"]<-"shot yes"

iguodala$shot[iguodala$shot_made_flag=="n"]<-"shot no"
iguodala$shot[iguodala$shot_made_flag=="y"]<-"shot yes"


curry$minute <- 12*curry$period-curry$minutes_remaining
thompson$minute <- 12*thompson$period-thompson$minutes_remaining
durant$minute <- 12*durant$period-durant$minutes_remaining
green$minute <- 12*green$period-green$minutes_remaining
iguodala$minute <- 12*iguodala$period-iguodala$minutes_remaining


sink(file = './output/stephen-curry-summary.txt')
summary(curry)
# closing sinking operation
sink()


sink(file = './output/klay-thompson-summary.txt')
summary(thompson)
# closing sinking operation
sink()


sink(file = './output/kevin-durant-summary.txt')
summary(durant)
# closing sinking operation
sink()


sink(file = './output/draymond-green-summary.txt')
summary(green)
# closing sinking operation
sink()


sink(file = './output/andre-iguodala-summary.txt')
summary(iguodala)
# closing sinking operation
sink()

binded <- rbind(curry,thompson,durant,green,iguodala)
write.csv(binded, file="./data/shots-data.csv")

sink(file = './output/shot-data-summary.txt')
summary(binded)
# closing sinking operation
sink()
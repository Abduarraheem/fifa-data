library("caret")
library("glmnet")
library("tidyverse")
library("dplyr")
library("ISLR2")
library("ggplot2")
library("ggrepel")
require(data.table)

team_plots_dir <- "team_plots/"
dir.create(team_plots_dir)
unlink(paste0(team_plots_dir, "*"))

teams <- read.csv("./male_teams.csv")
players <- read.csv("./male_players (legacy).csv")
coaches <- read.csv("./male_coaches.csv")

modifiedTeams <- subset(teams, team_name == unique(teams$team_name)[1:100])

months <- c("-02-", "-06-", "-09-", "-12-")

#lastTeams <- data.frame(matrix(ncol = 54, nrow = 0))
#colnames(lastTeams) <- names(modifiedTeams)
#for (month in months){
#    rbind(lastTeams, filter(modifiedTeams, fifa_update_date %like% month))
#}

lastTeams <- modifiedTeams[grepl(paste(months, collapse = "|"), modifiedTeams$fifa_update_date), ]

for (team in modifiedTeams$team_name){
    club_df <- subset(modifiedTeams, team_name == team)
    plot <- ggplot(club_df, aes(club_df$fifa_update_date, club_df$overall)) +
        geom_point() +
        geom_text_repel(aes(label = club_df$team_name))
    ggsave(paste0(team_plots_dir, team, ".png"), plot)
}

#teamPerformance <- data.frame(
#    team_id = teams[, 1],
#    team_name = teams[, c(6)],
#    year = teams[, c(4)],
#    overall = teams[, c(12)],
#   attack = teams[, c(13)],
#    midfield = teams[, c(14)],
#   defense = teams[, c(15)],
#    coach = teams[, c(16)]
#)

#####################################################################

modifiedPlayers <- subset(players, player_positions == c("GK", "CB", "LB", "RB"))

#####################################################################

teams2 <- teams
colnames(teams2)[10] <- "nat_id"
players2 %>% inner_join(teams, on = "club_team_id == team_id")
foreignPlayers

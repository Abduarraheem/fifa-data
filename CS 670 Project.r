library("caret")
library("glmnet")
library("tidyverse")
library("dplyr")
library("ISLR2")
library("ggplot2")
library("ggrepel")
require(data.table)

team_plots_dir <- "team_plots/"
unlink(paste0(team_plots_dir, "*"))

teams <- read.csv("./male_teams.csv")
players <- read.csv("./male_players (legacy).csv")
coaches <- read.csv("./male_coaches.csv")

coachID <- coaches$coach_id

team_names <- teams$team_name

modifiedTeams <- subset(teams, team_name == unique(teams$team_name)[1:100])

months <- c("-02-", "-06-", "-09-", "-12-")

lastTeams <- data.frame(matrix(ncol = 54, nrow = 0))
colnames(lastTeams) <- names(modifiedTeams)
for (month in months){
    rbind(lastTeams, filter(modifiedTeams, fifa_update_date %like% month))
}

new_df <- modifiedTeams[grepl(paste(months, collapse = "|"), modifiedTeams$fifa_update_date), ]

plot(new_df$overall, new_df$fifa_update_date, labels=new_df$team_name)

for (team in new_df$team_name){
    club_df <- subset(new_df, team_name == team)
    club_df
    plot <- ggplot(club_df, aes(club_df$fifa_update_date, club_df$overall)) +
        geom_point() +
        geom_text_repel(aes(label = club_df$team_name))
    ggsave(paste0(team_plots_dir, team, "_plot.png"), plot)
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


model <- lm(overall ~ coach, data=teamPerformance)

plot(teamPerformance$coach, teamPerformance$overall)
abline(model)
unique(teamPerformance$team_name)
amesData %>% group_by(Foundation) %>% summarise(count = n())
names(amesData)

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

modifiedTeams <- subset(teams, team_name %in% unique(teams$team_name)[1:100])

months <- c("-02-")

lastTeams <- modifiedTeams[grepl(paste(months, collapse = "|"), modifiedTeams$fifa_update_date), ]

coachNames <- data.frame(matrix(ncol = 1, nrow = 0))
colnames(coachNames) <- c("name")

for (team in unique(lastTeams$team_name)){
    team_df <- subset(lastTeams, team_name == team) # filter out countries.

    for (i in seq_len(nrow(team_df))) { # Now makes a list of all names first, then uses it in plot
        coachNames[nrow(coachNames) + 1, ] <- 
        c(coaches[which(team_df[i, ]$coach_id == coaches$coach_id), ]$short_name)
    }

    plot <- ggplot(team_df, aes(team_df$fifa_update_date, team_df$overall)) +
        ggtitle("Coach and Team Performance Overall") +
        xlab("Date") +
        ylab("Overall Rating") +
        geom_point() +
        geom_text_repel(aes(label = paste(team_df$team_name, "/", coachNames$name)))
    suppressMessages(ggsave(paste0(team_plots_dir, team, ".png"), width=20, height=4, plot))

    coachNames <- data.frame(matrix(ncol = 1, nrow = 0)) # Resets the coaches dataframe
    colnames(coachNames) <- c("name")
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

modifiedPlayers <- subset(players, club_position %in%
c("CB", "LB", "RB", "RWB", "LWB", "SW"))

lastPlayers <- data.frame(matrix(ncol = 5, nrow = 0))

colnames(lastPlayers) <- c(
    "defender",
    "goalkeeper",
    "clubName",
    "gkOverall",
    "gkOverallLater"
)

for (row in seq_len(nrow(modifiedPlayers))) {
    date <- as.Date(modifiedPlayers[row, "club_joined_date"])
    formattedFifaUpdate <- as.Date(modifiedPlayers$fifa_update_date)
    index <- which.min(abs(formattedFifaUpdate - date))

    filterData <- modifiedPlayers[which(
        modifiedPlayers$fifa_update_date == modifiedPlayers$fifa_update_date[index] &
        modifiedPlayers$club_team_id == modifiedPlayers[row, "club_team_id"] &
        modifiedPlayers$club_position == "GK"), ]
    
    if (nrow(filterData) == 0) {
        next
    }

    lastPlayers[nrow(lastPlayers) + 1, ] <- c(modifiedPlayers$short_name,
    filterData$short_name, filterData$club_name, filterData$overall, NULL)
}

lastPlayers

nrow(lastPlayers)


# find when players join new club
# grab team name
# grab GK
# grab overall GK
# after year or time grab overall again

#####################################################################

teams2 <- teams
players2 <- players
colnames(teams2)[10] <- "nat_id"
names(teams2)
players2 %>% inner_join(teams2, by=c("club_team_id" = "team_id"))
foreignPlayers <- subset(players2, nationality_id != nat_id)

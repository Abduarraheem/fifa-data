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
    team_df <- subset(lastTeams, team_name == team) # need to filter out countries.

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

#####################################################################

unlink(paste0(team_plots_dir, "*"))

coachNames <- data.frame(matrix(ncol = 1, nrow = 0))
colnames(coachNames) <- c("name")

for (team in unique(teams$team_name)){
    team_df <- subset(teams, team_name == team) # need to filter out countries.

    if (length(unique(team_df$coach_id)) <= 1) {
        next
    }

    finalTeamDF <- data.frame(matrix(ncol = 54, nrow = 0))
    colnames(finalTeamDF) <- names(team_df)

    for (i in seq_len(nrow(team_df))) { # Now makes a list of all names first, then uses it in plot
        sname <- c(coaches[which(team_df[i, ]$coach_id == coaches$coach_id), ]$short_name)
        
        if (nrow(coachNames) == 0) {
            finalTeamDF <- rbind(finalTeamDF, team_df[i, ])

            coachNames[nrow(coachNames) + 1, ] <- 
                c(coaches[which(team_df[i, ]$coach_id == coaches$coach_id), ]$short_name)
        }
        else if (i == nrow(team_df)) {
            finalTeamDF <- rbind(finalTeamDF, team_df[i, ])

            coachNames[nrow(coachNames) + 1, ] <- 
                c(coaches[which(team_df[i, ]$coach_id == coaches$coach_id), ]$short_name)
        }
        else if (tail(coachNames, 1)[1, 1] == sname & nrow(coachNames) > 0) {
            #print(sname)
            next
        }
        else {
            finalTeamDF <- rbind(finalTeamDF, team_df[i - 1, ])
            coachNames[nrow(coachNames) + 1, ] <- 
                c(coaches[which(team_df[i - 1, ]$coach_id == coaches$coach_id), ]$short_name)

            finalTeamDF <- rbind(finalTeamDF, team_df[i, ])
            coachNames[nrow(coachNames) + 1, ] <- 
                c(coaches[which(team_df[i, ]$coach_id == coaches$coach_id), ]$short_name)
        }
    }
    print(nrow(team_df))
    print(nrow(finalTeamDF))
    plot <- ggplot(finalTeamDF, aes(finalTeamDF$fifa_update_date, finalTeamDF$overall)) +
        ggtitle("Coach and Team Performance Overall") +
        xlab("Date") +
        ylab("Overall Rating") +
        geom_point() +
        geom_text_repel(aes(label = paste(finalTeamDF$team_name, "/", coachNames$name)))
    suppressMessages(ggsave(paste0(team_plots_dir, team, ".png"), width=20, height=4, plot))

    coachNames <- data.frame(matrix(ncol = 1, nrow = 0)) # Resets the coaches dataframe
    colnames(coachNames) <- c("name")
}

#####################################################################

defGK_plots_dir <- "defGK_plots/"
dir.create(defGK_plots_dir)
unlink(paste0(defGK_plots_dir, "*"))

modifiedPlayers <- subset(players, club_position %in%
c("CB", "LB", "RB", "RWB", "LWB", "SW") )

modifiedPlayers <- subset(modifiedPlayers, short_name %in%
unique(modifiedPlayers$short_name)[1:100])

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

    filterData <- subset(players, 
        as.Date(fifa_update_date) >= as.Date(modifiedPlayers$fifa_update_date[index]) &
        club_team_id == modifiedPlayers[row, "club_team_id"] &
        club_position == "GK")

    if (nrow(filterData) != 0) {
        plot <- ggplot(filterData, aes(filterData$fifa_update_date, filterData$overall)) +
        ggtitle("GK Overall as Cause of Defender") +
        xlab("Date") +
        ylab("Overall Rating") +
        geom_point() +
        geom_text_repel(aes(label = paste(filterData$short_name, "/", modifiedPlayers[row,]$short_name)))
        suppressMessages(ggsave(paste0(defGK_plots_dir, modifiedPlayers[row,]$short_name, ".png"), width=20, height=4, plot))    

        lastPlayers[nrow(lastPlayers) + 1, ] <- c(modifiedPlayers$short_name[row],
        filterData$short_name[1], filterData$club_name[1], filterData$overall[1], filterData$overall[1])
    }

}

#####################################################################

teams2 <- teams
players2 <- players
colnames(teams2)[10] <- "nat_id"
names(teams2)
players2 %>% inner_join(teams2, by=c("club_team_id" = "team_id"))
foreignPlayers <- subset(players2, nationality_id != nat_id)

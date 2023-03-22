library("caret")
library("glmnet")
library("tidyverse")
library("dplyr")
library("ISLR2")
library("ggplot2")
library("ggrepel")
library("randomForest")
library("ROCR")
library("vtable")
library("kableExtra")
library("corrplot")
require(data.table)

team_plots_dir <- "team_plots/"
dir.create(team_plots_dir)
unlink(paste0(team_plots_dir, "*"))

teams <- read.csv("./male_teams.csv")
players <- read.csv("./male_players (legacy).csv")
coaches <- read.csv("./male_coaches.csv")

# Prints out plots for teams only looking at February updates

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

# Prints out plots for teams with more than one coach in dataset

unlink(paste0(team_plots_dir, "*"))

coachNames <- data.frame(matrix(ncol = 1, nrow = 0))
colnames(coachNames) <- c("name")

for (team in unique(teams$team_id)){
    team_df <- subset(teams, team_id == team) # need to filter out countries.

    if (length(unique(team_df$coach_id)) <= 1) {
        next
    }

    finalTeamDF <- data.frame(matrix(ncol = 54, nrow = 0))
    colnames(finalTeamDF) <- names(team_df)

    for (i in seq_len(nrow(team_df))) { # Now makes a list of all names first, then uses it in plot
        sname <- c(coaches[which(team_df[i, ]$coach_id == coaches$coach_id), ]$short_name)
        
        if (identical(sname, character(0))) {
            next
        }
        else if (nrow(coachNames) == 0) {
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

    plot <- ggplot(finalTeamDF, aes(finalTeamDF$fifa_update_date, finalTeamDF$overall)) +
        ggtitle("Coach and Team Performance Overall") +
        xlab("Date") +
        ylab("Overall Rating") +
        geom_point() +
        geom_text_repel(aes(label = paste(finalTeamDF$team_name, "/", coachNames$name)))
    suppressMessages(ggsave(paste0(team_plots_dir, finalTeamDF$team_name[1], ".png"), width=10, height=4, plot))

    coachNames <- data.frame(matrix(ncol = 1, nrow = 0)) # Resets the coaches dataframe
    colnames(coachNames) <- c("name")
}

#====================================================================

# model <- glm(overall ~ coach_id + fifa_update_date + (coach_id * fifa_update_date), data = teams, family = gaussian())
# results <- predict(model, type="response")
# results

newTeams <- data.frame(matrix(ncol = 54, nrow = 0))
colnames(newTeams) <- names(teams)
for (team in unique(teams$team_id)){
    team_df <- subset(teams, team_id == team)

    if (length(unique(team_df$coach_id)) > 1) {
        newTeams <- rbind(newTeams, team_df)
    }
}

# Replaces NA values in coach_id column with median of coach_id
val <- median(na.omit(newTeams$coach_id))
newTeams$coach_id[is.na(newTeams$coach_id)] <- val

# WARNING: will remove coach_id column as well if you dont run above line (has some NA values in it)
newTeams <- newTeams[, colSums(is.na(newTeams)) == 0] # Remove all columns with any NA values in them

set.seed(1)
split <- 0.7
sample <- sample(nrow(newTeams), split * nrow(newTeams))
train_data <- newTeams[sample, ]
test_data <- newTeams[-sample, ]
rfTeams <- randomForest(as.factor(overall > mean(overall)) ~ .,
    data = train_data, ntree = 500, mtry = 7,
    importance = TRUE, na.action = na.exclude)


predictions <- predict(rfTeams, test_data)
cm <- confusionMatrix(predictions, as.factor(test_data$overall > mean(test_data$overall)))

names(cm$byClass)

varImpPlot(rfTeams)


pred2 <- predict(rfTeams, test_data, type = "prob")
pred3 <- prediction(pred2[, 2], as.factor(test_data$overall > mean(test_data$overall)))
auc <- performance(pred3, "auc")@y.values[[1]]
cat("AUC: ", auc)

#********************************************************************

# Testing midfield strength
glmMidfield <- glm(as.factor(overall > mean(overall)) ~ midfield,
    data = train_data, family = binomial())

midfieldPred <- predict(glmMidfield, test_data)
midfieldPred <- ifelse(midfieldPred >= 0, TRUE, FALSE)

confusionMatrix(as.factor(midfieldPred), as.factor(test_data$overall > mean(test_data$overall))) # Acc: 0.923

#********************************************************************

# Testing attack strength
glmAttack <- glm(as.factor(overall > mean(overall)) ~ attack,
    data = train_data, family = binomial())

attackPred <- predict(glmAttack, test_data)
attackPred <- ifelse(attackPred >= 0, TRUE, FALSE)

confusionMatrix(as.factor(attackPred), as.factor(test_data$overall > mean(test_data$overall))) # Acc: 0.893

#********************************************************************

# Testing defence strength
glmDefence <- glm(as.factor(overall > mean(overall)) ~ defence,
    data = train_data, family = binomial()
)

defencePred <- predict(glmDefence, test_data)
defencePred <- ifelse(defencePred >= 0, TRUE, FALSE)

confusionMatrix(as.factor(defencePred), as.factor(test_data$overall > mean(test_data$overall))) # Acc: 0.9243

#********************************************************************

# Testing coach_id
glmCoach <- glm(as.factor(overall > mean(overall)) ~ coach_id + fifa_update_date,
    data = train_data, family = binomial()
)

coachPred <- predict(glmCoach, test_data)
coachPred <- ifelse(coachPred >= 0, TRUE, FALSE)

confusionMatrix(as.factor(coachPred), as.factor(test_data$overall > mean(test_data$overall))) # Acc: 0.6071 with fifa_update, 0.6348 without fifa_update

#####################################################################

defGK_plots_dir <- "defGK_plots/"
dir.create(defGK_plots_dir)
unlink(paste0(defGK_plots_dir, "*"))

modifiedPlayers <- subset(players, club_position %in%
c("CB", "LB", "RB", "RWB", "LWB", "SW"))

modifiedPlayers <- subset(modifiedPlayers, short_name %in%
unique(modifiedPlayers$short_name)[1:3000])

modifiedPlayers['gkName'] <- NA
modifiedPlayers["gkOverall"] <- NA

# lastPlayers <- data.frame(matrix(ncol = 5, nrow = 0))

# colnames(lastPlayers) <- c(
#     "defender",
#     "goalkeeper",
#     "clubName",
#     "gkOverall",
#     "gkOverallLater"
# )

for (row in seq_len(nrow(modifiedPlayers))) {
    date <- as.Date(modifiedPlayers[row, "club_joined_date"])
    formattedFifaUpdate <- as.Date(modifiedPlayers$fifa_update_date)
    index <- which.min(abs(formattedFifaUpdate - date))

    filterData <- subset(players,
        as.Date(fifa_update_date) >= as.Date(modifiedPlayers$fifa_update_date[index]) &
        club_team_id == modifiedPlayers[row, "club_team_id"] &
        club_position == "GK")

    if (nrow(filterData) != 0) {
        modifiedPlayers[row, ]$gkName <- filterData$short_name[1]
        modifiedPlayers[row, ]$gkOverall <- filterData$overall[1]

        # plot <- ggplot(filterData, aes(filterData$fifa_update_date, filterData$overall)) +
        # ggtitle("GK Overall as Cause of Defender") +
        # xlab("Date") +
        # ylab("Overall Rating") +
        # geom_point() +
        # geom_text_repel(aes(label = paste(filterData$short_name, "/", modifiedPlayers[row,]$short_name)))
        # suppressMessages(ggsave(paste0(defGK_plots_dir, modifiedPlayers[row,]$short_name, ".png"), width=10, height=4, plot))    

        # lastPlayers[nrow(lastPlayers) + 1, ] <- c(modifiedPlayers$short_name[row],
        # filterData$short_name[1], filterData$club_name[1], filterData$overall[1], filterData$overall[1])
    }
}

nrow(modifiedPlayers)

# ====================================================================

# model <- glm(overall ~ coach_id + fifa_update_date + (coach_id * fifa_update_date), data = teams, family = gaussian())
# results <- predict(model, type="response")
# results

# Replaces NA values in gkOverall column with median of gkOverall
val <- median(na.omit(modifiedPlayers$gkOverall))
modifiedPlayers$gkOverall[is.na(modifiedPlayers$gkOverall)] <- val

modifiedPlayers$gkName[is.na(modifiedPlayers$gkName)] <- "None"

newPlayers <- modifiedPlayers[, colSums(is.na(modifiedPlayers)) == 0] # Remove all columns with any NA values in them

# Remove arithmetic signs from numeric columns
newPlayers[] <- lapply(newPlayers, function(x) { 
    gsub("[-|\\+].*", "", x)
})

set.seed(1)
split <- 0.7
sample <- sample(nrow(newPlayers), split * nrow(newPlayers))
trainDataPlayers <- newPlayers[sample, ]
testDataPlayers <- newPlayers[-sample, ]
rfPlayers <- randomForest(as.factor(as.numeric(gkOverall) > mean(as.numeric(gkOverall))) ~ .,
    data = trainDataPlayers, ntree = 500, mtry = 35,
    importance = TRUE)

predictions <- predict(rfPlayers, testDataPlayers)
confusionMatrix(predictions, as.factor(as.numeric(testDataPlayers$gkOverall)
> mean(as.numeric(testDataPlayers$gkOverall))))

print(varImpPlot(rfPlayers))

pred2 <- predict(rfPlayers, testDataPlayers, type = "prob")
pred3 <- prediction(pred2[, 2], as.factor(as.numeric(testDataPlayers$gkOverall) > mean(as.numeric(testDataPlayers$gkOverall))))
auc <- performance(pred3, "auc")@y.values[[1]]
cat("AUC: ", auc)

summary(rfPlayers)

#********************************************************************

newPlayers[] <- lapply(newPlayers, function(x) {
    gsub("[-|\\+].*", "", x)
})
trainDataPlayers[] <- lapply(trainDataPlayers, function(x) {gsub("[-|\\+].*", "", x)})
testDataPlayers[] <- lapply(testDataPlayers, function(x) {gsub("[-|\\+].*", "", x)})

testDataPlayers$gkOverall <- as.numeric(testDataPlayers$gkOverall)
testDataPlayers$rb <- as.numeric(testDataPlayers$rb)
testDataPlayers$lb <- as.numeric(testDataPlayers$lb)
testDataPlayers$cb <- as.numeric(testDataPlayers$cb)
testDataPlayers$rwb <- as.numeric(testDataPlayers$rwb)
testDataPlayers$lwb <- as.numeric(testDataPlayers$lwb)
str(newPlayers)

unique(as.numeric(trainDataPlayers$lb))

# Testing defence strength
glmStat <- glm(as.factor(as.numeric(gkOverall) > mean(as.numeric(gkOverall))) ~ as.numeric(rb) + as.numeric(cb) + as.numeric(rwb) + as.numeric(lwb) + as.numeric(lb), data = trainDataPlayers, family = binomial())
summary(glmStat)
names(trainDataPlayers)
cor(sapply(trainDataPlayers[,96:101], function(x) {as.numeric(x)}))
class(trainDataPlayers)

# str(as.factor(modifiedPlayers$cb))

# length(unique(as.factor(trainDataPlayers$cb)))

statPred <- predict(glmStat, testDataPlayers)
statPred <- ifelse(statPred >= 0, TRUE, FALSE)
confusionMatrix(as.factor(statPred), as.factor(as.numeric(testDataPlayers$gkOverall) > mean(as.numeric(testDataPlayers$gkOverall)))) # Acc: 0.9243

pred2 <- predict(glmStat, testDataPlayers)
pred3 <- prediction(pred2, as.factor(as.numeric(testDataPlayers$gkOverall) > mean(as.numeric(testDataPlayers$gkOverall))))
aucDefense1 <- performance(pred3, "auc")@y.values[[1]]
cat("AUC:", aucDefense1)

perf <- performance(pred3, "tpr", "fpr")
plot(perf,
    lwd = 2,
    main = "Defence Player Defence Stats ROC"
)
abline(a = 0, b = 1)

#====================================================================

glmOverall <- glm(as.factor(as.numeric(gkOverall) > mean(as.numeric(gkOverall))) ~ as.numeric(overall), data = trainDataPlayers, family = binomial())
summary(glmOverall)

overallPred <- predict(glmOverall, testDataPlayers)
overallPred <- ifelse(overallPred >= 0, TRUE, FALSE)
confusionMatrix(as.factor(overallPred), as.factor(as.numeric(testDataPlayers$gkOverall) > mean(as.numeric(testDataPlayers$gkOverall)))) # Acc: 0.9243

pred2 <- predict(glmOverall, testDataPlayers)
pred3 <- prediction(pred2, as.factor(as.numeric(testDataPlayers$gkOverall) > mean(as.numeric(testDataPlayers$gkOverall))))
aucDefense2 <- performance(pred3, "auc")@y.values[[1]]
cat("AUC:", aucDefense2)

perf <- performance(pred3, "tpr", "fpr")
plot(perf,
    lwd = 2,
    main = "Defence Player Overall ROC"
)
abline(a = 0, b = 1)

# ====================================================================

glmOverallDate <- glm(as.factor(as.numeric(gkOverall) > mean(as.numeric(gkOverall))) ~ as.numeric(overall) + club_joined_date, data = trainDataPlayers, family = binomial())
summary(glmOverallDate)

overallPredDate <- predict(glmOverallDate, testDataPlayers)
overallPredDate <- ifelse(overallPredDate >= 0, TRUE, FALSE)
cmOverallDate <- confusionMatrix(as.factor(overallPredDate), as.factor(as.numeric(testDataPlayers$gkOverall) > mean(as.numeric(testDataPlayers$gkOverall)))) # Acc: 0.9243
print(cmOverallDate)
class(overallPredDate)

pred2 <- predict(glmOverallDate, testDataPlayers)
pred3 <- prediction(pred2, as.factor(as.numeric(testDataPlayers$gkOverall) > mean(as.numeric(testDataPlayers$gkOverall))))
aucDefense2 <- performance(pred3, "auc")@y.values[[1]]
cat("AUC:", aucDefense2)

perf <- performance(pred3, "tpr", "fpr")
plot(perf,
    lwd = 2,
    main = "Defence Player Overall + Club Joined Date ROC"
)
abline(a = 0, b = 1)

#####################################################################

newPlayers$overall <- as.numeric(newPlayers$overall)

corGraph <- cor(sapply(trainDataPlayers[, c(92, 96:101)], function(x) {
    as.numeric(x)
}))

colnames(corGraph) <- names(trainDataPlayers[, c(92, 96:101)])
rownames(corGraph) <- names(trainDataPlayers[, c(92, 96:101)])
corrplot(corGraph)

st(newPlayers[, c("gkOverall", "overall")],
    labels = c("GK Overall", "DF Player Overall"),
    title = "Summary of Defence and Goalkeeper Player Statistics",
    col.align = "right",
    out = "return"
) %>%
    kbl(caption = "Summary of Defence and Goalkeeper Player Statistics") %>%
    kable_styling()


teams2 <- teams
players2 <- players
colnames(teams2)[10] <- "nat_id"
names(teams2)
players2 %>% inner_join(teams2, by=c("club_team_id" = "team_id"))
foreignPlayers <- subset(players2, nationality_id != nat_id)

require(ggplot2)
require(cowplot)
theme_set(theme_cowplot())
require(dplyr)
require(lme4)
library(lmerTest)
require(tidyverse)
library(openxlsx)

#change user name:
Task2_Astronauts = read.xlsx("VECTION Astronauts Long Format.xlsx", sheet = 2, startRow = 1, colNames = FALSE)
Task2_Astronauts$Astronauts = "Astronauts"

colnames(Task2_Astronauts) = c("id","session", "Posture","trial","Distance_Real", "Distance_Subject", "Astronauts")
unique(Task2_Astronauts$id)

Check_Task2 = Task2_Astronauts %>% 
  group_by(session,Posture,id,Distance_Real) %>% 
  mutate(length(id)) %>% 
  slice(1)

Check2_Task2 = Check_Task2 %>% 
  group_by(id) %>% 
  mutate(length(id)) %>% 
  slice(1)

f="woman"
m="man"


GenderFrame = data.frame(ID = c(2533, #astronauts
                                3513,
                                8927,
                                5706,
                                1646,
                                8171,
                                3569,
                                8994,
                                1001,
                                2282,
                                8186,
                                2665,
                                9599,
                                7692, 
                                6290, 
                                8179),
                         gender = c(
                           f, #astronauts
                           m,
                           f,
                           m,
                           m,
                           f,
                           f,
                           m,
                           m,
                           m,
                           f,
                           m,
                           f,
                           f,
                           f,
                           f),
                         DaysInSpaceAtSpace1 = c(
                           3,
                           3,
                           5,
                           5,
                           5,
                           4,
                           NA,
                           NA,
                           NA,
                           5,
                           3,
                           2,
                           4,
                           NA,
                           NA,
                           NA),
                         DaysOnEarthAtBDC2 = c(7,
                                               7,
                                               6,
                                               6,
                                               6,
                                               NA,
                                               NA,
                                               NA,
                                               7,
                                               2,
                                               4,
                                               7,
                                               NA,
                                               NA,
                                               NA,
                                               NA))

MenIDs = (GenderFrame %>% filter(gender == "man"))$ID
WomenIDs = (GenderFrame %>% filter(gender == "woman"))$ID

Task2_Astronauts$DaysInSpaceAtSpace1 = c()
Task2_Astronauts$DaysOnEarthAtBDC2 = c()
for(i in c(unique(Task2_Astronauts$id))){
  Task2_Astronauts$DaysInSpaceAtSpace1[Task2_Astronauts$id == i] = GenderFrame$DaysInSpaceAtSpace1[GenderFrame$ID == i]
  Task2_Astronauts$DaysOnEarthAtBDC2[Task2_Astronauts$id == i] = GenderFrame$DaysOnEarthAtBDC2[GenderFrame$ID == i]
}


Task2_Astronauts = Task2_Astronauts %>% 
  mutate(gender = case_when(
    id %in% MenIDs ~ "man",
    id %in% WomenIDs ~ "1woman"))

Task2_Astronauts$Ratio = Task2_Astronauts$Distance_Real/Task2_Astronauts$Distance_Subject

Task2_Astronauts$Posture[Task2_Astronauts$Posture == "stting"] = "sitting"
Task2_Astronauts$Posture[Task2_Astronauts$Posture == "sitting" & Task2_Astronauts$Astronauts == "Astronauts"] = "Sitting"
Task2_Astronauts$Posture[Task2_Astronauts$Posture == "lying" & Task2_Astronauts$Astronauts == "Astronauts"] = "Lying/Space"
Task2_Astronauts$Posture[Task2_Astronauts$Posture == "sitting" & Task2_Astronauts$Astronauts == "Controls"] = "Sitting"
Task2_Astronauts$Posture[Task2_Astronauts$Posture == "lying" & Task2_Astronauts$Astronauts == "Controls"] = "Lying"
Task2_Astronauts$Posture[Task2_Astronauts$Posture == "space"] = "Lying/Space"

#Task2_Astronauts = Task2_Astronauts[complete.cases(Task2_Astronauts),]

Task2_Astronauts = Task2_Astronauts %>% filter(id %in% c(2533,
                                                         3513,
                                                         8927, 
                                                         5706, 
                                                         1646, 
                                                         1001, 
                                                         2282, 
                                                         8186,
                                                         2665,
                                                         9599, 
                                                         7692, 
                                                         8179))

unique(Task2_Astronauts$id)

# Task2_Astronauts = Task2_Astronauts %>% 
#   mutate(LogRatio = log(Ratio),
#          IQR = IQR(log(Ratio)),
#          LowerQuartile = quantile(log(Ratio), prob=c(.25)),
#          UpperQuartile = quantile(log(Ratio), prob=c(.75)),
#          IQR2 = UpperQuartile-LowerQuartile,
#          Outlier = case_when(
#            log(Ratio) > UpperQuartile+1.5*IQR | log(Ratio) < LowerQuartile-1.5*IQR ~ "Outlier",
#            TRUE ~ "No Outlier"))

Task2_Astronauts = Task2_Astronauts %>% 
  group_by(id) %>% 
  mutate(SD3 = 3*sd(Ratio),
         Mean = mean(Ratio),
         Outlier = case_when(
           Ratio > Mean+SD3 | Ratio < Mean-SD3 ~ "Outlier",
           TRUE ~ "No Outlier"))

AllTrials = length(Task2_Astronauts$Ratio)

Task2_Astronauts = Task2_Astronauts %>% 
  filter(Outlier == "No Outlier")

TrialsAfterCriterion1 = length(Task2_Astronauts$Ratio)
AllTrials-TrialsAfterCriterion1
(AllTrials-TrialsAfterCriterion1)/AllTrials


Task2_Astronauts = Task2_Astronauts %>%
  mutate(
    In_Space =
      case_when(
        Posture %in% c("Sitting", "Lying") ~ 0,
        Posture %in% c("Lying/Space") ~ 1))

#Task2_Astronauts = Task2_Astronauts %>% 
#  group_by(id) %>%
#  mutate(SD_Per_Subject = sd(Ratio),
#         Mean_Per_Subject = mean(Ratio))

Task2_Astronauts = Task2_Astronauts %>%
  group_by(session,Astronauts) %>%
  mutate(PointInTime = case_when(
    session == "BDC 1" & Astronauts == "Astronauts" ~ "Session 1 (BDC 1)",
    session == "BDC 2" & Astronauts == "Astronauts" ~ "Session 4 (BDC 2)",
    session == "BDC 3" & Astronauts == "Astronauts" ~ "Session 5 (BDC 3)",
    session == "BDC 1" & Astronauts == "Controls" ~ "Session 1",
    session == "BDC 2" & Astronauts == "Controls" ~ "Session 2",
    session == "BDC 3" & Astronauts == "Controls" ~ "Session 3",
    session == "BDC 4" & Astronauts == "Controls" ~ "Session 4",
    session == "BDC 5" & Astronauts == "Controls" ~ "Session 5",
    session == "Space 1" ~ "Session 2 (Space 1)",
    session == "Space 2" ~ "Session 3 (Space 2)")
  )


Task2_Astronauts = Task2_Astronauts %>%
  group_by(session,Astronauts) %>%
  mutate(PointInTime_Cont = case_when(
    session == "BDC 1" & Astronauts == "Astronauts" ~ 1,
    session == "BDC 2" & Astronauts == "Astronauts" ~ 4,
    session == "BDC 3" & Astronauts == "Astronauts" ~ 5,
    session == "BDC 1" & Astronauts == "Controls" ~ 1,
    session == "BDC 2" & Astronauts == "Controls" ~ 2,
    session == "BDC 3" & Astronauts == "Controls" ~ 3,
    session == "BDC 4" & Astronauts == "Controls" ~ 4,
    session == "BDC 5" & Astronauts == "Controls" ~ 5,
    session == "Space 1" ~ 2,
    session == "Space 2" ~ 3)
  )


Task2_Astronauts = Task2_Astronauts %>%
  group_by(id,session,Posture,Distance_Real) %>% 
  mutate(Error = abs(Ratio - mean(Ratio)))

Task2_Astronauts_Error = Task2_Astronauts %>% 
  filter(Error != 0)
sum(Task2_Astronauts$Error == 0)/length(Task2_Astronauts$Error)

require(ggplot2)
require(dplyr)
require(cowplot)
require(quickpsy)
require(purrr)
theme_set(theme_cowplot())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
source("Utilities/Funs.R")


SimulatePsychometricData = function(nParticipants,
                                    ConditionOfInterest,
                                    StandardValues,
                                    reps,
                                    PSE_Differences,
                                    JND_Differences,
                                    Multiplicator_PSE_Standard,
                                    Multiplicator_SD_Standard,
                                    Type_ResponseFunction,
                                    SD_ResponseFunctions,
                                    Mean_Variability_Between,
                                    SD_Variability_Between){
  
  
  ID = paste0("S0",1:nParticipants)
  
  Psychometric = expand.grid(ID=ID, 
                             ConditionOfInterest=ConditionOfInterest, 
                             StandardValues=StandardValues,
                             PSE_Difference = PSE_Differences,
                             JND_Difference = JND_Differences,
                             reps = 1:reps,
                             SD_ResponseFunction = SD_ResponseFunctions)
  
  Psychometric = Psychometric %>%
    group_by(ID) %>%#
    mutate(PSE_Factor_ID = rnorm(1,1,Mean_Variability_Between), #how much variability is in the means of the psychometric functions between subjects?
           SD_Factor_ID = rnorm(1,1,SD_Variability_Between)) #how much variability is in the standard deviations of the psychometric functions between subjects?
  
  Psychometric = Psychometric %>%
    mutate(
      Mean_Standard = StandardValues+StandardValues*Multiplicator_PSE_Standard,
      SD_Standard = StandardValues*Multiplicator_SD_Standard,
      Mean = (Mean_Standard + (ConditionOfInterest==1)*Mean_Standard*PSE_Difference),
      SD = abs(SD_Standard + (ConditionOfInterest==1)*SD_Standard*JND_Difference))
  
  Psychometric = Psychometric %>%
    mutate(
      Mean = Mean*PSE_Factor_ID,
      SD = SD*SD_Factor_ID)
  
  if (Type_ResponseFunction == "normal"){
    
    Psychometric = Psychometric %>%
      mutate(
        staircase_factor = rnorm(length(reps),1,SD_ResponseFunction*(1+ConditionOfInterest*JND_Difference)))
    
  } else if (Type_ResponseFunction == "Cauchy"){
    
    Psychometric = Psychometric %>%
      mutate(
        staircase_factor = rcauchy(length(reps),1,SD_ResponseFunction*(1+ConditionOfInterest*JND_Difference)))
    
  } else{
    
    print("distribution not valid")
    
  }
  
  Psychometric = Psychometric %>%
    mutate(Presented_TestStimulusStrength = Mean*staircase_factor,
           Difference = Presented_TestStimulusStrength - StandardValues)
  
  Psychometric = Psychometric %>%
    mutate(
      AnswerProbability = pnorm(Presented_TestStimulusStrength,Mean,SD),
      
      Answer = as.numeric(rbernoulli(length(AnswerProbability),AnswerProbability))
    )
  
  
  Psychometric = Psychometric %>%
    filter(abs(staircase_factor-1) < 0.75) %>%
    group_by(ID,ConditionOfInterest,StandardValues,Difference) %>%
    mutate(Yes = sum(Answer==1),
           Total = length(ConditionOfInterest))
  
  Psychometric
}

RangeRepetitions = c(40,70,100)
ConditionOfInterest = c(0,1)
StandardValues = c(5)
PSE_Differences = c(0,0.15)
JND_Differences = c(0,0.2)
Multiplicator_PSE_Standard = 0
Multiplicator_SD_Standard = 0.15
Type_ResponseFunction = "Cauchy"
SD_ResponseFunctions = c(0.01,0.02,0.03,0.04,0.05,0.1,0.2)
Mean_Variability_Between = 0.2
SD_Variability_Between = 0.2

DataframeAtrevido = data.frame()

for (reps in RangeRepetitions){
  for (SD_ResponseFunction in SD_ResponseFunctions){
    DataframeAtrevido_Temp = SimulatePsychometricData(100,
                                     ConditionOfInterest,
                                     StandardValues,
                                     reps,
                                     PSE_Differences,
                                     JND_Differences,
                                     Multiplicator_PSE_Standard,
                                     Multiplicator_SD_Standard,
                                     Type_ResponseFunction,
                                     SD_ResponseFunction,
                                     Mean_Variability_Between,
                                     SD_Variability_Between)
    DataframeAtrevido_Temp$nReps = reps
    DataframeAtrevido = rbind(DataframeAtrevido,DataframeAtrevido_Temp)
  }
}

DataframeAtrevido = DataframeAtrevido %>%
  group_by(ID,StandardValues,ConditionOfInterest,SD_ResponseFunction,PSE_Difference,JND_Difference, nReps) %>%
  mutate(NumberOfTrials = length(ID),
         RowNumber = 1:length(ID)) %>%
  group_by(ID,reps,ConditionOfInterest,PSE_Difference,JND_Difference) %>%
  mutate(MinNumberOfTrials = min(NumberOfTrials)) %>%
  filter(RowNumber <= MinNumberOfTrials)

save(DataframeAtrevido, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/AreJNDsOK/DataframeAtrevido.RData"))
load(file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/AreJNDsOK/DataframeAtrevido.RData"))

JNDs = quickpsy::quickpsy(DataframeAtrevido,
                          Presented_TestStimulusStrength,
                          Yes,
                          grouping = .(ID,StandardValues,ConditionOfInterest,nReps,
                                       SD_ResponseFunction,PSE_Difference,JND_Difference,SD),
                          bootstrap = "none")

FittedJNDs = JNDs$par %>%
    filter(parn == "p2") %>%
    mutate(ActualSD = SD)
save(FittedJNDs, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/AreJNDsOK/FittedJNDs.RData"))
load(file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/AreJNDsOK/FittedJNDs.RData"))


ggplot(FittedJNDs %>% filter(JND_Difference == 0 & PSE_Difference == 0 & nReps == 100),aes(as.factor(ConditionOfInterest),par-ActualSD)) +
  geom_jitter(alpha = 0.2, width = 0.1) +
  geom_flat_violin(size=1) +
  ylim(c(-1,4)) +
  xlab("Experimental Condition") +
  ylab("Fitted JND - Actual JND") +
  stat_summary(fun = "mean", geom = "point", size = 6, aes(group=ID), shape = 95) +
  stat_summary(fun = "mean", geom = "point", size = 8, aes(group=0), shape = 16, color = "black") +
  facet_wrap(SD_ResponseFunction~.) +
  ggtitle("No effect of Experimental Condition on PSE, no effect of Experimental Condition on JND") +
  geom_hline(yintercept = 0, linetype = 2)
ggsave(paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/AreJNDsOK/DifferencePlot_NoEffects.jpg"), w = 10, h= 10)

ggplot(FittedJNDs %>% filter(JND_Difference == 0.2 & PSE_Difference == 0),aes(as.factor(ConditionOfInterest),par-ActualSD)) +
  geom_jitter(alpha = 0.2, width = 0.1) +
  geom_flat_violin(size=1) +
  ylim(c(-1,4)) +
  xlab("Experimental Condition") +
  ylab("Fitted JND - Actual JND") +
  stat_summary(fun = "mean", geom = "point", size = 6, aes(group=ID), shape = 95) +
  stat_summary(fun = "mean", geom = "point", size = 8, aes(group=0), shape = 16, color = "black") +
  facet_wrap(SD_ResponseFunction~.) +
  ggtitle("No effect of EC on PSE, effect of EC on JND") +
  geom_hline(yintercept = 0, linetype = 2)
ggsave(paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/AreJNDsOK/DifferencePlot_JNDEffect.jpg"), w = 10, h= 10)

ggplot(FittedJNDs %>% filter(JND_Difference == 0 & PSE_Difference == 0.15),aes(as.factor(ConditionOfInterest),par-ActualSD)) +
  geom_jitter(alpha = 0.2, width = 0.1) +
  geom_flat_violin(size=1) +
  ylim(c(-1,4)) +
  xlab("Experimental Condition") +
  ylab("Fitted JND - Actual JND") +
  stat_summary(fun = "mean", geom = "point", size = 6, aes(group=ID), shape = 95) +
  stat_summary(fun = "mean", geom = "point", size = 8, aes(group=0), shape = 16, color = "black") +
  facet_wrap(SD_ResponseFunction~.) +
  ggtitle("Effect of EC on PSE, no effect of EC on JND") +
  geom_hline(yintercept = 0, linetype = 2)
ggsave(paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/AreJNDsOK/DifferencePlot_PSEEffect.jpg"), w = 10, h= 10)

ggplot(FittedJNDs %>% filter(JND_Difference == 0.2 & PSE_Difference == 0.15),aes(as.factor(ConditionOfInterest),par-ActualSD)) +
  geom_jitter(alpha = 0.2, width = 0.1) +
  geom_flat_violin(size=1) +
  ylim(c(-1,4)) +
  xlab("Experimental Condition") +
  ylab("Fitted JND - Actual JND") +
  stat_summary(fun = "mean", geom = "point", size = 6, aes(group=ID), shape = 95) +
  stat_summary(fun = "mean", geom = "point", size = 8, aes(group=0), shape = 16, color = "black") +
  facet_wrap(SD_ResponseFunction~.) +
  ggtitle("Effect of EC on PSE, effect of EC on JND") +
  geom_hline(yintercept = 0, linetype = 2)
ggsave(paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/AreJNDsOK/DifferencePlot_BothEffects.jpg"), w = 10, h= 10)

ggplot(hello %>% filter(ID == "S01" & ConditionOfInterest == 1 & JND_Difference == 0 & PSE_Difference == 0) %>% 
         mutate(FunctionType = paste0("Cauchy (",SD_ResponseFunction,")")),
       aes(Presented_TestStimulusStrength)) +
  geom_density() +
  facet_wrap(FunctionType~.) +
  xlab("Presented Stimulus Strength") +
  ylab("Density")
ggsave(paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/AreJNDsOK/ExampleOfDistributionOfResponses.jpg"),w = 7, h= 7)

ggplot(hello %>% filter(ID == "S01" & ConditionOfInterest == 1 & JND_Difference == 0 & PSE_Difference == 0),
       aes(Presented_TestStimulusStrength, Answer)) +
  binomial_smooth() +
  geom_point() +
  facet_wrap(SD_ResponseFunction~.) +
  xlab("Presented Stimulus Strength") +
  ylab("Probability to answer that PEST is stronger")
ggsave(paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/AreJNDsOK/ExampleOfFittedPsychometricFunctions.jpg"),w = 7, h= 7)

FittedJNDs_Means_Diffs = FittedJNDs %>%
  filter(par < 5) %>% 
  group_by(SD_ResponseFunction, ConditionOfInterest,PSE_Difference,JND_Difference) %>%
  mutate(Mean_JND_Difference = mean(par-ActualSD)) %>%
  slice(1) %>% 
  mutate(FunctionType = paste0("Cauchy\n(",SD_ResponseFunction,")"),
         PSE_Dif = paste0("PSE Difference: ", PSE_Difference),
         JND_Dif = paste0("JND Difference: ", JND_Difference)) %>%
  ungroup() %>% 
  dplyr::select(FunctionType,ConditionOfInterest,Mean_JND_Difference,PSE_Dif,JND_Dif)

source("Are JNDs okay MoC.R")
Summary = rbind(FittedJNDs_Means,FittedJNDs_Means_Constant)

ggplot(Summary,aes(FunctionType,Mean_JND_Difference,color = as.factor(ConditionOfInterest))) +
  geom_point(size = 10) +
  geom_line() +
  xlab("Response Function") +
  ylab("Mean Fitted JND - Actual JND") +
  scale_color_manual(name = "Experimental\nCondition",values = c("orange","blue")) +
  facet_grid(PSE_Dif*JND_Dif~nReps)
ggsave(paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/AreJNDsOK/Fits.jpg"),w = 15, h= 7.5)


ggplot(FittedJNDs_Means_Diffs,aes(FunctionType,Mean_JND_Difference,color = as.factor(ConditionOfInterest))) +
  geom_point(size = 10) +
  xlab("Response Function") +
  ylab("Mean Fitted JND - Actual JND") +
  scale_color_manual(name = "Experimental\nCondition",values = c("orange","blue")) +
  facet_grid(PSE_Dif~JND_Dif) +
  geom_hline(yintercept = 0, linetype = 2, size = 2)



FittedJNDs_Means_nReps = FittedJNDs %>%
  filter(par < 5) %>% 
  group_by(SD_ResponseFunction, ConditionOfInterest,PSE_Difference,JND_Difference,nReps) %>%
  mutate(Mean_JND_Difference = mean(par-ActualSD)) %>%
  slice(1) %>% 
  mutate(FunctionType = paste0("Cauchy\n(",SD_ResponseFunction,")"),
         PSE_Dif = paste0("PSE Difference: ", PSE_Difference),
         JND_Dif = paste0("JND Difference: ", JND_Difference)) %>%
  ungroup() %>% 
  dplyr::select(FunctionType,ConditionOfInterest,Mean_JND_Difference,PSE_Dif,JND_Dif,nReps)

ggplot(FittedJNDs_Means_nReps,aes(FunctionType,Mean_JND_Difference,color = as.factor(ConditionOfInterest))) +
  geom_point(size = 10) +
  xlab("Response Function") +
  ylab("Mean Fitted JND - Actual JND") +
  scale_color_manual(name = "Experimental\nCondition",values = c("orange","blue")) +
  facet_grid(PSE_Dif*JND_Dif~nReps) +
  geom_hline(yintercept = 0, linetype = 2, size = 2)

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
    
  } else if (Type_ResponseFunction == "uniform"){
    
    Psychometric = Psychometric %>%
      mutate(
        staircase_factor = runif(length(reps),0.25,1.75))
    
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

RangeNs = c(10,12,14,16,18,20)
RangeRepetitions = c(40,70,100)

ConditionOfInterest = c(0,1)
StandardValues = c(5)
PSE_Differences = c(0,0.15)
JND_Differences = c(0,0.2)
Multiplicator_PSE_Standard = 0
Multiplicator_SD_Standard = 0.15
Type_ResponseFunction = "uniform"
SD_ResponseFunctions = 0
Mean_Variability_Between = 0.2
SD_Variability_Between = 0.2

nIterations = 200

TimeStartSimulations = Sys.time()

PowerfulDataframe = data.frame()

# hello_Constant = SimulatePsychometricData(1000,
#                          ConditionOfInterest,
#                          StandardValues,
#                          64,
#                          PSE_Differences,
#                          JND_Differences,
#                          Multiplicator_PSE_Standard,
#                          Multiplicator_SD_Standard,
#                          Type_ResponseFunction,
#                          SD_ResponseFunctions,
#                          Mean_Variability_Between,
#                          SD_Variability_Between)
# 
# hello_Constant = hello_Constant %>% 
#   group_by(ID,StandardValues,ConditionOfInterest,SD_ResponseFunction,PSE_Difference,JND_Difference) %>%
#   mutate(NumberOfTrials = length(ID),
#          RowNumber = 1:length(ID)) %>% 
#   ungroup() %>% 
#   mutate(MinNumberOfTrials = min(NumberOfTrials)) %>% 
#   group_by(ID,StandardValues,ConditionOfInterest,SD_ResponseFunction) %>%
#   filter(RowNumber <= MinNumberOfTrials)
# 
# save(hello_Constant, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/AreJNDsOK/hello_Constant.RData"))
load(file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/AreJNDsOK/hello_Constant.RData"))

# FittedJNDs_Constant_JNDs = quickpsy::quickpsy(hello_Constant,
#                           Presented_TestStimulusStrength,
#                           Yes,
#                           grouping = .(ID,StandardValues,
#                                        ConditionOfInterest,
#                                        SD,
#                                        Mean,
#                                        SD_ResponseFunction,
#                                        PSE_Difference,
#                                        JND_Difference),
#                                        bootstrap = "none")
# 
# FittedJNDs_Constant = FittedJNDs_Constant_JNDs$par %>%
#     filter(parn == "p2") %>%
#     mutate(ActualSD = SD)
# FittedJNDs_Constant$PSE = (FittedJNDs_Constant_JNDs$parini %>% filter(paran == "p1"))$par
# save(FittedJNDs_Constant, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/AreJNDsOK/FittedJNDs_Constant.RData"))
load(file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/AreJNDsOK/FittedJNDs_Constant.RData"))


ggplot(FittedJNDs_Constant %>% filter(JND_Difference == 0 & PSE_Difference == 0),aes(as.factor(ConditionOfInterest),par-ActualSD)) +
  geom_jitter(alpha = 0.2, width = 0.1) +
  geom_flat_violin(size=1) +
  xlab("Experimental Condition") +
  ylab("Fitted JND - Actual JND") +
  stat_summary(fun = "mean", geom = "point", size = 6, aes(group=ID), shape = 95) +
  stat_summary(fun = "mean", geom = "point", size = 8, aes(group=0), shape = 16, color = "black") +
  facet_wrap(SD_ResponseFunction~.) +
  ggtitle("No effect of EC on PSE, effect of EC on JND")
ggsave(paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/AreJNDsOK/DifferencePlot_NoEffects_ConstantStimuli.jpg"),w = 7, h= 7)

ggplot(FittedJNDs_Constant %>% filter(JND_Difference == 0.2 & PSE_Difference == 0),aes(as.factor(ConditionOfInterest),par-ActualSD)) +
  geom_jitter(alpha = 0.2, width = 0.1) +
  geom_flat_violin(size=1) +
  xlab("Experimental Condition") +
  ylab("Fitted JND - Actual JND") +
  stat_summary(fun = "mean", geom = "point", size = 6, aes(group=ID), shape = 95) +
  stat_summary(fun = "mean", geom = "point", size = 8, aes(group=0), shape = 16, color = "black") +
  facet_wrap(SD_ResponseFunction~.) +
  ggtitle("No effect of EC on PSE, effect of EC on JND")
ggsave(paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/AreJNDsOK/DifferencePlot_JNDEffect_ConstantStimuli.jpg"),w = 7, h= 7)

ggplot(FittedJNDs_Constant %>% filter(JND_Difference == 0 & PSE_Difference == 0.15),aes(as.factor(ConditionOfInterest),par-ActualSD)) +
  geom_jitter(alpha = 0.2, width = 0.1) +
  geom_flat_violin(size=1) +
  xlab("Experimental Condition") +
  ylab("Fitted JND - Actual JND") +
  stat_summary(fun = "mean", geom = "point", size = 6, aes(group=ID), shape = 95) +
  stat_summary(fun = "mean", geom = "point", size = 8, aes(group=0), shape = 16, color = "black") +
  facet_wrap(SD_ResponseFunction~.) +
  ggtitle("Effect of EC on PSE, no effect of EC on JND")
ggsave(paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/AreJNDsOK/DifferencePlot_PSEEffect_ConstantStimuli.jpg"),w = 7, h= 7)

ggplot(FittedJNDs_Constant %>% filter(JND_Difference == 0.2 & PSE_Difference == 0.15),aes(as.factor(ConditionOfInterest),par-ActualSD)) +
  geom_jitter(alpha = 0.2, width = 0.1) +
  geom_flat_violin(size=1) +
  xlab("Experimental Condition") +
  ylab("Fitted JND - Actual JND") +
  stat_summary(fun = "mean", geom = "point", size = 6, aes(group=ID), shape = 95) +
  stat_summary(fun = "mean", geom = "point", size = 8, aes(group=0), shape = 16, color = "black") +
  facet_wrap(SD_ResponseFunction~.) +
  ggtitle("Effect of EC on PSE, effect of EC on JND")
ggsave(paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/AreJNDsOK/DifferencePlot_BothEffects_ConstantStimuli.jpg"),w = 7, h= 7)

ggplot(hello_Constant %>% filter(ID == "S02" & ConditionOfInterest == 1 & StandardValues == 8 & JND_Difference == 0 & PSE_Difference == 0),
       aes(Presented_TestStimulusStrength)) +
  geom_density() +
  facet_wrap(SD_ResponseFunction~.) +
  xlab("Presented Stimulus Strength") +
  ylab("Density")
ggsave(paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/AreJNDsOK/ExampleOfDistributionOfResponses_ConstantStimuli.jpg"),w = 7, h= 7)

ggplot(hello_Constant %>% filter(ID == "S06" & ConditionOfInterest == 1 & StandardValues == 8 & JND_Difference == 0 & PSE_Difference == 0),
       aes(Presented_TestStimulusStrength, Answer)) +  
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  geom_point() +
  geom_point(aes(Presented_TestStimulusStrength,AnswerProbability),color = "red") +
  facet_wrap(SD_ResponseFunction~.) +
  xlab("Presented Stimulus Strength") +
  ylab("Probability to answer that PEST is stronger")
ggsave(paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/AreJNDsOK/ExampleOfFittedPsychometricFunctions_ConstantStimuli.jpg"),w = 7, h= 7)


FittedJNDs_Means_Constant = FittedJNDs_Constant %>%
  filter(par < 5) %>% 
  group_by(ConditionOfInterest,PSE_Difference,JND_Difference) %>%
  mutate(Mean_JND_Difference = mean(par-ActualSD)) %>%
  slice(1) %>% 
  mutate(FunctionType = "MoCS",
         PSE_Dif = paste0("PSE Difference: ", PSE_Difference),
         JND_Dif = paste0("JND Difference: ", JND_Difference)) %>%
  ungroup() %>% 
  dplyr::select(FunctionType,ConditionOfInterest,Mean_JND_Difference,PSE_Dif,JND_Dif)

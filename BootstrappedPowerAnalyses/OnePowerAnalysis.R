require(dplyr)
require(ggplot2)
require(purrr)
require(lme4)
require(lmerTest)
setwd(paste0(dirname(rstudioapi::getSourceEditorContext()$path)))
source("GetValuesFromPilotData.R")

############Make a function to simulate the data
SimulatePsychometricData = function(nParticipants,
                                    ConditionOfInterest1,
                                    ConditionOfInterest2,
                                    StandardValues,
                                    reps,
                                    PSE_Difference_CoI1,
                                    PSE_Difference_CoI2,
                                    PSE_Interaction,
                                    JND_Difference_CoI1,
                                    JND_Difference_CoI2,
                                    JND_Interaction,
                                    Multiplicator_PSE_Standard,
                                    Multiplicator_SD_Standard,
                                    Type_ResponseFunction,
                                    SD_ResponseFunction,
                                    Mean_Variability_Between,
                                    SD_Variability_Between){
  
ID = paste0("S0",1:nParticipants)
  
Psychometric = expand.grid(ID=ID, 
                             ConditionOfInterest1=ConditionOfInterest1, 
                             ConditionOfInterest2=ConditionOfInterest2,
                             StandardValues=StandardValues, 
                             reps = 1:reps)
  
Psychometric = Psychometric %>%
    group_by(ID) %>%#
    mutate(PSE_Factor_ID = rnorm(1,1,Mean_Variability_Between), #how much variability is in the means of the psychometric functions between subjects?
           SD_Factor_ID = rnorm(1,1,SD_Variability_Between)) #how much variability is in the standard deviations of the psychometric functions between subjects?
  
Psychometric = Psychometric %>%
  ungroup() %>%
    mutate(
      Mean_Standard = StandardValues*Multiplicator_PSE_Standard,
      SD_Standard = StandardValues*Multiplicator_SD_Standard,
      Mean_Step1 = case_when(
        ConditionOfInterest1 == "1Environment" & ConditionOfInterest2 == "1Fixation" ~ Mean_Standard,
        ConditionOfInterest1 == "1Environment" & ConditionOfInterest2 == "2Pursuit" ~ Mean_Standard + Mean_Standard*PSE_Difference_CoI2,
        ConditionOfInterest1 == "2No_Environment" & ConditionOfInterest2 == "1Fixation" ~ Mean_Standard + Mean_Standard*PSE_Difference_CoI1,
        ConditionOfInterest1 == "2No_Environment" & ConditionOfInterest2 == "2Pursuit" ~ Mean_Standard + Mean_Standard*PSE_Difference_CoI1 + 
                                                                                                         Mean_Standard*PSE_Difference_CoI2 + 
                                                                                                         Mean_Standard*PSE_Interaction),
      SD_Step1 = case_when(
        ConditionOfInterest1 == "1Environment" & ConditionOfInterest2 == "1Fixation" ~ SD_Standard,
        ConditionOfInterest1 == "1Environment" & ConditionOfInterest2 == "2Pursuit" ~ SD_Standard + JND_Difference_CoI2,
        ConditionOfInterest1 == "2No_Environment" & ConditionOfInterest2 == "1Fixation" ~ SD_Standard + JND_Difference_CoI1,
        ConditionOfInterest1 == "2No_Environment" & ConditionOfInterest2 == "2Pursuit" ~ SD_Standard + JND_Difference_CoI1 + 
                                                                                                       JND_Difference_CoI2 + 
                                                                                                       JND_Interaction))
  
Psychometric = Psychometric %>%
    mutate(
      Mean = Mean_Step1*PSE_Factor_ID,
      SD = SD_Step1*SD_Factor_ID)
  
if (Type_ResponseFunction == "normal"){
    
    Psychometric = Psychometric %>%
      mutate(
        staircase_factor = rnorm(length(reps),1,SD_ResponseFunction))
    
  } else if (Type_ResponseFunction == "Cauchy"){
    
    Psychometric = Psychometric %>%
      mutate(
        staircase_factor = rcauchy(length(reps),1,SD_ResponseFunction))
    
  } else{
    
    print("distribution not valid")
    
  }
  
Psychometric = Psychometric %>%
    mutate(Presented_TestStimulusStrength = Mean*staircase_factor)
  
Psychometric = Psychometric %>%
    mutate(
      AnswerProbability = pnorm(Presented_TestStimulusStrength,Mean,abs(SD)),
      Answer = as.numeric(rbernoulli(length(AnswerProbability),AnswerProbability))
    )

Psychometric
}


###############################################################
###########################simulate one data set###############
###############################################################
#set the parameters
RangeNs = c(25,50)
nParticipants = 200
ConditionOfInterest1 = c("1Environment", "2No_Environment")
ConditionOfInterest2 = c("1Fixation", "2Pursuit")
StandardValues = c(2, 4, 6)
reps = 30
PSE_Difference_CoI1 = PSE_Environment
PSE_Difference_CoI2 = PSE_Pursuit
#for Interaction we got a value of:
PSE_Interaction
#but let's simulate power for a range of values:
Range_PSE_Interaction = PSE_Interaction*c(1,0.5, 0.15) #Performance for CoI2 is 20% lower in presence of CoI1 = NoEnvironment than in presence of CoI1 = Environment
JND_Difference_CoI1 = SD_Environment
JND_Difference_CoI2 = SD_Pursuit
JND_Interaction = SD_Interaction
Multiplicator_PSE_Standard = Mean_Standard
#estimated SDs can be inflated heavily when trial counts are low, so let's divide it by two to get a more appropriate value:
Multiplicator_SD_Standard = Multiplicator_SD_Standard
Type_ResponseFunction = Type_ResponseFunction
SD_ResponseFunction = SD_ResponseFunction
Mean_Variability_Between = Mean_Variability_Between
SD_Variability_Between = SD_Variability_Between

# Build one dataset to check whether the everything was simulated properly
set.seed(657)
# Test = SimulatePsychometricData(nParticipants = 1000,
#                          ConditionOfInterest1,
#                          ConditionOfInterest2,
#                          StandardValues,
#                          reps,
#                          PSE_Difference_CoI1,
#                          PSE_Difference_CoI2,
#                          PSE_Interaction,
#                          JND_Difference_CoI1,
#                          JND_Difference_CoI2,
#                          JND_Interaction,
#                          Multiplicator_PSE_Standard,
#                          Multiplicator_SD_Standard,
#                          Type_ResponseFunction,
#                          SD_ResponseFunction,
#                          Mean_Variability_Between,
#                          SD_Variability_Between)
# 
# 
# BaseValuesForSim = Test %>% group_by(ID,ConditionOfInterest1,ConditionOfInterest2,StandardValues) %>%
#   slice(1)
# 
# 
# #Fit Psychometric Functions
# Test_PSEs1 = quickpsy::quickpsy(Test,Presented_TestStimulusStrength,Answer,
#                                                grouping = .(ID,StandardValues,ConditionOfInterest1,ConditionOfInterest2),
#                                                bootstrap = "none")
# Test_PSEs = Test_PSEs1$par %>% filter(parn == "p1")
# Test_PSEs$SD_Fitted = (Test_PSEs1$par %>% filter(parn == "p2"))$par
# 
# JointDF = inner_join(Test_PSEs,BaseValuesForSim)
# save(JointDF, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path), "JointDF.RData"))

load(file = paste0(dirname(rstudioapi::getSourceEditorContext()$path), "JointDF.RData"))



#Sample analysis
Hey = lmer(Mean/StandardValues ~ ConditionOfInterest1*ConditionOfInterest2 + (StandardValues | ID),
            data = JointDF %>% filter(SD > 0 & SD < 3*StandardValues & Mean > 0 & Mean < 3 * StandardValues))
summary(Hey)
Hey2 = lmer(par/StandardValues ~ ConditionOfInterest1*ConditionOfInterest2 + (StandardValues | ID),
     data = JointDF %>% filter(SD_Fitted > 0 & SD_Fitted < 3*StandardValues & par > 0 & par < 3 * StandardValues))
summary(Hey2)

Intercept_Obs = summary(LMM)$coef["(Intercept)","Estimate"]
Intercept_Sim_Early = summary(Hey)$coef["(Intercept)","Estimate"]
Intercept_Sim_Late = summary(Hey2)$coef["(Intercept)","Estimate"]
Coef_Environment_Obs = summary(LMM)$coef["EnvironmentNo_Environment","Estimate"]
Coef_Environment_Sim_Early = summary(Hey)$coef["ConditionOfInterest12No_Environment","Estimate"]
Coef_Environment_Sim_Late = summary(Hey2)$coef["ConditionOfInterest12No_Environment","Estimate"]
Coef_Pursuit_Obs = summary(LMM)$coef["FixationPursuit","Estimate"]
Coef_Pursuit_Sim_Early = summary(Hey)$coef["ConditionOfInterest22Pursuit","Estimate"]
Coef_Pursuit_Sim_Late = summary(Hey2)$coef["ConditionOfInterest22Pursuit","Estimate"]
Coef_EnvironmentXPursuit_Obs = summary(LMM)$coef["EnvironmentNo_Environment:FixationPursuit","Estimate"]
Coef_EnvironmentXPursuit_Sim_Early = summary(Hey)$coef["ConditionOfInterest12No_Environment:ConditionOfInterest22Pursuit","Estimate"]
Coef_EnvironmentXPursuit_Sim_Late = summary(Hey2)$coef["ConditionOfInterest12No_Environment:ConditionOfInterest22Pursuit","Estimate"]


###Compare Observed SDs to simulated SDs
Hey_SD = lmer(SD/StandardValues ~ ConditionOfInterest1 * ConditionOfInterest2 +
                 (StandardValues| ID),
               data = JointDF %>% filter(SD > 0 & SD < 3*StandardValues & Mean > 0 & Mean < 3 * StandardValues))
Hey_SD2 = lmer(SD_Fitted/StandardValues ~ ConditionOfInterest1 * ConditionOfInterest2 +
                (StandardValues| ID),
              data = JointDF %>% filter(SD_Fitted > 0 & SD_Fitted < 3*StandardValues & par > 0 & par < 3 * StandardValues))

Intercept_Obs = summary(LMM_SD)$coef["(Intercept)","Estimate"]
Intercept_Sim_Early = summary(Hey_SD)$coef["(Intercept)","Estimate"]
Intercept_Sim_Late = summary(Hey_SD2)$coef["(Intercept)","Estimate"]
Coef_Environment_SD_Obs = summary(LMM_SD)$coef["EnvironmentNo_Environment","Estimate"]
Coef_Environment_SD_Sim_Early = summary(Hey_SD)$coef["ConditionOfInterest12No_Environment","Estimate"]
Coef_Environment_SD_Sim_Late = summary(Hey_SD2)$coef["ConditionOfInterest12No_Environment","Estimate"]
Coef_Pursuit_SD_Obs = summary(LMM_SD)$coef["FixationPursuit","Estimate"]
Coef_Pursuit_SD_Sim_Early = summary(Hey_SD)$coef["ConditionOfInterest22Pursuit","Estimate"]
Coef_Pursuit_SD_Sim_Late = summary(Hey_SD2)$coef["ConditionOfInterest22Pursuit","Estimate"]
Coef_EnvironmentXPursuit_SD_Obs = summary(LMM_SD)$coef["EnvironmentNo_Environment:FixationPursuit","Estimate"]
Coef_EnvironmentXPursuit_SD_Sim_Early = summary(Hey_SD)$coef["ConditionOfInterest12No_Environment:ConditionOfInterest22Pursuit","Estimate"]
Coef_EnvironmentXPursuit_SD_Sim_Late = summary(Hey_SD2)$coef["ConditionOfInterest12No_Environment:ConditionOfInterest22Pursuit","Estimate"]

########Mean Standard
Mean_Standard_Obs = summary(LMM)$coef["(Intercept)","Estimate"]
Mean_Standard_Sim = summary(Hey)$coef["(Intercept)","Estimate"]

####Multiplicator_SD_Standard
Multiplicator_SD_Standard_Obs = summary(LMM_SD)$coef["(Intercept)","Estimate"]
Multiplicator_SD_Standard_Sim = summary(Hey_SD2)$coef["(Intercept)","Estimate"] + mean(ranef(Hey_SD2)$ID$`(Intercept)`)

####Mean_Variability_Between
Mean_Variability_Between_Obs = sd(ranef(LMM)$ID$'(Intercept)')
Mean_Variability_Between_Sim = sd(ranef(Hey)$ID$'(Intercept)')

####SD_Variability_Between
SD_Variability_Between_Obs = sd(ranef(LMM_SD)$ID$'(Intercept)')
SD_Variability_Between_Sim = sd(ranef(Hey_SD2)$ID$'(Intercept)')



#####check means rather than using LMMs
BaseValuesForSim %>% group_by(ConditionOfInterest1, ConditionOfInterest2) %>%
  filter(SD > 0 & SD < 3*StandardValues & Mean > 0 & Mean < 3 * StandardValues) %>%
  mutate(Mean_Mean_Step1 = mean(Mean_Standard/StandardValues)) %>% slice(1) %>%
  dplyr::select(ConditionOfInterest1, ConditionOfInterest2,Mean_Mean_Step1)

BaseValuesForSim %>% group_by(ConditionOfInterest1, ConditionOfInterest2) %>%
  filter(SD > 0 & SD < 3*StandardValues & Mean > 0 & Mean < 3 * StandardValues) %>%
  mutate(Mean_Mean_Step1 = mean(Mean_Step1/StandardValues)) %>% slice(1) %>%
  dplyr::select(ConditionOfInterest1, ConditionOfInterest2,Mean_Mean_Step1)

BaseValuesForSim %>% group_by(ConditionOfInterest1, ConditionOfInterest2) %>%
  filter(SD > 0 & SD < 3*StandardValues & Mean > 0 & Mean < 3 * StandardValues) %>%
  mutate(Mean_Mean = mean(Mean/StandardValues)) %>% slice(1) %>%
  dplyr::select(ConditionOfInterest1, ConditionOfInterest2,Mean_Mean)

Fits %>% filter(SD > 0 & SD < 3*velH & PSE > 0 & PSE < 3 * velH) %>%  group_by(Fixation,Environment) %>%
  mutate(Mean_Mean = mean(PSE/velH)) %>% slice(1) %>% dplyr::select(Fixation, Environment, Mean_Mean)
  
###SDs
JointDF %>% group_by(ConditionOfInterest1, ConditionOfInterest2) %>%
  filter(SD_Fitted > 0 & SD_Fitted < 3*StandardValues & par > 0 & par < 3 * StandardValues) %>%
  mutate(Mean_SD = mean(SD_Fitted/StandardValues)) %>% slice(1) %>%
  dplyr::select(ConditionOfInterest1, ConditionOfInterest2,Mean_SD)

Fits %>% filter(SD > 0 & SD < 3*velH & PSE > 0 & PSE < 3 * velH) %>%  group_by(Fixation,Environment) %>%
  mutate(Mean_SD = mean(SD/velH)) %>% slice(1) %>% dplyr::select(Fixation, Environment, Mean_SD)



###################################################################################
###############################Power Analysis######################################
###################################################################################
nIterations = 100
TimeStartSimulations = Sys.time()
PowerfulDataframe = data.frame()

for (nParticipants in c(63)){
  for (reps in c(30,40)){
    for (PSE_Interaction in Range_PSE_Interaction){
      
      TimeStartTrial = Sys.time() #get time at beginning of trial
      
      for(i in 1:nIterations){
        
        print(paste0("Number of Participants: ", nParticipants))
        print(paste0("Iteration: ", i))
        print(paste0("PSE_Interaction: ", round(PSE_Interaction,4)))
        
        Test = SimulatePsychometricData(nParticipants,
                                 ConditionOfInterest1,
                                 ConditionOfInterest2,
                                 StandardValues,
                                 reps,
                                 PSE_Difference_CoI1,
                                 PSE_Difference_CoI2,
                                 PSE_Interaction,
                                 JND_Difference_CoI1,
                                 JND_Difference_CoI2,
                                 JND_Interaction,
                                 Multiplicator_PSE_Standard,
                                 Multiplicator_SD_Standard,
                                 Type_ResponseFunction,
                                 SD_ResponseFunction,
                                 Mean_Variability_Between,
                                 SD_Variability_Between)
        
  
        GLMM = glmer(Answer ~ ConditionOfInterest1*ConditionOfInterest2 + Presented_TestStimulusStrength + (Presented_TestStimulusStrength| ID) + (Presented_TestStimulusStrength| StandardValues), 
                     family = binomial(link = "logit"), 
                     data = Test %>% filter(Presented_TestStimulusStrength < StandardValues*3 & Presented_TestStimulusStrength > 0),
                     nAGQ = 1,
                     glmerControl(optimizer = "nloptwrap"))
        
        Test_PSEs1 = quickpsy::quickpsy(Test %>% filter(Presented_TestStimulusStrength < StandardValues*3 & Presented_TestStimulusStrength > 0),Presented_TestStimulusStrength, Answer,
                                                 grouping = .(ID,StandardValues,ConditionOfInterest1,ConditionOfInterest2),
                                                 bootstrap = "none")
        Test_PSEs = Test_PSEs1$par %>% filter(parn == "p1")
        Test_PSEs$SD_Fitted = (Test_PSEs1$par %>% filter(parn == "p2"))$par
        
        LMM = lmer(par ~ ConditionOfInterest1*ConditionOfInterest2 + (StandardValues | ID),
                   data = Test_PSEs %>% filter(SD_Fitted > 0 & SD_Fitted < 3*StandardValues & par > 0 & par < 3 * StandardValues))
        
        PowerfulDataframe = rbind(PowerfulDataframe,data.frame(nParticipants = rep(nParticipants,2),
                                                      rep = rep(reps,2),
                                                      PSE_Interaction = rep(PSE_Interaction,2),
                                                      WhichValue = c("PSE_GLMM","PSE_LMM"),
                                                      pvalue = c(summary(GLMM)$coefficients["ConditionOfInterest12No_Environment:ConditionOfInterest22Pursuit","Pr(>|z|)"],
                                                                 summary(LMM)$coefficients["ConditionOfInterest12No_Environment:ConditionOfInterest22Pursuit","Pr(>|t|)"]),
                                                      estimate = c(summary(GLMM)$coefficients["ConditionOfInterest12No_Environment:ConditionOfInterest22Pursuit","Estimate"],
                                                                   summary(LMM)$coefficients["ConditionOfInterest12No_Environment:ConditionOfInterest22Pursuit","Estimate"]),
                                                      iteration = rep(i,2)))
        print(paste0("GLMM: ", round(summary(GLMM)$coefficients["ConditionOfInterest12No_Environment:ConditionOfInterest22Pursuit","Pr(>|z|)"],3)))
        print(paste0("LMM: ", round(summary(LMM)$coefficients["ConditionOfInterest12No_Environment:ConditionOfInterest22Pursuit","Pr(>|t|)"],3)))
      }
      print(paste0(nIterations, " iterations took ", round(Sys.time() - TimeStartTrial), " seconds.")) 
      print(paste0("The power for the current run through (",nParticipants," Participants, ", reps, " Repetitions) is ",mean(PowerfulDataframe$pvalue[PowerfulDataframe$nParticipants == nParticipants] < 0.05)))
      save(PowerfulDataframe, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/SavedVariables/PowerfulDataframe.RData"))
    }
  }
}

load(file=paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/PowerfulDataframe.RData"))
DurationSimulations = Sys.time() - TimeStartSimulations
colnames(PowerfulDataframe) = c("nParticipants","rep","PSE_Interaction", "WhichValue","pvalue","estimate","iteration")
alpha = 0.05


PowerfulDataframe = PowerfulDataframe %>% group_by(nParticipants,rep, WhichValue,PSE_Interaction) %>% 
  mutate(Power = mean(pvalue < alpha))

PowerfulDataframe %>% group_by(nParticipants,WhichValue,PSE_Interaction,rep) %>% 
  slice(1)

ggplot(PowerfulDataframe %>% filter(PSE_Interaction %in% sort(unique(PowerfulDataframe$PSE_Interaction))[1:3]), aes(nParticipants,Power, color = as.factor(PSE_Interaction))) +
  geom_line(linewidth = 1) +
  geom_point() +
  xlab("Number of Participants") +
  ylab("Power") +
  geom_hline(yintercept = 0.8, linetype=1) +
  geom_hline(yintercept = 0.9, linetype=2) +
  geom_hline(yintercept = 0.95, linetype=3) +
  ylim(c(0,1)) +
  facet_wrap(WhichValue~rep) +
  theme(legend.position = c(0.4,0.2))
ggsave("Figures/FigureOnePowerAnalysis.jpg",w = 7, h = 7)



#######################Try bootstrapping!
FinalData = Data %>%
  filter(!(nGoodPursuitPerCondition < 10 & Fixation == "Pursuit" & 
             Environment == "Environment_Local_Info") &
           !(nGoodFixationPerCondition < 10 & Fixation == "Fixation" & 
               Environment == "Environment_Local_Info")) %>% 
  filter(Good == 1) %>% #only good trials
  filter(ID %in% GoodIDs)

Power_DF_Bootstrap = data.frame()
k = 0

for (nParticipants in c(25,50,75)){
  
  for (k in 1:100){
    SampledIDs = sample(GoodIDs,nParticipants,replace = TRUE)
    BootstrappedData = data.frame()
    
    j = 0
    for (i in SampledIDs){
      j = j+1
      DF_ID_Replaced = FinalData %>% filter(ID == i)
      DF_ID_Replaced$ID = paste0("p",j)
      BootstrappedData = rbind(BootstrappedData, DF_ID_Replaced)
    }
    
    Fits_Bootstrap = (quickpsy::quickpsy(BootstrappedData, #only good participants
                                         x = velH_Pest, 
                                         k = Pest_Faster, 
                                         grouping = .(Environment ,Fixation,velH,ID), 
                                         bootstrap = "none"))$parini
    Fits_Bootstrap = cbind(Fits_Bootstrap %>% 
                             filter(paran == "p1"),(Fits_Bootstrap %>% 
                                                      filter(paran == "p2"))$par)
    colnames(Fits_Bootstrap) = c(colnames(Fits_Bootstrap)[1:5],"PSE","SD")
    
    LMM = lmer(PSE/velH ~ Environment * Fixation + (velH| ID), 
               data = Fits_Bootstrap %>% filter(SD > 0 & PSE > 0 & PSE < 3 * velH))
    
    Power_DF_Bootstrap = rbind(Power_DF_Bootstrap,
                               data.frame(nParticipants = nParticipants, iteration = k, pvalue = summary(LMM)$coefficients["EnvironmentNo_Environment:FixationPursuit","Pr(>|t|)"]))
    print(paste0("Participants:", nParticipants))
  }
}

save(Power_DF_Bootstrap, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/SavedVariables/Power_DF_Bootstrap.RData"))
load(file=paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/Power_DF_Bootstrap.RData"))

Power_DF_Bootstrap = Power_DF_Bootstrap %>% group_by(nParticipants) %>% 
  mutate(Power = mean(pvalue < alpha))

ggplot(Power_DF_Bootstrap, aes(nParticipants,Power)) +
  geom_line(linewidth = 1) +
  geom_point() +
  xlab("Number of Participants") +
  ylab("Power") +
  geom_hline(yintercept = 0.8, linetype=1) +
  geom_hline(yintercept = 0.9, linetype=2) +
  geom_hline(yintercept = 0.95, linetype=3) +
  ylim(c(0,1)) +
  theme(legend.position = c(0.4,0.2))


################################Compare Simulated Data to Fits
ggplot(JointDF %>% filter(par > 0 & par < StandardValues*3),
       aes(ConditionOfInterest1,par/StandardValues,color = ConditionOfInterest2)) +
  geom_boxplot()

ggplot(Fits %>% filter(PSE > 0 & PSE < velH*3),
       aes(Environment,PSE/velH,color = Fixation)) +
  geom_boxplot()

ggplot(JointDF %>% filter(par > 0 & par < StandardValues*3 & SD_Fitted > 0 & SD_Fitted < 3*StandardValues),
       aes(ConditionOfInterest1,SD_Fitted/StandardValues,color = ConditionOfInterest2)) +
  geom_boxplot()

ggplot(Fits %>% filter(PSE > 0 & PSE < velH*3 & SD > 0 & SD < 3*velH),
       aes(Environment,SD/velH,color = Fixation)) +
  geom_boxplot()

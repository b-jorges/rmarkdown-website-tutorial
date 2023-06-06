library(ggplot2)
library(quickpsy)
library(dplyr)
library(lme4)
library(lmerTest)
require(cowplot)
theme_set(theme_cowplot())

Data = read.csv(paste0(dirname(rstudioapi::getSourceEditorContext()$path),
                       "/AF.csv"))

Data = Data %>% 
  mutate(Good = case_when(
              Environment == "No_Environment" ~ 1,
              Mean_Speed_Ball == 0 ~ 1,
              is.nan(Mean_Speed_Ball) ~ 1,
              velH < 0 & Mean_Speed_Ball < velH/2 & Fixation == "Pursuit" ~ 1,
              velH > 0 & Mean_Speed_Ball > velH/2 & Fixation == "Pursuit" ~ 1,
              abs(Mean_Speed_Ball) < abs(velH/3) & Fixation == "Fixation" ~ 1,
              TRUE ~ 0))

Data = Data %>% 
  mutate(GoodPursuit = case_when(
              Environment == "No_Environment" ~ "No Environment",
              Fixation == "Fixation" ~ "Fixation",
              Mean_Speed_Ball == 0 ~ "Eye-Tracking broken",
              is.nan(Mean_Speed_Ball) ~ "Eye-Tracking broken",
              velH < 0 & Mean_Speed_Ball > velH/2 ~ "BadPursuit",
              velH > 0 & Mean_Speed_Ball < velH/2 ~ "BadPursuit",
              velH < 0 & Mean_Speed_Ball <= velH/2 ~ "GoodPursuit",
              velH > 0 & Mean_Speed_Ball >= velH/2 ~ "GoodPursuit"),
         GoodFixation = case_when(
              Environment == "No_Environment" ~ "No Environment",
              Fixation == "Pursuit" ~ "Pursuit",
              abs(Mean_Speed_Ball) <= abs(velH)/3 ~ "GoodFixation",
              abs(Mean_Speed_Ball) > abs(velH)/3 ~ "BadFixation"))

Data$velH = abs(Data$velH)
Data$velH_Pest = abs(Data$velH_Pest)

#Filter out when there is less than 6 valid trials left in a condition:
Data = Data %>% 
  group_by(ID,Environment, Fixation, velH) %>%
  mutate(nGoodPursuitPerCondition = sum(GoodPursuit == "GoodPursuit"),
         nGoodFixationPerCondition = sum(GoodFixation == "GoodFixation"))


###chuck out participants that don't do eye stuff as we want them to
ParticipantGood = Data %>%
  group_by(ID,Environment, Fixation, velH) %>%
  dplyr::select(ID, Environment, Fixation, velH, nGoodPursuitPerCondition,nGoodFixationPerCondition) %>% 
  filter(Environment != "No_Environment") %>% 
  slice(1)

ParticipantGood = ParticipantGood %>% 
  mutate(ConditionGood = case_when(
    nGoodPursuitPerCondition > 10 | nGoodFixationPerCondition > 10 ~ 1,
    TRUE ~ 0)) %>% 
  group_by(ID) %>%
  mutate(ParticipantGood = case_when(
    sum(ConditionGood==1) > 4 ~ 1,
    TRUE ~ 0))
WhichParticipantsGood = ParticipantGood %>% 
  group_by(ID) %>% 
  slice(1)
GoodIDs = WhichParticipantsGood$ID[WhichParticipantsGood$ParticipantGood == 1]


CheckQuality = Data %>%
  filter(Environment != "No_Environment") %>% 
  group_by(ID,Fixation, Environment, velH) %>% 
  mutate(HowManyGood = sum(Good == 1)) %>% 
  slice(1) %>% 
  dplyr::select(ID,Fixation,Environment,velH,HowManyGood,nGoodPursuitPerCondition,nGoodFixationPerCondition)

Fits = (quickpsy::quickpsy(Data %>%
                             filter(!(nGoodPursuitPerCondition < 10 & Fixation == "Pursuit" & 
                                      Environment == "Environment_Local_Info") &
                                    !(nGoodFixationPerCondition < 10 & Fixation == "Fixation" & 
                                      Environment == "Environment_Local_Info")) %>% 
                             filter(Good == 1) %>% #only good trials
                             filter(ID %in% GoodIDs), #only good participants
                               x = velH_Pest, 
                               k = Pest_Faster, 
                               grouping = .(Environment,Fixation,velH,ID), 
                               bootstrap = "none"))$parini

Fits = cbind(Fits %>% 
  filter(paran == "p1"),(Fits %>% 
    filter(paran == "p2"))$par)
colnames(Fits) = c(colnames(Fits)[1:5],"PSE","SD")

mean(Fits$SD[Fits$SD > 0]/Fits$velH[Fits$SD > 0])


############################################################
############get values for power simulations################
############################################################
######PSE Differences
######Take values from LMM:
LMM = lmer(PSE/velH ~ Environment * Fixation + 
             (velH| ID), 
           data = Fits %>% filter(SD > 0 & PSE > 0 & PSE < 3 * velH))
summary(LMM)

Intercept = summary(LMM)$coef["(Intercept)","Estimate"]  + mean(ranef(LMM)$ID$'(Intercept)')
Coef_Environment = summary(LMM)$coef["EnvironmentNo_Environment","Estimate"]*1/Intercept
Coef_Pursuit = summary(LMM)$coef["FixationPursuit","Estimate"]*1/Intercept
Coef_EnvironmentXPursuit = summary(LMM)$coef["EnvironmentNo_Environment:FixationPursuit","Estimate"]*1/Intercept

#Difference between "Environment" and "No Environment" in percent:
PSE_Environment = Coef_Environment

#Difference between "Fixation" and "Pursuit" in percent
PSE_Pursuit = Coef_Pursuit

#How much slower is the target perceived at "No Environment" when "Pursuit" in comparison to "Environment" and "Pursuit"
PSE_Interaction = Coef_EnvironmentXPursuit


######SD Differences
######Take values from LMM:
LMM_SD = lmer(SD/velH ~ Environment * Fixation + 
             (velH| ID), 
           data = Fits %>% filter(SD > 0 & SD < 3*velH & PSE > 0 & PSE < 3 * velH))
summary(LMM_SD)
ranef(LMM_SD)

Intercept = summary(LMM_SD)$coef["(Intercept)","Estimate"]
Coef_Environment_SD = summary(LMM_SD)$coef["EnvironmentNo_Environment","Estimate"]*1/Intercept
Coef_Pursuit_SD = summary(LMM_SD)$coef["FixationPursuit","Estimate"]*1/Intercept
Coef_EnvironmentXPursuit_SD = summary(LMM_SD)$coef["EnvironmentNo_Environment:FixationPursuit","Estimate"]*1/Intercept

#Difference between "Environment" and "No Environment" in percent:
SD_Environment = Coef_Environment_SD
SD_Environment

#Difference between "Fixation" and "Pursuit" in percent
SD_Pursuit = Coef_Pursuit_SD
SD_Pursuit

#How does variability differ in "No Environment" when "Pursuit" in comparison to "Environment" and "Pursuit"
SD_Interaction = Coef_EnvironmentXPursuit_SD
SD_Interaction

########Mean Standard
Mean_Standard = summary(LMM)$coef["(Intercept)","Estimate"] + mean(ranef(LMM)$ID$'(Intercept)')

####Multiplicator_SD_Standard
Multiplicator_SD_Standard = summary(LMM_SD)$coef["(Intercept)","Estimate"] + mean(ranef(LMM_SD)$ID$'(Intercept)')

####Mean_Variability_Between
Mean_Variability_Between = sd(ranef(LMM)$ID$'(Intercept)')

####SD_Variability_Between
SD_Variability_Between = sd(ranef(LMM_SD)$ID$'(Intercept)')


####SD_ResponseFunction ... fitting Cauchy and normal functions
require(MASS)
ResponseDistribution = Data %>% 
  group_by(ID, velH) %>%
  mutate(Scale_Cauchy = fitdistr(velH_Pest/velH,"cauchy")$estimate[2],
         SD_Normal = fitdistr(velH_Pest/velH,"normal")$estimate[2],
         loglikelihood_Cauchy = fitdistr(velH_Pest/velH,"cauchy")$loglik,
         loglikelihood_Normal = fitdistr(velH_Pest/velH,"normal")$loglik,
         loglikelihood_Difference = loglikelihood_Cauchy-loglikelihood_Normal) %>% 
  dplyr::select(ID,velH,Scale_Cauchy,loglikelihood_Cauchy,SD_Normal,loglikelihood_Normal, loglikelihood_Difference) %>% 
  slice(1) %>%
  ungroup() %>% 
  summarise(median_Scale_Cauchy = median(Scale_Cauchy),
            median_SD_Normal = median(SD_Normal),
            median_loglike_CauchyMinusNormal = median(loglikelihood_Difference))

if (ResponseDistribution[3] > 0){
  SD_ResponseFunction = ResponseDistribution$median_Scale_Cauchy
  Type_ResponseFunction = "Cauchy"
} else {
  SD_ResponseFunction = ResponseDistribution$median_SD_Normal
  Type_ResponseFunction = "normal"
}
SD_ResponseFunction
Type_ResponseFunction








LMM = lmer(PSE/velH ~ Environment * Fixation + 
             (velH| ID), 
           data = Fits %>% filter(SD > 0 & PSE > 0 & PSE < 3 * velH))
summary(LMM)
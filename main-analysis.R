setwd('/Users/azeezatmustapha/Desktop/school/winter3/abda')
###Group 18
##Load package
library(scaler)
library(dplyr) 
library(brms)
library(xtable)

###Reading and cleaning of data
statlog <- read.csv("data/statlog.csv", sep=' ')

statlog = subset(statlog, select = -c(vessels,thal) )

statlog$HeartDisease[statlog$HeartDisease == 1] <- 0
statlog$HeartDisease[statlog$HeartDisease == 2] <- 1

statlog$Location <- 'Statlog'

clv <- read.csv("data/cleveland.csv", sep = ",")
clv = subset(clv, select = -c(vessels,thal) )
clv$HeartDisease[clv$HeartDisease != 0] <- 1
clv$Location <- 'Cleveland'

va <- read.csv("data/va.csv", sep = ",")
colSums(va =="?") ## over 90% of vessels and thal contains ? so they are removed 
va = subset(va, select = -c(vessels,thal) )

va$HeartDisease[va$HeartDisease != 0] <- 1
va$Location <- 'VA Long Beach'

### CHECK for the column with the missing value
colSums(va =="?")

### Delete rows of categorical variables with missing values
va <- va[va$ST_Slope != "?" & va$FastingBS != "?", ]


hung <- read.csv("data/hungarian.csv", sep = ",")
colSums(hung =="?") 

## over 90% of vessels and thal contains ? so they are removed 
hung = subset(hung, select = -c(vessels,thal) )

### CHECK for the column with the missing value
colSums(hung =="?")
### Delete rows of categorical variables with missing values
hung <- hung[hung$ST_Slope != "?" & hung$FastingBS != "?" & hung$ExerciseAngina != "?", ]

colSums(hung =="?")

hung$HeartDisease[hung$HeartDisease != 0] <- 1
hung$Location <- 'Hungary'

swit <- read.csv("data/switzerland.csv", sep=',')

colSums(swit =="?") 

## over 90% of vessels and thal contains ? so they are removed
swit = subset(swit, select = -c(vessels,thal) )

### CHECK for the column with the missing value
colSums(swit =="?")
### Delete rows of categorical variables with missing values
swit <- swit[swit$ST_Slope != "?" & swit$FastingBS != "?" & swit$ExerciseAngina != "?", ]
colSums(swit =="?")


swit$HeartDisease[swit$HeartDisease != 0] <- 1
swit$Location <- 'Switzerland'

###MERGE ALL DATA FRAME
list_of_dfs <- list(statlog, clv, hung, va, swit)
main_df <- do.call(rbind, list_of_dfs)

### recode all categorical data to match 
main_df$ChestPainType[main_df$ChestPainType == 4] <- 'ASY'
main_df$ChestPainType[main_df$ChestPainType == 3] <- 'NAP'
main_df$ChestPainType[main_df$ChestPainType == 2] <- 'ATA'
main_df$ChestPainType[main_df$ChestPainType == 1] <- 'TA'

main_df$RestingECG[main_df$RestingECG == 0] <- 'Normal'
main_df$RestingECG[main_df$RestingECG == 1] <- 'ST'
main_df$RestingECG[main_df$RestingECG == 2] <- 'LVH'

main_df$ExerciseAngina[main_df$ExerciseAngina == 0] <- 'N'
main_df$ExerciseAngina[main_df$ExerciseAngina == 1] <- 'Y'

main_df$ST_Slope[main_df$ST_Slope == 1] <- 'Up'
main_df$ST_Slope[main_df$ST_Slope == 2] <- 'Flat'
main_df$ST_Slope[main_df$ST_Slope == 3] <- 'Down'

main_df$Sex[main_df$Sex == 0] <- 'F'
main_df$Sex[main_df$Sex == 1] <- 'M'


### TRANSFORMING VARIABLES TO APPROPRIATE TYPE
str(main_df)
main_df$ChestPainType <- as.factor(main_df$ChestPainType)
main_df$Sex <- as.factor(main_df$Sex)
main_df$FastingBS <- as.factor(main_df$FastingBS)
main_df$RestingECG  <- as.factor(main_df$RestingECG)
main_df$ST_Slope <- as.factor(main_df$ST_Slope)
main_df$Location  <- as.factor(main_df$Location)
main_df$ExerciseAngina <- as.factor(main_df$ExerciseAngina)
main_df$HeartDisease <- as.factor(main_df$HeartDisease)
main_df$RestingBP <- as.numeric(main_df$RestingBP)
main_df$Cholesterol <- as.numeric(main_df$Cholesterol)
main_df$MaxHR <- as.numeric(main_df$MaxHR)
main_df$Oldpeak <- as.numeric(main_df$Oldpeak)

# Calculate group means
group_means <- ave(main_df$Cholesterol, main_df$Location, FUN = function(x) mean(x, na.rm = TRUE))
group_means
# Replace the value with group mean
main_df$Cholesterol[is.na(main_df$Cholesterol) ] <- group_means[is.na(main_df$Cholesterol)]

# Calculate group means for restingBP
group_meansrestingBP <- ave(main_df$RestingBP, main_df$Location, FUN = function(x) mean(x, na.rm = TRUE))

# Replace the value with group mean
main_df$RestingBP[is.na(main_df$RestingBP) ] <- group_meansrestingBP[is.na(main_df$RestingBP)]

summary(main_df)

#scaling and centering of variables
df = scale_df(main_df) %>% center_df()

##Prior Definition
### Prior for model with sex and age
prior_def_age_sex <- prior(student_t(1, 0, 2.5), class = "Intercept") +
  prior(normal(0.5, 1), class = "b", coef = "Age") +
  prior(normal(0.5, 1), class = "b", coef = "SexM")

### Prior for model with all variables
prior_def <- prior(student_t(1, 0, 2.5), class = "Intercept") +
prior(normal(0.5, 1), class = "b", coef = "Age") +
  prior(normal(0.5, 1), class = "b", coef = "Cholesterol") +
  prior(normal(0.5, 1), class = "b", coef = "SexM")+
  prior(normal(0, 1), class = "b", coef = "ChestPainTypeATA")+
  prior(normal(0, 1), class = "b", coef = "ChestPainTypeNAP")+
  prior(normal(0, 1), class = "b", coef = "ChestPainTypeTA")+
   prior(normal(0.5, 1), class = "b", coef = "RestingBP")+
  prior(normal(0.5, 1), class = "b", coef = "FastingBS1")+
  prior(normal(0, 1), class = "b", coef = "RestingECGNormal")+
  prior(normal(0.5, 1), class = "b", coef = "RestingECGST")+
  prior(normal(0, 1), class = "b", coef = "MaxHR")+
  prior(normal(0.5, 1), class = "b", coef = "ExerciseAnginaY")+
  prior(normal(0, 1), class = "b", coef = "Oldpeak")+
  prior(normal(0, 1), class = "b", coef = "ST_SlopeFlat")+
  prior(normal(0, 1), class = "b", coef = "ST_SlopeUp")

### Prior for model with all variables and interactions
prior_def_interaction <- prior(student_t(1, 0, 2.5), class = "Intercept") +
  prior(normal(0.5, 1), class = "b", coef = "Age") +
  prior(normal(0.5, 1), class = "b", coef = "Cholesterol") +
  prior(normal(0.5, 1), class = "b", coef = "SexM")+
  prior(normal(0, 1), class = "b", coef = "ChestPainTypeATA")+
  prior(normal(0, 1), class = "b", coef = "ChestPainTypeNAP")+
  prior(normal(0, 1), class = "b", coef = "ChestPainTypeTA")+
  prior(normal(0, 1), class = "b", coef = "Age:ChestPainTypeATA")+
  prior(normal(0, 1), class = "b", coef = "Age:ChestPainTypeNAP")+
  prior(normal(0, 1), class = "b", coef = "Age:ChestPainTypeTA")+
  prior(normal(0, 1), class = "b", coef = "Age:RestingBP")+
  prior(normal(0.5, 1), class = "b", coef = "RestingBP")+
  prior(normal(0.5, 1), class = "b", coef = "FastingBS1")+
  prior(normal(0, 1), class = "b", coef = "RestingECGNormal")+
  prior(normal(0.5, 1), class = "b", coef = "RestingECGST")+
  prior(normal(0, 1), class = "b", coef = "MaxHR")+
  prior(normal(0.5, 1), class = "b", coef = "ExerciseAnginaY")+
  prior(normal(0, 1), class = "b", coef = "Oldpeak")+
  prior(normal(0, 1), class = "b", coef = "ST_SlopeFlat")+
  prior(normal(0, 1), class = "b", coef = "ST_SlopeUp")+
  prior(normal(0, 1), class = "b", coef = "SexM:FastingBS1")+
  prior(normal(0, 1), class = "b", coef = "SexM:RestingBP")

set.seed(100)
##MODELS
### Model with age and sex as predictors 
model_Age_Sex <- brm(HeartDisease ~ Age + Sex, 
                 family = bernoulli(link = logit), data = df, prior = prior_def_age_sex, save_pars = save_pars(all = TRUE))

summary(model_Age_Sex)
### extract into latext table
table1 = summary(model_Age_Sex)$fixed
xtable(table1)

plot(model_Age_Sex)

### multilevel model of age and sex 
model_Age_Sex_location <- brm(HeartDisease ~ Age + Sex + (1|Location), 
                     family = bernoulli(link = logit), data = df, prior = prior_def_age_sex,  save_pars = save_pars(all = TRUE))

summary(model_Age_Sex_location)

### extract into latext table
table2 = summary(model_Age_Sex_location)$fixed
xtable(table2)

### model with all variables
model_All <- brm(HeartDisease ~ 
                   Age + 
                   Sex +
                   ChestPainType +
                   RestingBP+
                   Cholesterol+
                   FastingBS+
                   RestingBP +
                   MaxHR +
                   RestingECG+
                   ExerciseAngina+
                   Oldpeak+
                   ST_Slope , 
                 family = bernoulli(link = logit), data = df, prior = prior_def, save_pars = save_pars(all = TRUE))

summary(model_All)
### extract into latext table
table3 = summary(model_All)$fixed
xtable(table3)

plot(model_All)

### multilevel model with all variables
model_All_location <- brm(HeartDisease ~ Age + Sex +
                            ChestPainType +
                            RestingBP+
                            Cholesterol+
                            FastingBS+
                            RestingBP +
                            MaxHR +
                            RestingECG+
                            ExerciseAngina+
                            Oldpeak+
                            ST_Slope + (1|Location), 
                            family = bernoulli(link = logit), data = df, prior = prior_def, control = list(adapt_delta = 0.95), save_pars = save_pars(all = TRUE))

summary(model_All_location)

### extract into latext table
table4 = summary(model_All_location)$fixed
xtable(table4)


##Interaction effects
model_All_int <- brm(HeartDisease ~ Age + Sex +
                            ChestPainType +
                            RestingBP+
                            Cholesterol+
                            FastingBS+
                            RestingBP +
                            MaxHR +
                            RestingECG+
                            ExerciseAngina+
                            Oldpeak+
                            ST_Slope +Age*Sex+ Age*ChestPainType + Age*RestingBP + Sex*FastingBS + Sex*RestingBP,
                          family = bernoulli(link = logit), data = df, prior = prior_def_interaction, control = list(adapt_delta = 0.95), save_pars = save_pars(all = TRUE))
summary(model_All_int)

### extract into latext table
table5 = summary(model_All_int)$fixed
xtable(table5)

### multilevel model with interaction effects 
model_All_int_location <- brm(HeartDisease ~ Age + Sex +
                       ChestPainType +
                       RestingBP+
                       Cholesterol+
                       FastingBS+
                        RestingBP +
                        MaxHR +
                         RestingECG+
                       ExerciseAngina+
                       Oldpeak+
                       ST_Slope +  Age*Sex +Age*ChestPainType + Age*RestingBP + Sex*FastingBS + Sex*RestingBP + (1|Location) 
                     , family = bernoulli(link = logit), data = df, prior = prior_def_interaction, control = list(adapt_delta = 0.95), save_pars = save_pars(all = TRUE))

summary(model_All_int_location)

### extract into latext table
table6 = summary(model_All_int_location)$fixed
xtable(table6)

### Visualization of the conditional effects of the interaction effects
conditional_effects(model_All_int)

### Model comparison
loo( model_Age_Sex, model_Age_Sex_location, model_All, model_All_location,model_All_int, model_All_int_location, moment_match = TRUE)


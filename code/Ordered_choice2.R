library(dplyr)
library(MASS)
library(lmtest)
library(mlogit)

load("~/processed_data/df_for_modeling.RData")

summary(df_for_modeling)
dim(df_for_modeling)
colnames(df_for_modeling)

print((df_for_modeling$Quality.of.Life))

sum(!complete.cases(df_for_modeling))  # Number of rows with NA
sum(sapply(df_for_modeling, function(x) any(is.infinite(x))))

df_for_modeling <- df_for_modeling[!is.na(df_for_modeling$Quality.of.Life), ]
df_for_modeling <- df_for_modeling %>% select(-ID)


# numeric_vars <- sapply(df_for_modeling, is.numeric)
# df_for_modeling[numeric_vars] <- scale(df_for_modeling[numeric_vars])
# 
# str(df_for_modeling)
# 
# train_index <- createDataPartition(df_for_modeling$Quality.of.Life, p = 0.8, list = FALSE)
# train_data <- df_for_modeling[train_index, ]
# test_data  <- df_for_modeling[-train_index, ]


ordered_logit_model <- ologit.reg(Quality.of.Life ~ . , data = df_for_modeling)
summary(ordered_logit_model)
coeftest(ordered_logit_model)
brant(ordered_logit_model)


ordered_logit_model_update2

ordered_logit_model_update <- ologit.reg(Quality.of.Life ~ . - Smoke.Detector - Traffic - Aware.of.AARC - Knowledge
                                         - Weakness - Email - Mobile.Apps - Preferance - Primary.Care - Familiarity.with.Ethnic.Origin
                                         - Student - Sibling_LivingWith - Religious.Importance - Marital.Status - Alone
                                         - Spouse_LivingWith - Retired - English.Difficulties - Physical.Check.up
                                         - Dental.Insurance - Informed - Housing - Focus.Group - City.Election - Contact.City.Official
                                         - Recycle - Satisfaction.With.Housing - APD.Languages - Library.Internet.Acess
                                         - X9.1.1 - Disclosure - Antidepressants - Danger - Social.Networks - Health.Website - Family
                                         - Urgentcare - Public.Transportation - EMS.Classes - Familiarity.with.America
                                         - Language - Home.Phone - District - Citizenship.Class - Religious.Attendance
                                         - Fire.Alarm - Public.Computer - Car.Share - Religion - Hypertension - Transportation..Medical.
                                         - Online.Communities - Preference - City.Residency - City.Effort.Satisfaction
                                         - Parent_LivingWith - Full_Time_Employment_Status - Identify.Ethnically - Unmet.Health.Need
                                         - Unmet.Dental.Needs - Comunication.Problem - users - Acquaintances - Recovery - Treatment
                                         - Counseling - Diagnosed - Have.an.Advanced.Directive - Small.Businesses
                                         - Homemaker_Status - Children_LivingWith - Belonging - Smoking - Dentist.Check.up - X3.1.1
                                         - Ethnicity - Income - Close.Friend - Superstition - English.Speaking - Place.to.Live.Combined.Score
                                         - Walking - Compost - Bicycling - Carpooling - Personal.Car - Disappointment
                                         - Education.Completed - Diabetes - Activities - English.Classes - Small.Business
                                         - Public.Meeting - House_Ownership - Shame - Household.Size - Healthy.Diet - Quality_of_city_life
                                         - Age - Literature - Folkmedicine
                                         , data = df_for_modeling)

lrtest(ordered_logit_model_update2, ordered_logit_model_update)

summary(ordered_logit_model_update)

df_for_modeling$Satisfaction <- as.ordered(df_for_modeling$Satisfaction)


df_for_modeling$isKorean <- 0
df_for_modeling$isKorean[df_for_modeling$Ethnicity=="Korean"] = 1

df_for_modeling$highIncome <- 0
df_for_modeling$highIncome[df_for_modeling$Income %in% c("7", "8")] <- 1

df_for_modeling$SuperstitionHigh <- 0
df_for_modeling$SuperstitionHigh[df_for_modeling$Superstition == "4"] <- 1

df_for_modeling$GoodEnglish <- 0
df_for_modeling$GoodEnglish[df_for_modeling$English.Speaking %in% c("3", "4")] <- 1

df_for_modeling$highQualityCity <- 0
df_for_modeling$highQualityCity[df_for_modeling$Quality_of_city_life %in% c("3", "4")] <- 1


ordered_logit_model_bad <- ologit.reg(Quality.of.Life ~ Age+ Marital.Status + Language + Education.Completed +
                                        Alone 
                                        , data = df_for_modeling)

summary(ordered_logit_model_bad)
coeftest(ordered_logit_model)

marginaleffects(ordered_logit_model, characteristics = atFUN(min))
ordered_logit_model$Hetero <- FALSE
margins.oglmx(ordered_logit_model_update)

ordered_logit_model_update_polr <- polr(Quality.of.Life ~ . - Smoke.Detector - Traffic - Aware.of.AARC - Knowledge
                                        - Weakness - Email - Mobile.Apps - Preferance - Primary.Care - Familiarity.with.Ethnic.Origin
                                        - Student - Sibling_LivingWith - Religious.Importance - Marital.Status - Alone
                                        - Spouse_LivingWith - Retired - English.Difficulties - Physical.Check.up
                                        - Dental.Insurance - Informed - Housing - Focus.Group - City.Election - Contact.City.Official
                                        - Recycle - Satisfaction.With.Housing - APD.Languages - Library.Internet.Acess
                                        - X9.1.1 - Disclosure - Antidepressants - Danger - Social.Networks - Health.Website - Family
                                        - Urgentcare - Public.Transportation - EMS.Classes - Familiarity.with.America
                                        - Language - Home.Phone - District - Citizenship.Class - Religious.Attendance
                                        - Fire.Alarm - Public.Computer - Car.Share - Religion - Hypertension - Transportation..Medical.
                                        - Online.Communities - Preference - City.Residency - City.Effort.Satisfaction
                                        - Parent_LivingWith - Full_Time_Employment_Status - Identify.Ethnically - Unmet.Health.Need
                                        - Unmet.Dental.Needs - Comunication.Problem - users - Acquaintances - Recovery - Treatment
                                        - Counseling - Diagnosed - Have.an.Advanced.Directive - Small.Businesses
                                        - Homemaker_Status - Children_LivingWith - Belonging - Smoking - Dentist.Check.up - X3.1.1
                                        - Ethnicity - Income - Close.Friend - Superstition - English.Speaking - Place.to.Live.Combined.Score
                                        - Walking - Compost - Bicycling - Carpooling - Personal.Car - Disappointment
                                        - Education.Completed - Diabetes - Activities - English.Classes - Small.Business
                                        - Public.Meeting - House_Ownership - Shame - Household.Size - Healthy.Diet - Quality_of_city_life
                                        - Age - Literature - Folkmedicine
                                        , data = df_for_modeling)

lipsitz.test(ordered_logit_model_update_polr)
brant(ordered_logit_model_update_polr)

# coef_table_logit <- coef(summary(ordered_logit_model))
# p_values <- pnorm(abs(coef_table_logit[, "t value"]), lower.tail = FALSE) * 2
# coef_table_logit <- cbind(coef_table_logit, "p value" = p_values)
# print(coef_table_logit)

ordered_logit_model_update <- update(ordered_logit_model, . ~ . - Smoke.Detector, data = df_for_modeling)
ordered_logit_model_update <- update(ordered_logit_model_update, . ~ . - Ethnicity.Combined)
ordered_logit_model_update <- update(ordered_logit_model_update, . ~ . - Traffic)
ordered_logit_model_update <- update(ordered_logit_model_update, . ~ . - City.Effort.Satisfaction)
ordered_logit_model_update <- update(ordered_logit_model_update, . ~ . - Religion)
ordered_logit_model_update <- update(ordered_logit_model_update, . ~ . - Small.Businesses)
ordered_logit_model_update <- update(ordered_logit_model_update, . ~ . - Marital.Status)
ordered_logit_model_update <- update(ordered_logit_model_update, . ~ . - Religious.Importance)
ordered_logit_model_update <- update(ordered_logit_model_update, . ~ . - Religious.Attendance)
ordered_logit_model_update <- update(ordered_logit_model_update, . ~ . - Education.Completed)
ordered_logit_model_update <- update(ordered_logit_model_update, . ~ . - Place.to.Live.Combined.Score)
ordered_logit_model_update <- update(ordered_logit_model_update, . ~ . - users)
ordered_logit_model_update <- update(ordered_logit_model_update, . ~ . - Informed)
ordered_logit_model_update <- update(ordered_logit_model_update, . ~ . - Smoking)
ordered_logit_model_update <- update(ordered_logit_model_update, . ~ . - Knowledge)
ordered_logit_model_update <- update(ordered_logit_model_update, . ~ . - Household.Size)
ordered_logit_model_update <- update(ordered_logit_model_update, . ~ . - Belonging)
ordered_logit_model_update <- update(ordered_logit_model_update, . ~ . - City.Residency)
ordered_logit_model_update <- update(ordered_logit_model_update, . ~ . - Identify.Ethnically)
ordered_logit_model_update <- update(ordered_logit_model_update, . ~ . - Healthy.Diet)
ordered_logit_model_update <- update(ordered_logit_model_update, . ~ . - Activities)
ordered_logit_model_update <- update(ordered_logit_model_update, . ~ . - Commuinity.Combined.Score)
ordered_logit_model_update <- update(ordered_logit_model_update, . ~ . - English.Difficulties)

summary(ordered_logit_model_update)

coef_table_logit_update <- coef(summary(ordered_logit_model_update))
p_values <- pnorm(abs(coef_table_logit_update[, "t value"]), lower.tail = FALSE) * 2
coef_table_logit_update <- cbind(coef_table_logit_update, "p value" = p_values)
print(coef_table_logit_update)



exp(coef(ordered_logit_model))

ordered_probit_model <- polr(Quality.of.Life ~ ., data = df_for_modeling, Hess = TRUE, method = "probit")

summary(ordered_probit_model)
coeftest(ordered_probit_model)

coef_table_probit <- coef(summary(ordered_probit_model))
p_values <- pnorm(abs(coef_table_probit[, "t value"]), lower.tail = FALSE) * 2
coef_table_probit <- cbind(coef_table_probit, "p value" = p_values)
print(coef_table_probit)

ordered_probit_model_update <- update(ordered_logit_model, . ~ . - Housing.Combined)
ordered_probit_model_update <- update(ordered_probit_model_update, . ~ . - Ethnicity.Combined)
ordered_probit_model_update <- update(ordered_probit_model_update, . ~ . - Traffic)
ordered_probit_model_update <- update(ordered_probit_model_update, . ~ . - City.Effort.Satisfaction)
ordered_probit_model_update <- update(ordered_probit_model_update, . ~ . - Religion)
ordered_probit_model_update <- update(ordered_probit_model_update, . ~ . - Small.Businesses)
ordered_probit_model_update <- update(ordered_probit_model_update, . ~ . - Marital.Status)
ordered_probit_model_update <- update(ordered_probit_model_update, . ~ . - Religious.Importance)
ordered_probit_model_update <- update(ordered_probit_model_update, . ~ . - Religious.Attendance)
ordered_probit_model_update <- update(ordered_probit_model_update, . ~ . - Education.Completed)
ordered_probit_model_update <- update(ordered_probit_model_update, . ~ . - Place.to.Live.Combined.Score)
ordered_probit_model_update <- update(ordered_probit_model_update, . ~ . - users)
ordered_probit_model_update <- update(ordered_probit_model_update, . ~ . - Informed)
ordered_probit_model_update <- update(ordered_probit_model_update, . ~ . - Smoking)
ordered_probit_model_update <- update(ordered_probit_model_update, . ~ . - Knowledge)
ordered_probit_model_update <- update(ordered_probit_model_update, . ~ . - Household.Size)
ordered_probit_model_update <- update(ordered_probit_model_update, . ~ . - Belonging)
ordered_probit_model_update <- update(ordered_probit_model_update, . ~ . - City.Residency)
ordered_probit_model_update <- update(ordered_probit_model_update, . ~ . - Identify.Ethnically)
ordered_probit_model_update <- update(ordered_probit_model_update, . ~ . - Healthy.Diet)
ordered_probit_model_update <- update(ordered_probit_model_update, . ~ . - Activities)
ordered_probit_model_update <- update(ordered_probit_model_update, . ~ . - Commuinity.Combined.Score)
ordered_probit_model_update <- update(ordered_probit_model_update, . ~ . - English.Difficulties)

summary(ordered_probit_model_update)

coef_table_probit_update <- coef(summary(ordered_probit_model_update))
p_values <- pnorm(abs(coef_table_probit_update[, "t value"]), lower.tail = FALSE) * 2
coef_table_probit_update <- cbind(coef_table_probit_update, "p value" = p_values)
print(coef_table_probit_update)

# We change all to numeric to start LPM
df_for_lpm <- lapply(df_for_modeling, function(x) {
  if (is.factor(x) || is.character(x)) {
    as.numeric(factor(x))  # encode categories as 1, 2, 3...
  } else {
    x
  }
})
df_for_lpm <- as.data.frame(df_for_lpm)

lpm_model <- lm(Quality.of.Life ~ Age + Gender + Income + Achieving.Ends.Meet + US.Residency + 
                  + Primary.Language + English.Speaking + Familiarity.with.America + Familiarity.with.Ethnic.Origin + 
                  + Discrimination + Regular.Exercise + Satisfaction + Superstition + Place.to.Work + Quality_of_city_life + 
                  + Satisfaction.With.Housing + Health.Combined.Score + Life.Satisfaction.Combined.Score, data = df_for_lpm)
summary(lpm_model)

lpm_model <- lm(Quality.of.Life ~ . - Smoke.Detector - Traffic - Aware.of.AARC - Knowledge
                                        - Weakness - Email - Mobile.Apps - Preferance - Primary.Care - Familiarity.with.Ethnic.Origin
                                        - Student - Sibling_LivingWith - Religious.Importance - Marital.Status - Alone
                                        - Spouse_LivingWith - Retired - English.Difficulties - Physical.Check.up
                                        - Dental.Insurance - Informed - Housing - Focus.Group - City.Election - Contact.City.Official
                                        - Recycle - Satisfaction.With.Housing - APD.Languages - Library.Internet.Acess
                                        - X9.1.1 - Disclosure - Antidepressants - Danger - Social.Networks - Health.Website - Family
                                        - Urgentcare - Public.Transportation - EMS.Classes - Familiarity.with.America
                                        - Language - Home.Phone - District - Citizenship.Class - Religious.Attendance
                                        - Fire.Alarm - Public.Computer - Car.Share - Religion - Hypertension - Transportation..Medical.
                                        - Online.Communities - Preference - City.Residency - City.Effort.Satisfaction
                                        - Parent_LivingWith - Full_Time_Employment_Status - Identify.Ethnically - Unmet.Health.Need
                                        - Unmet.Dental.Needs - Comunication.Problem - users - Acquaintances - Recovery - Treatment
                                        - Counseling - Diagnosed - Have.an.Advanced.Directive - Small.Businesses
                                        - Homemaker_Status - Children_LivingWith - Belonging - Smoking - Dentist.Check.up - X3.1.1
                                        - Ethnicity - Income - Close.Friend - Superstition - English.Speaking - Place.to.Live.Combined.Score
                                        - Walking - Compost - Bicycling - Carpooling - Personal.Car - Disappointment
                                        - Education.Completed - Diabetes - Activities - English.Classes - Small.Business
                                        - Public.Meeting - House_Ownership - Shame - Household.Size - Healthy.Diet - Quality_of_city_life
                                        - Age - Literature - Folkmedicine
                                        , data = df_for_lpm)


# TESTS
library(marginaleffects)

mfx <- marginaleffects(model_ologit)
summary(mfx)

# Do it 3 times for every model!!!
df_for_modeling$hat <- predict(ordered_logit_model_update, type = "link")  # logit scale
df_for_modeling$hat_sq <- df_for_modeling$hat^2

# Fit linktest regression
linktest_model <- glm(Quality.of.Life ~ hat + hat_sq, data = df_for_modeling, family = binomial)
summary(linktest_model)

install.packages("ResourceSelection")
library(ResourceSelection)

# HL test (g = number of groups, usually 10)
hl_logit <- hoslem.test(ordered_logit_model_update$Quality.of.Life, fitted(ordered_logit_model_update), g = 10)
hl_logit
hl_probit <- hoslem.test(ordered_probit_model_update$Quality.of.Life, fitted(ordered_probit_model_update), g = 10)
hl_robit
hl_lpm <- hoslem.test(lpm_model$Quality.of.Life, fitted(lpm_model), g = 10)
hl_lpm

install.packages("ordLGT")
library(ordLGT)

# Example ordinal logistic regression using VGAM
library(VGAM)
model_ologit <- vglm(Quality.of.Life ~ Age + Gender + Income + Achieving.Ends.Meet + US.Residency + 
                       + Primary.Language + English.Speaking + Familiarity.with.America + Familiarity.with.Ethnic.Origin + 
                       + Discrimination + Regular.Exercise + Satisfaction + Superstition + Place.to.Work + Quality_of_city_life + 
                       + Satisfaction.With.Housing + Health.Combined.Score + Life.Satisfaction.Combined.Score, family = cumulative(parallel = TRUE), data = df_for_lpm)

# Lipsitz test
lipsitz.test(model_ologit)

pulkrob.test(model_ologit)




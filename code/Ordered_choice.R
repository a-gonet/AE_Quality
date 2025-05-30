library(dplyr)
library(MASS)
library(lmtest)

options(scipen = 20)

load("processed_data/data_transformed.RData")

summary(data_transformed)

print(class(data_transformed$Quality.of.Life))

ologit2 = polr(Quality.of.Life~Age+Gender+Ethnicity+Marital.Status+Education.Completed+Household.Size+Religion+Income+Achieving.Ends.Meet
               +US_Born_Status+US.Residency+English.Speaking+Familiarity.with.America+Familiarity.with.Ethnic.Origin+Discrimination, 
               data=data_transformed)

summary(ologit2)
coeftest(ologit2)

library(dplyr)
library(MASS)
library(lmtest)
library(mlogit)

load("processed_data/df_for_modeling.RData")

summary(df_for_modeling)

print((df_for_modeling$Quality.of.Life))

model <- MASS::polr(Quality.of.Life ~ ., data = df_for_modeling, Hess = TRUE)

summary(model)

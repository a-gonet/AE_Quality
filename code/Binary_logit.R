library(stats)
library(dplyr)

load("processed_data/df_onehot.RData")

df_onehot[] <- lapply(df_onehot, function(x) {
  if (is.factor(x) || is.character(x)) {
    as.numeric(as.character(x))
  } else {
    x
  }
})


general_to_specific <- function(base_model, data, type = "logit") {
  current_model <- base_model
  
  repeat {
    coefs <- summary(current_model)$coefficients
    coefs_no_intercept <- coefs[rownames(coefs) != "(Intercept)", , drop = FALSE]
    
    max_p <- max(coefs_no_intercept[, 4], na.rm = TRUE)
    max_var <- rownames(coefs_no_intercept)[which.max(coefs_no_intercept[, 4])]
    
    cat("Sprawdzam zmienną:", max_var, "z p-wartością =", round(max_p, 5), "\n")
    
    # Jeśli wszystkie p-wartości < 0.05 → STOP
    if (max_p < 0.05) {
      cat("\n---\nModel końcowy:\n")
      print(summary(current_model))
      cat("\n---\nTest Likelihood Ratio vs model zerowy:\n")
      print(lrtest(current_model, base_model))
      break
    }
    
    # Usuń zmienną z najwyższym p
    new_formula <- update(formula(current_model), paste(". ~ . -", max_var))
    new_model <- glm(formula = new_formula, data = data, family = binomial(link = type))
    
    # Test czy usunięcie zmiennej istotnie pogarsza model
    lr <- lrtest(current_model, new_model)
    p_lr <- lr$`Pr(>Chisq)`[2]
    
    cat("LR test p-wartość po usunięciu zmiennej:", round(p_lr, 5), "\n---\n")
    
    if (p_lr > 0.05) {
      # Usunięcie zmiennej jest ok, kontynuuj
      current_model <- new_model
    } else {
      cat("Usunięcie zmiennej", max_var, "istotnie pogarsza model (p =", round(p_lr, 5), ")\n")
      cat("\n---\nModel końcowy:\n")
      print(summary(current_model))
      cat("\n---\nTest Likelihood Ratio vs model zerowy:\n")
      print(lrtest(base_model, current_model))
      break
    }
  }
  
  return(current_model)
}


base_logit <- glm(badQuality ~ (Age + Gender + Education.Completed + Household.Size + 
                                  Friends_LivingWith + Full_Time_Employment_Status + Part_Time_Employment_Status + 
                                  Homemaker_Status + Achieving.Ends.Meet + US_Born_Status + 
                                  US.Residency + Discrimination + Smoking + Regular.Exercise + 
                                  Healthy.Diet + Hypertension + Diabetes + Arthritis + Urgentcare + 
                                  Health.Insurance + House_Ownership + Nursing.Home + Smoke.Detector + 
                                  Recycle + Compost + Public.Transportation + Bicycling + Carpooling + 
                                  Personal.Car + Car.Share + Walking + Home.Phone + Health.Combined.Score + 
                                  Relatives.Support.Score + Religiousness.Score + `Ethnicity_Asian Indian` + 
                                  Ethnicity_Chinese + Ethnicity_Filipino + Ethnicity_Korean + 
                                  Ethnicity_Vietnamese + isPartnered + GoodEnglish + EthnicFamiliar + 
                                  DissatisfiedLife + SatisfiedLife + HousingSatisfaction + 
                                  highIncome + lowIncome + US_Live_percent) ,
                  data = df_onehot, family = binomial(link="logit"))

base_probit <- glm(badQuality ~ (Age + Gender + Education.Completed + Household.Size + 
                                        Friends_LivingWith + Full_Time_Employment_Status + Part_Time_Employment_Status + 
                                        Homemaker_Status + Achieving.Ends.Meet + US_Born_Status + 
                                        US.Residency + Discrimination + Smoking + Regular.Exercise + 
                                        Healthy.Diet + Hypertension + Diabetes + Arthritis + Urgentcare + 
                                        Health.Insurance + House_Ownership + Nursing.Home + Smoke.Detector + 
                                        Recycle + Compost + Public.Transportation + Bicycling + Carpooling + 
                                        Personal.Car + Car.Share + Walking + Home.Phone + Health.Combined.Score + 
                                        Relatives.Support.Score + Religiousness.Score + `Ethnicity_Asian Indian` + 
                                        Ethnicity_Chinese + Ethnicity_Filipino + Ethnicity_Korean + 
                                        Ethnicity_Vietnamese + isPartnered + GoodEnglish + EthnicFamiliar + 
                                        DissatisfiedLife + SatisfiedLife + HousingSatisfaction + 
                                        highIncome + lowIncome + US_Live_percent) ,
                  data = df_onehot, family = binomial(link="probit"))

base_logit_exc <- glm(excellentQuality ~ (Age + Gender + Education.Completed + Household.Size + 
                                        Friends_LivingWith + Full_Time_Employment_Status + Part_Time_Employment_Status + 
                                        Homemaker_Status + Achieving.Ends.Meet + US_Born_Status + 
                                        US.Residency + Discrimination + Smoking + Regular.Exercise + 
                                        Healthy.Diet + Hypertension + Diabetes + Arthritis + Urgentcare + 
                                        Health.Insurance + House_Ownership + Nursing.Home + Smoke.Detector + 
                                        Recycle + Compost + Public.Transportation + Bicycling + Carpooling + 
                                        Personal.Car + Car.Share + Walking + Home.Phone + Health.Combined.Score + 
                                        Relatives.Support.Score + Religiousness.Score + `Ethnicity_Asian Indian` + 
                                        Ethnicity_Chinese + Ethnicity_Filipino + Ethnicity_Korean + 
                                        Ethnicity_Vietnamese + isPartnered + GoodEnglish + EthnicFamiliar + 
                                        DissatisfiedLife + SatisfiedLife + HousingSatisfaction + 
                                        highIncome + lowIncome + US_Live_percent) ,
                  data = df_onehot, family = binomial(link="logit"))

base_probit_exc <- glm(excellentQuality ~ (Age + Gender + Education.Completed + Household.Size + 
                                        Friends_LivingWith + Full_Time_Employment_Status + Part_Time_Employment_Status + 
                                        Homemaker_Status + Achieving.Ends.Meet + US_Born_Status + 
                                        US.Residency + Discrimination + Smoking + Regular.Exercise + 
                                        Healthy.Diet + Hypertension + Diabetes + Arthritis + Urgentcare + 
                                        Health.Insurance + House_Ownership + Nursing.Home + Smoke.Detector + 
                                        Recycle + Compost + Public.Transportation + Bicycling + Carpooling + 
                                        Personal.Car + Car.Share + Walking + Home.Phone + Health.Combined.Score + 
                                        Relatives.Support.Score + Religiousness.Score + `Ethnicity_Asian Indian` + 
                                        Ethnicity_Chinese + Ethnicity_Filipino + Ethnicity_Korean + 
                                        Ethnicity_Vietnamese + isPartnered + GoodEnglish + EthnicFamiliar + 
                                        DissatisfiedLife + SatisfiedLife + HousingSatisfaction + 
                                        highIncome + lowIncome + US_Live_percent) ,
                  data = df_onehot, family = binomial(link="probit"))

final_model_logit <- general_to_specific(base_logit, df_onehot, "logit")
final_model_probit <- general_to_specific(base_probit, df_onehot, "probit")

final_model_logit_exc <- general_to_specific(base_logit_exc, df_onehot, "logit")
final_model_probit_exc <- general_to_specific(base_probit_exc, df_onehot, "probit")

summary(final_model_logit)
summary(final_model_probit)
summary(final_model_logit_exc)
summary(final_model_probit_exc)

library(stargazer)
stargazer(final_model_logit, final_model_probit, type = "text", dep.var.labels.include = FALSE,
          column.labels = c("badQuality", "badQuality"))

lrtest(final_model_logit, final_model_probit)

stargazer(final_model_logit_exc, final_model_probit_exc, type = "text", dep.var.labels.include = FALSE,
          column.labels = c("excellentQuality", "excellentQuality"))

lrtest(final_model_logit_exc, final_model_probit_exc)


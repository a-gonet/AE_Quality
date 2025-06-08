library(stats)
library(dplyr)
library(mfx)

# WNE package from Github
install.packages("devtools")
library("devtools")

install_github("Rand-0/WNE")
library(WNE)

#setwd("C:\\Users\\Adam\\Documents\\AE_Quality")

load("processed_data/df_onehot.RData")

# df_onehot[] <- lapply(df_onehot, function(x) {
#   if (is.factor(x) || is.character(x)) {
#     as.numeric(as.character(x))
#   } else {
#     x
#   }
# })

df_onehot$Ethnicity_Asian_Indian <- df_onehot$`Ethnicity_Asian Indian`
df_onehot$`Ethnicity_Asian Indian` <- NULL

df_onehot$badQuality <- as.numeric(df_onehot$badQuality) -1
df_onehot$excellentQuality <- as.numeric(df_onehot$excellentQuality) -1

general_to_specific <- function(base_model, data, type = "logit") {
  current_model <- base_model
  
  repeat {
    coefs <- summary(current_model)$coefficients
    coefs_no_intercept <- coefs[rownames(coefs) != "(Intercept)", , drop = FALSE]
    
    max_p <- max(coefs_no_intercept[, 4], na.rm = TRUE)
    max_coef <- rownames(coefs_no_intercept)[which.max(coefs_no_intercept[, 4])]
    
    cat("Sprawdzam współczynnik:", max_coef, "z p-wartością =", round(max_p, 5), "\n")
    
    if (max_p < 0.05) {
      cat("\n---\nModel końcowy:\n")
      print(summary(current_model))
      cat("\n---\nTest LR vs model zerowy:\n")
      print(lrtest(current_model, base_model))
      break
    }
    
    # Sprawdź, czy jest w I(...)
    if (grepl("^I\\(", max_coef)) {
      # usuń tylko ten współczynnik
      new_formula <- update(formula(current_model), paste(". ~ . -", max_coef))
      cat("Usuwam pojedynczy współczynnik (I(...)):", max_coef, "\n")
    } else {
      # wyciągnij nazwę zmiennej bazowej (np. Walking1 -> Walking)
      # lub po prostu spróbuj odczytać prefix zmiennej (np. Walking1 bez cyfry na końcu)
      # Spróbuj najpierw wziąć pierwszą "czystą" nazwę (do cyfry lub do kropki)
      
      # Alternatywnie wyciągnij ze wzoru term labels:
      term_labels <- attr(terms(current_model), "term.labels")
      
      # Funkcja pomocnicza: czy dana zmienna występuje w term_labels?
      base_var <- NA
      for (v in term_labels) {
        # Sprawdź, czy max_coef jest podciągiem v lub odwrotnie
        if (grepl(max_coef, v) || grepl(v, max_coef)) {
          base_var <- v
          break
        }
      }
      
      # Jeśli nie znaleziono, to jako fallback usuwamy max_coef
      if (is.na(base_var)) base_var <- max_coef
      
      # Sprawdź typ zmiennej w danych
      # Uwaga: data musi mieć kolumny bez transformacji np. I(...)
      # dlatego próbujemy uprościć base_var
      base_var_clean <- gsub("I\\((.*)\\)", "\\1", base_var)
      base_var_clean <- gsub("\\^2", "", base_var_clean)
      
      # Jeśli base_var_clean nie jest kolumną, to fallback
      if (!(base_var_clean %in% colnames(data))) {
        base_var_clean <- base_var
      }
      
      if (base_var_clean %in% colnames(data)) {
        var_class <- class(data[[base_var_clean]])
      } else {
        var_class <- NA
      }
      
      if (!is.na(var_class) && "factor" %in% var_class) {
        # jeśli factor, to usuń cały factor
        new_formula <- update(formula(current_model), paste(". ~ . -", base_var_clean))
        cat("Usuwam cały factor:", base_var_clean, "\n")
      } else {
        # numeric lub brak informacji - usuwamy pojedynczą zmienną (potęgę lub samą zmienną)
        new_formula <- update(formula(current_model), paste(". ~ . -", max_coef))
        cat("Usuwam pojedynczą zmienną:", max_coef, "\n")
      }
    }
    
    new_model <- glm(formula = new_formula, data = data, family = binomial(link = type))
    lr <- lrtest(current_model, new_model)
    p_lr <- lr$`Pr(>Chisq)`[2]
    
    cat("LR test p-wartość po usunięciu:", round(p_lr, 5), "\n---\n")
    
    if (p_lr > 0.05) {
      current_model <- new_model
    } else {
      cat("Usunięcie istotnie pogarsza model (p =", round(p_lr, 5), ")\n")
      cat("\n---\nModel końcowy:\n")
      print(summary(current_model))
      cat("\n---\nTest LR vs model zerowy:\n")
      print(lrtest(base_model, current_model))
      break
    }
  }
  
  return(current_model)
}


# from execises files
# Osius-Rojek test
# Based on description in Hosmer and Lemeshow (2000) p. 153.
# Assumes data are aggregated into Explanatory Variable Pattern form.

o.r.test = function(obj) {
  # first, check to see if we fed in the right kind of object
  # stopifnot(family(obj)$family == "binomial" && family(obj)$link == "logit")
  mf <- obj$model
  trials = rep(1, times = nrow(mf))
  if(any(colnames(mf) == "(weights)")) 
    trials <- mf[[ncol(mf)]]
  prop = mf[[1]]
  # the double bracket (above) gets the index of items within an object
  if (is.factor(prop)) 
    prop = as.numeric(prop) == 2  # Converts 1-2 factor levels to logical 0/1 values
  pi.hat = obj$fitted.values 
  y <- trials*prop
  yhat <- trials*pi.hat
  nu <- yhat*(1-pi.hat)
  pearson <- sum((y - yhat)^2/nu)
  c = (1 - 2*pi.hat)/nu
  exclude <- c(1,which(colnames(mf) == "(weights)"))
  vars <- data.frame(c,mf[,-exclude]) 
  wlr <- lm(formula = c ~ ., weights = nu, data = vars)
  rss <- sum(nu*residuals(wlr)^2 )
  J <- nrow(mf)
  A <- 2*(J - sum(1/trials))
  z <- (pearson - (J - ncol(vars) - 1))/sqrt(A + rss)
  p.value <- 2*(1 - pnorm(abs(z)))
  cat("z = ", z, "with p-value = ", p.value, "\n")
}


base_logit_lin <- glm(badQuality ~ Age +  Gender + Education.Completed  + Household.Size + 
                                  Friends_LivingWith + Full_Time_Employment_Status + Part_Time_Employment_Status + 
                                  Homemaker_Status + Achieving.Ends.Meet + US_Born_Status + 
                                  US.Residency  + Discrimination + Smoking + Regular.Exercise + 
                                  Healthy.Diet + Hypertension + Diabetes + Arthritis + Urgentcare + 
                                  Health.Insurance + House_Ownership + Nursing.Home + Smoke.Detector + 
                                  Recycle + Compost + Public.Transportation + Bicycling + Carpooling + 
                                  Personal.Car + Car.Share + Walking + Home.Phone + Health.Combined.Score  + 
                                  Relatives.Support.Score  + Religiousness.Score  + Ethnicity_Asian_Indian +
                                  Ethnicity_Chinese + Ethnicity_Filipino + Ethnicity_Korean + 
                                  Ethnicity_Vietnamese + isPartnered + GoodEnglish + EthnicFamiliar + 
                                  DissatisfiedLife + SatisfiedLife + HousingSatisfaction + 
                                  highIncome + lowIncome + US_Live_percent, 
                                  data = df_onehot, family = binomial(link="logit"))

summary(base_logit_lin)

# Linktest
# source("linktest.R")
linktest_result = WNE::linktest(base_logit_lin)
# square interactions needed

final_logit_linear <- general_to_specific(base_logit_lin, df_onehot, "logit")

linktest_result0 = WNE::linktest(final_logit_linear)
print(linktest_result0)
# square interactions needed


base_logit <- glm(badQuality ~ (Age + I(Age^2) + Gender + Education.Completed + I(Education.Completed^2) + Household.Size + I(Household.Size^2) +
                                  Friends_LivingWith + Full_Time_Employment_Status + Part_Time_Employment_Status + 
                                  Homemaker_Status + Achieving.Ends.Meet + US_Born_Status + 
                                  US.Residency + I(US.Residency^2) + Discrimination + Smoking + Regular.Exercise + 
                                  Healthy.Diet + Hypertension + Diabetes + Arthritis + Urgentcare + 
                                  Health.Insurance + House_Ownership + Nursing.Home + Smoke.Detector + 
                                  Recycle + Compost + Public.Transportation + Bicycling + Carpooling + 
                                  Personal.Car + Car.Share + Walking + Home.Phone + Health.Combined.Score + I(Health.Combined.Score^2) + 
                                  Relatives.Support.Score + I(Relatives.Support.Score^2) + Religiousness.Score + I(Religiousness.Score^2) + 
                                  Ethnicity_Asian_Indian +
                                  Ethnicity_Chinese + Ethnicity_Filipino + Ethnicity_Korean + 
                                  Ethnicity_Vietnamese + isPartnered + GoodEnglish + EthnicFamiliar + 
                                  DissatisfiedLife + SatisfiedLife + HousingSatisfaction + 
                                  highIncome + lowIncome + US_Live_percent +I(US_Live_percent^2)) ,
                  data = df_onehot, family = binomial(link="logit"))

base_probit <- glm(badQuality ~ (Age + I(Age^2) + Gender + Education.Completed + I(Education.Completed^2) + Household.Size + I(Household.Size^2) +
                                   Friends_LivingWith + Full_Time_Employment_Status + Part_Time_Employment_Status + 
                                   Homemaker_Status + Achieving.Ends.Meet + US_Born_Status + 
                                   US.Residency + I(US.Residency^2) + Discrimination + Smoking + Regular.Exercise + 
                                   Healthy.Diet + Hypertension + Diabetes + Arthritis + Urgentcare + 
                                   Health.Insurance + House_Ownership + Nursing.Home + Smoke.Detector + 
                                   Recycle + Compost + Public.Transportation + Bicycling + Carpooling + 
                                   Personal.Car + Car.Share + Walking + Home.Phone + Health.Combined.Score + I(Health.Combined.Score^2) + 
                                   Relatives.Support.Score + I(Relatives.Support.Score^2) + Religiousness.Score + I(Religiousness.Score^2) + 
                                   Ethnicity_Asian_Indian +
                                   Ethnicity_Chinese + Ethnicity_Filipino + Ethnicity_Korean + 
                                   Ethnicity_Vietnamese + isPartnered + GoodEnglish + EthnicFamiliar + 
                                   DissatisfiedLife + SatisfiedLife + HousingSatisfaction + 
                                   highIncome + lowIncome + US_Live_percent +I(US_Live_percent^2)) ,
                  data = df_onehot, family = binomial(link="probit"))

base_logit_exc <- glm(excellentQuality ~ (Age + I(Age^2) + Gender + Education.Completed + I(Education.Completed^2) + Household.Size + I(Household.Size^2) +
                                      Friends_LivingWith + Full_Time_Employment_Status + Part_Time_Employment_Status + 
                                      Homemaker_Status + Achieving.Ends.Meet + US_Born_Status + 
                                      US.Residency + I(US.Residency^2) + Discrimination + Smoking + Regular.Exercise + 
                                      Healthy.Diet + Hypertension + Diabetes + Arthritis + Urgentcare + 
                                      Health.Insurance + House_Ownership + Nursing.Home + Smoke.Detector + 
                                      Recycle + Compost + Public.Transportation + Bicycling + Carpooling + 
                                      Personal.Car + Car.Share + Walking + Home.Phone + Health.Combined.Score + I(Health.Combined.Score^2) + 
                                      Relatives.Support.Score + I(Relatives.Support.Score^2) + Religiousness.Score + I(Religiousness.Score^2) + 
                                      Ethnicity_Asian_Indian +
                                      Ethnicity_Chinese + Ethnicity_Filipino + Ethnicity_Korean + 
                                      Ethnicity_Vietnamese + isPartnered + GoodEnglish + EthnicFamiliar + 
                                      DissatisfiedLife + SatisfiedLife + HousingSatisfaction + 
                                      highIncome + lowIncome + US_Live_percent +I(US_Live_percent^2)) ,
                  data = df_onehot, family = binomial(link="logit"))

base_probit_exc <- glm(excellentQuality ~ (Age + I(Age^2) + Gender + Education.Completed + I(Education.Completed^2) + Household.Size + I(Household.Size^2) +
                                       Friends_LivingWith + Full_Time_Employment_Status + Part_Time_Employment_Status + 
                                       Homemaker_Status + Achieving.Ends.Meet + US_Born_Status + 
                                       US.Residency + I(US.Residency^2) + Discrimination + Smoking + Regular.Exercise + 
                                       Healthy.Diet + Hypertension + Diabetes + Arthritis + Urgentcare + 
                                       Health.Insurance + House_Ownership + Nursing.Home + Smoke.Detector + 
                                       Recycle + Compost + Public.Transportation + Bicycling + Carpooling + 
                                       Personal.Car + Car.Share + Walking + Home.Phone + Health.Combined.Score + I(Health.Combined.Score^2) + 
                                       Relatives.Support.Score + I(Relatives.Support.Score^2) + Religiousness.Score + I(Religiousness.Score^2) + 
                                       Ethnicity_Asian_Indian +
                                       Ethnicity_Chinese + Ethnicity_Filipino + Ethnicity_Korean + 
                                       Ethnicity_Vietnamese + isPartnered + GoodEnglish + EthnicFamiliar + 
                                       DissatisfiedLife + SatisfiedLife + HousingSatisfaction + 
                                       highIncome + lowIncome + US_Live_percent +I(US_Live_percent^2)) ,
                  data = df_onehot, family = binomial(link="probit"))


final_model_logit <- general_to_specific(base_logit, df_onehot, "logit")
final_model_probit <- general_to_specific(base_probit, df_onehot, "probit")

final_model_logit_exc <- general_to_specific(base_logit_exc, df_onehot, "logit")
final_model_probit_exc <- general_to_specific(base_probit_exc, df_onehot, "probit")

summary(final_model_logit)
PseudoR2(final_model_logit, c("McFadden", "McKelveyZavoina"))

summary(final_model_probit)
PseudoR2(final_model_probit, c("McFadden", "McKelveyZavoina"))

summary(final_model_logit_exc)
PseudoR2(final_model_logit_exc, c("McFadden", "McKelveyZavoina"))

summary(final_model_probit_exc)
PseudoR2(final_model_probit_exc, c("McFadden", "McKelveyZavoina"))



library(stargazer)
stargazer(final_model_logit, final_model_probit, type = "text", dep.var.labels.include = FALSE,
          column.labels = c("badQuality", "badQuality"))

lrtest(final_model_logit, final_model_probit)

stargazer(final_model_logit_exc, final_model_probit_exc, type = "text", dep.var.labels.include = FALSE,
          column.labels = c("excellentQuality", "excellentQuality"))

lrtest(final_model_logit_exc, final_model_probit_exc)


#final summary?
stargazer(final_model_probit, final_model_probit_exc, type = "text", dep.var.labels.include = FALSE,
          column.labels = c("badQuality", "excellentQuality"))


linktest_result1 = WNE::linktest(final_model_probit)
print(linktest_result1)
# no more other square interactions needed

linktest_result2 = WNE::linktest(final_model_probit_exc)
print(linktest_result2)
# no more other square interactions needed

library(ResourceSelection)
hoslem.test(df_onehot$badQuality, fitted(final_model_probit)) #H0 -> appropriate model
o.r.test(final_model_probit)#H0 -> appropriate model

hoslem.test(df_onehot$badQuality, fitted(final_model_probit_exc)) #H0 -> appropriate model
o.r.test(final_model_probit_exc)#H0 -> appropriate model

meff1 = logitmfx(formula=final_model_logit$formula, data = df_onehot, atmean=TRUE)
print(meff1)

meff2 = probitmfx(formula=final_model_probit$formula, data = df_onehot, atmean=TRUE)
print(meff2)

meff3 = logitmfx(formula=final_model_logit_exc$formula, data = df_onehot, atmean=TRUE)
print(meff3)

meff4 = probitmfx(formula=final_model_probit_exc$formula, data = df_onehot, atmean=TRUE)
print(meff4)

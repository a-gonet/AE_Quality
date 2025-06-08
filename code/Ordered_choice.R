library(dplyr)
library(MASS)
library(lmtest)
library(oglmx)

options(scipen = 20)

load("processed_data/data_transformed.RData")

summary(data_transformed)

print(class(data_transformed$Quality.of.Life))

ologit2 = polr(Quality.of.Life~Age+Gender+Ethnicity+Marital.Status+Education.Completed+Household.Size+Religion+Income+Achieving.Ends.Meet
               +US_Born_Status+US.Residency+English.Speaking+Familiarity.with.America+Familiarity.with.Ethnic.Origin+Discrimination, 
               data=data_transformed)

summary(ologit2)
coeftest(ologit2)


ologit0 <- ologit.reg(Quality.of.Life ~ 1, data = df_onehot)
lrtest(ologit0, ordered_logit_model)

ordered_logit_model <- ologit.reg(Quality.of.Life ~ . -Income - See.Family -Close.Family - Helpful.Family
                                  - See.Friends - Close.Friends - Helpful.Friends, data = df_onehot)
summary(ordered_logit_model)

ologit_better <- ologit.reg(Quality.of.Life ~ . -Income - See.Family -Close.Family - Helpful.Family
                            - See.Friends - Close.Friends - Helpful.Friends
                            
                            - Spouse_LivingWith - Student - Drinking - Diabetes - Religious.Attendance 
                            - Recycle - Public.Transportation - Home.Phone
                            
                            -Language_Viet -isPartnered -Ethnicity_Korean -Smoke.Detector -Full_Time_Employment_Status
                            -Parent_LivingWith -Alone - Religion_Hindu -DissatisfiedLife
                            
                            -Retired - Hypertension -Urgentcare - Health.Insurance - Religion_Muslim -`Housing_Apartment/ Townhouse/ Condominium`
                            -`Housing_One-family house` -`Housing_Two-family house/ duplex`
                            
                            -Education.Completed -Smoking - Bicycling -Car.Share -Access.to.a.Computer -Ethnicity_Chinese
                            -Religion_Buddhist -Language_Tagalog -Religion_None -Religion_Protestant -Language_Hindi
                            
                            -Gender -Children_LivingWith -Homemaker_Status -Personal.Car -Language_Gujarati
                            
                            -Compost -Mobile.Devices -`Ethnicity_Asian Indian` -House_Ownership_Rent -ReligionImportance
                            
                            -Household.Size -EthnicFamiliar
                            
                            
                            
                            , data = df_onehot)

lrtest(ordered_logit_model, ologit_better)
summary(ologit_better)
brant(ologit_better)




ologit_better2 <- vglm(Quality.of.Life ~ . -Income - See.Family -Close.Family - Helpful.Family
                       - See.Friends - Close.Friends - Helpful.Friends
                       
                       - Spouse_LivingWith - Student - Drinking - Diabetes - Religious.Attendance 
                       - Recycle - Public.Transportation - Home.Phone
                       
                       -Language_Viet -isPartnered -Ethnicity_Korean -Smoke.Detector -Full_Time_Employment_Status
                       -Parent_LivingWith -Alone - Religion_Hindu -DissatisfiedLife
                       
                       -Retired - Hypertension -Urgentcare - Health.Insurance - Religion_Muslim -`Housing_Apartment/ Townhouse/ Condominium`
                       -`Housing_One-family house` -`Housing_Two-family house/ duplex`
                       
                       -Education.Completed -Smoking - Bicycling -Car.Share -Access.to.a.Computer -Ethnicity_Chinese
                       -Religion_Buddhist -Language_Tagalog -Religion_None -Religion_Protestant -Language_Hindi
                       
                       -Gender -Children_LivingWith -Homemaker_Status -Personal.Car -Language_Gujarati
                       
                       -Compost -Mobile.Devices -`Ethnicity_Asian Indian` -House_Ownership_Rent -ReligionImportance
                       
                       -Household.Size -EthnicFamiliar
                       , family = cumulative(parallel = FALSE), data = df_onehot)
summary(ologit_better2)
lrtest(ologit_better2, ologit_better)
brant(ologit_better2)

library(car)
vif(ologit_better2)

ologit_better3 <- polr(Quality.of.Life ~ Friends_LivingWith+Part_Time_Employment_Status+US.Residency
                       , data = df_onehot)

brant(ologit_better3)

all_cols <- colnames(df_onehot)

# 2. Zrób wektor zmiennych do usunięcia (bez myślników!)
exclude_cols <- c("Income", "See.Family", "Close.Family", "Helpful.Family",
                  "See.Friends", "Close.Friends", "Helpful.Friends",
                  "Spouse_LivingWith", "Student", "Drinking", "Diabetes", "Religious.Attendance",
                  "Recycle", "Public.Transportation", "Home.Phone",
                  "Language_Viet", "isPartnered", "Ethnicity_Korean", "Smoke.Detector", "Full_Time_Employment_Status",
                  "Parent_LivingWith", "Alone", "Religion_Hindu", "DissatisfiedLife",
                  "Retired", "Hypertension", "Urgentcare", "Health.Insurance", "Religion_Muslim",
                  "Housing_Apartment/ Townhouse/ Condominium", "Housing_One-family house", "Housing_Two-family house/ duplex",
                  "Education.Completed", "Smoking", "Bicycling", "Car.Share", "Access.to.a.Computer", "Ethnicity_Chinese",
                  "Religion_Buddhist", "Language_Tagalog", "Religion_None", "Religion_Protestant", "Language_Hindi",
                  "Gender", "Children_LivingWith", "Homemaker_Status", "Personal.Car", "Language_Gujarati",
                  "Compost", "Mobile.Devices", "Ethnicity_Asian Indian", "House_Ownership_Rent", "ReligionImportance",
                  "Household.Size", "EthnicFamiliar")

# 3. Usuń kolumny do wykluczenia
model_cols <- setdiff(all_cols, exclude_cols)

for(col in model_cols) {
  if (col == "Quality.of.Life") next
  
  # Pobierz kolumnę tymczasowo
  temp_col <- df_onehot[[col]]
  
  # Jeśli numeric i ma <10 unikalnych wartości, zamieniamy tylko lokalnie na factor
  if (is.numeric(temp_col) && length(unique(temp_col)) < 10) {
    temp_col <- as.factor(temp_col)
    cat("Temporarily converted numeric to factor:", col, "\n")
  }
  
  # Dla factor lub character wypisz tabelę
  if (is.factor(temp_col) || is.character(temp_col)) {
    cat("Checking column:", col, "\n")
    print(table(temp_col, df_onehot$Quality.of.Life))
    cat("\n---------------------------------\n")
  }
}


for(colname in names(model_cols)) {
  if (colname %in% names(df_onehot)) {
    df_onehot[[colname]] <- as.numeric(df_onehot[[colname]])
  }
}








ologit_better4 <- vglm(Quality.of.Life ~ Friends_LivingWith+US.Residency+Primary.Language
                       +Healthy.Diet+GoodHealth+SatisfiedLife+HousingSatisfaction
                       +highIncome
                       , family = cumulative(parallel = FALSE), data = df_onehot)
summary(ologit_better4)
lrtest(ologit_better2, ologit_better4)
brant(ologit_better2)

library(dplyr)

data <- read.csv("raw_data/Report_Quality_of_Life.csv")

data_clean <- data %>%
  mutate(across(everything(), ~ {
    x <- .x
    if (is.factor(x)) x <- as.character(x)
    x[x == ""] <- NA
    as.factor(x)  
  }))

data_clean$na_per_row <- apply(data_clean, 1, function(x) sum(is.na(x)))

data_filtered <- data_clean[data_clean$na_per_row <= 7 , ]

data_transformed <- data_filtered


# 1. Zmiana nazw kolumn (tak jak w oryginalnym kodzie)
data_transformed <- data_transformed %>%
  dplyr::rename(
    ID = Survey.ID,
    Alone = No.One,
    Spouse_LivingWith = Spouse,
    Children_LivingWith = Children,
    GrandChildren_LivingWith = Grand.Children,
    Parent_LivingWith = Parent,
    Friends_LivingWith = Friends,
    Full_Time_Employment_Status = Full.Time.Employment,
    Part_Time_Employment_Status = Part.Time.Employment,
    Homemaker_Status = Homemaker,
    US_Born_Status = US.Born,
    Quality_of_city_life = Qualtiy.of.Life, # Ta nazwa będzie używana dla skali porządkowej
    House_Ownership = Status.of.Ownership,
    Sibling_LivingWith = Brother.Sister,
    US.Residency = Duration.of.Residency,
    City.Residency = Residency
  )

# 2. Przekodowanie wartości na 0/1 (dla binarnych) i liczby porządkowe (dla skal)
#    Usuwamy bezpośrednie konwersje na as.factor/as.numeric w tym bloku.
data_transformed <- data_transformed %>%
  mutate(
    # --- Przekodowania binarne (Yes/No, Living with, Employed, etc. na 1/0) ---
    Alone = ifelse(Alone == "Living with no one", 1, 0), # Zmieniono z as.factor(ifelse(...))
    Spouse_LivingWith = ifelse(Spouse_LivingWith == "Living with spouse", 1, 0),
    Children_LivingWith = ifelse(Children_LivingWith == "Living with children", 1, 0),
    GrandChildren_LivingWith = ifelse(GrandChildren_LivingWith == "Living with grandchildren", 1, 0),
    Parent_LivingWith = ifelse(Parent_LivingWith == "Living with parents", 1, 0),
    Sibling_LivingWith = ifelse(Sibling_LivingWith == "Living with brothers/sisters", 1, 0),
    Friends_LivingWith = ifelse(Friends_LivingWith == "Living with friends/roommates", 1, 0),
    Full_Time_Employment_Status = ifelse(Full_Time_Employment_Status == "Employed full time", 1, 0),
    Part_Time_Employment_Status = ifelse(Part_Time_Employment_Status == "Employed part time", 1, 0),
    Homemaker_Status = ifelse(Homemaker_Status == "Full time homemaker", 1, 0),
    Student = ifelse(Student == "Student", 1, 0),
    Retired = ifelse(Retired == "Retired", 1, 0),
    US_Born_Status = case_when(
      US_Born_Status == "Yes" ~ 1,
      US_Born_Status == "No" ~ 0,
      TRUE ~ NA_real_
    ),
    House_Ownership = case_when(House_Ownership == "Own" ~ 1, House_Ownership == "Rent" ~ 0, TRUE ~ NA_real_),
    Hypertension = ifelse(Hypertension == "Yes", 1, 0),
    Diabetes = ifelse(Diabetes == "Yes", 1, 0),
    Arthritis = ifelse(Arthritis == "Yes", 1, 0),
    Physical.Check.up = ifelse(Physical.Check.up == "Yes", 1, 0),
    Dentist.Check.up = ifelse(Dentist.Check.up == "Yes", 1, 0),
    Urgentcare = ifelse(Urgentcare == "Yes", 1, 0),
    Folkmedicine = ifelse(Folkmedicine == "Yes", 1, 0),
    Primary.Care = ifelse(Primary.Care == "Yes", 1, 0),
    Health.Insurance = ifelse(Health.Insurance == "Yes", 1, 0),
    Dental.Insurance = ifelse(Dental.Insurance == "Yes", 1, 0),
    Unmet.Health.Need = ifelse(Unmet.Health.Need == "Yes", 1, 0),
    Unmet.Dental.Needs = ifelse(Unmet.Dental.Needs == "Yes", 1, 0),
    Transportation..Medical. = ifelse(Transportation..Medical. == "Yes", 1, 0),
    Interpretation..Medical. = ifelse(Interpretation..Medical. == "Yes", 1, 0),
    Comunication.Problem = ifelse(Comunication.Problem == "Yes", 1, 0),
    Preferance = ifelse(Preferance == "Yes", 1, 0), # Kolumna 56
    Family = case_when( # Kolumna 60
      Family == "Yes" ~ 1,
      Family == "No" ~ 0,
      TRUE ~ NA_real_
    ),
    Close.Friend = case_when(Close.Friend == "Yes" ~ 1, Close.Friend == "No" ~ 0, TRUE ~ NA_real_),
    Acquaintances = case_when(Acquaintances == "Yes" ~ 1, Acquaintances == "No" ~ 0, TRUE ~ NA_real_),
    Heal.Professionals = case_when(Heal.Professionals == "Yes" ~ 1, Heal.Professionals == "No" ~ 0, TRUE ~ NA_real_),
    Mobile.Apps = case_when(Mobile.Apps == "Yes" ~ 1, Mobile.Apps == "No" ~ 0, TRUE ~ NA_real_),
    Email = case_when(Email == "Yes" ~ 1, Email == "No" ~ 0, TRUE ~ NA_real_),
    Social.Networks = case_when(Social.Networks == "Yes" ~ 1, Social.Networks == "No" ~ 0, TRUE ~ NA_real_),
    Online.Communities = case_when(Online.Communities == "Yes" ~ 1, Online.Communities == "No" ~ 0, TRUE ~ NA_real_),
    Health.Website = case_when(Health.Website == "Yes" ~ 1, Health.Website == "No" ~ 0, TRUE ~ NA_real_),
    Weakness = ifelse(Weakness == "Yes", 1, 0),
    Shame = ifelse(Shame == "Yes", 1, 0),
    Disappointment = ifelse(Disappointment == "Yes", 1, 0),
    Disclosure = ifelse(Disclosure == "Yes", 1, 0),
    Antidepressants = ifelse(Antidepressants == "Yes", 1, 0),
    Danger = ifelse(Danger == "Yes", 1, 0),
    Recovery = ifelse(Recovery == "Yes", 1, 0),
    Treatment = ifelse(Treatment == "Yes", 1, 0),
    Counseling = ifelse(Counseling == "Yes", 1, 0),
    Preference = ifelse(Preference == "Yes", 1, 0), # Kolumna 83
    Diagnosed = ifelse(Diagnosed == "Yes", 1, 0),
    Have.an.Advanced.Directive = ifelse(Have.an.Advanced.Directive == "Yes", 1, 0),
    Aware.of.AARC = case_when(Aware.of.AARC == "Yes" ~ 1, Aware.of.AARC == "No" ~ 0, TRUE ~ NA_real_),
    EMS.Classes = case_when(EMS.Classes == "Yes" ~ 1, EMS.Classes == "No" ~ 0, TRUE ~ NA_real_),
    Fire.Alarm = case_when(Fire.Alarm == "Yes" ~ 1, Fire.Alarm == "No" ~ 0, TRUE ~ NA_real_),
    Public.Computer = case_when(Public.Computer == "Yes" ~ 1, Public.Computer == "No" ~ 0, TRUE ~ NA_real_),
    Library.Internet.Acess = case_when(Library.Internet.Acess == "Yes" ~ 1, Library.Internet.Acess == "No" ~ 0, TRUE ~ NA_real_),
    Literature = case_when(Literature == "Yes" ~ 1, Literature == "No" ~ 0, TRUE ~ NA_real_),
    Citizenship.Class = case_when(Citizenship.Class == "Yes" ~ 1, Citizenship.Class == "No" ~ 0, TRUE ~ NA_real_),
    Small.Business = case_when(Small.Business == "Yes" ~ 1, Small.Business == "No" ~ 0, TRUE ~ NA_real_), # Kolumna 138
    English.Classes = case_when(English.Classes == "Yes" ~ 1, English.Classes == "No" ~ 0, TRUE ~ NA_real_),
    X9.1.1 = case_when(X9.1.1 == "Yes" ~ 1, X9.1.1 == "No" ~ 0, TRUE ~ NA_real_),
    X3.1.1 = case_when(X3.1.1 == "Yes" ~ 1, X3.1.1 == "No" ~ 0, TRUE ~ NA_real_),
    APD.Languages = case_when(APD.Languages == "Yes" ~ 1, APD.Languages == "No" ~ 0, TRUE ~ NA_real_),
    District = case_when(District == "Yes" ~ 1, District == "No" ~ 0, TRUE ~ NA_real_),
    Nursing.Home = case_when(Nursing.Home == "Yes" ~ 1, Nursing.Home == "No" ~ 0, TRUE ~ NA_real_),
    Smoke.Detector = case_when(Smoke.Detector == "Yes" ~ 1, Smoke.Detector == "No" ~ 0, TRUE ~ NA_real_),
    Recycle = case_when(Recycle == "Yes" ~ 1, Recycle == "No" ~ 0, TRUE ~ NA_real_),
    Compost = case_when(Compost == "Yes" ~ 1, Compost == "No" ~ 0, TRUE ~ NA_real_),
    Public.Transportation = case_when(Public.Transportation == "Yes" ~ 1, Public.Transportation == "No" ~ 0, TRUE ~ NA_real_),
    Bicycling = case_when(Bicycling == "Yes" ~ 1, Bicycling == "No" ~ 0, TRUE ~ NA_real_),
    Carpooling = case_when(Carpooling == "Yes" ~ 1, Carpooling == "No" ~ 0, TRUE ~ NA_real_),
    Personal.Car = case_when(Personal.Car == "Yes" ~ 1, Personal.Car == "No" ~ 0, TRUE ~ NA_real_),
    Car.Share = case_when(Car.Share == "Yes" ~ 1, Car.Share == "No" ~ 0, TRUE ~ NA_real_),
    Walking = case_when(Walking == "Yes" ~ 1, Walking == "No" ~ 0, TRUE ~ NA_real_),
    Access.to.a.Computer = case_when(Access.to.a.Computer == "Yes" ~ 1, Access.to.a.Computer == "No" ~ 0, TRUE ~ NA_real_),
    Mobile.Devices = case_when(Mobile.Devices == "Yes" ~ 1, Mobile.Devices == "No" ~ 0, TRUE ~ NA_real_),
    Home.Phone = case_when(Home.Phone == "Yes" ~ 1, Home.Phone == "No" ~ 0, TRUE ~ NA_real_),
    Public.Meeting = case_when(Public.Meeting == "Yes" ~ 1, Public.Meeting == "No" ~ 0, TRUE ~ NA_real_),
    Council.Meeting = case_when(Council.Meeting == "Yes" ~ 1, Council.Meeting == "No" ~ 0, TRUE ~ NA_real_),
    Contact.City.Official = case_when(Contact.City.Official == "Yes" ~ 1, Contact.City.Official == "No" ~ 0, TRUE ~ NA_real_),
    City.Election = case_when(City.Election == "Yes" ~ 1, City.Election == "No" ~ 0, TRUE ~ NA_real_),
    Focus.Group = case_when(Focus.Group == "Yes" ~ 1, Focus.Group == "No" ~ 0, TRUE ~ NA_real_),
    
    # --- Przekodowania skal na liczby ---
    Income = case_when(
      Income == "$0 - $9.999" ~ 1, Income == "$10.000 - $19.999" ~ 2, Income == "$20.000 - $29.999" ~ 3,
      Income == "$30.000 - $39.999" ~ 4, Income == "$40.000 - $49.999" ~ 5, Income == "$50.000 - $59.999" ~ 6,
      Income == "$60.000 - $69.999" ~ 7, Income == "$70.000 and over" ~ 8, TRUE ~ NA_real_
    ),
    English.Speaking = case_when(
      English.Speaking == "Not at all" ~ 1, English.Speaking == "Not well" ~ 2,
      English.Speaking == "Well" ~ 3, English.Speaking == "Very well" ~ 4, TRUE ~ NA_real_
    ),
    English.Difficulties = case_when(
      English.Difficulties == "Not at all" ~ 1, English.Difficulties == "Not much" ~ 2,
      English.Difficulties == "Much" ~ 3, English.Difficulties == "Very much" ~ 4, TRUE ~ NA_real_
    ),
    Familiarity.with.America = case_when(
      Familiarity.with.America == "Very low" ~ 1, Familiarity.with.America == "Low" ~ 2,
      Familiarity.with.America == "High" ~ 3, Familiarity.with.America == "Very high" ~ 4, TRUE ~ NA_real_
    ),
    Familiarity.with.Ethnic.Origin = case_when(
      Familiarity.with.Ethnic.Origin == "Very low" ~ 1, Familiarity.with.Ethnic.Origin == "Low" ~ 2,
      Familiarity.with.Ethnic.Origin == "High" ~ 3, Familiarity.with.Ethnic.Origin == "Very high" ~ 4, TRUE ~ NA_real_
    ),
    Identify.Ethnically = case_when(
      Identify.Ethnically == "Not at all" ~ 1, Identify.Ethnically == "Not very close" ~ 2,
      Identify.Ethnically == "Somewhat close" ~ 3, Identify.Ethnically == "Very close" ~ 4, TRUE ~ NA_real_
    ),
    Belonging = case_when(
      Belonging == "Not at all" ~ 1, Belonging == "Not very much" ~ 2,
      Belonging == "Somewhat" ~ 3, Belonging == "Very much" ~ 4, TRUE ~ NA_real_
    ),
    Present.Health = case_when(
      Present.Health == "Poor" ~ 1, Present.Health == "Fair" ~ 2, Present.Health == "Good" ~ 3,
      Present.Health == "Very Good" ~ 4, Present.Health == "Excellent" ~ 5, TRUE ~ NA_real_
    ),
    Present.Mental.Health = case_when(
      Present.Mental.Health == "Poor" ~ 1, Present.Mental.Health == "Fair" ~ 2, Present.Mental.Health == "Good" ~ 3,
      Present.Mental.Health == "Very Good" ~ 4, Present.Mental.Health == "Excellent" ~ 5, TRUE ~ NA_real_
    ),
    Present.Oral.Health = case_when(
      Present.Oral.Health == "Poor" ~ 1, Present.Oral.Health == "Fair" ~ 2, Present.Oral.Health == "Good" ~ 3,
      Present.Oral.Health == "Very Good" ~ 4, Present.Oral.Health == "Excellent" ~ 5, TRUE ~ NA_real_
    ),
    Satisfaction = case_when( # Kolumna 59
      Satisfaction == "Not at all" ~ 1, Satisfaction == "Not very much" ~ 2,
      Satisfaction == "Pretty much" ~ 3, Satisfaction == "Very much" ~ 4, TRUE ~ NA_real_
    ),
    Ideal.Life = case_when(
      Ideal.Life == "Strongly disagree" ~ 1, Ideal.Life == "Disagree" ~ 2, Ideal.Life == "Slightly disagree" ~ 3,
      Ideal.Life == "Neither agree or disagree" ~ 4, Ideal.Life == "Slightly agree" ~ 5,
      Ideal.Life == "Agree" ~ 6, Ideal.Life == "Strongly agree" ~ 7, TRUE ~ NA_real_
    ),
    Satisfied.With.Life = case_when(
      Satisfied.With.Life == "Strongly disagree" ~ 1, Satisfied.With.Life == "Disagree" ~ 2, Satisfied.With.Life == "Slightly disagree" ~ 3,
      Satisfied.With.Life == "Neither agree or disagree" ~ 4, Satisfied.With.Life == "Slightly agree" ~ 5,
      Satisfied.With.Life == "Agree" ~ 6, Satisfied.With.Life == "Strongly agree" ~ 7, TRUE ~ NA_real_
    ),
    Knowledge = case_when(
      Knowledge == "Nothing at all" ~ 1, Knowledge == "Not very much" ~ 2,
      Knowledge == "Somewhat" ~ 3, Knowledge == "Very much" ~ 4, TRUE ~ NA_real_
    ),
    Superstition = case_when(
      Superstition == "Strongly disagree" ~ 1, Superstition == "Somewhat disagree" ~ 2,
      Superstition == "Somewhat agree" ~ 3, Superstition == "Strongly agree" ~ 4, TRUE ~ NA_real_
    ),
    Family.Respect = case_when(
      Family.Respect == "Strongly disagree" ~ 1, Family.Respect == "Somewhat disagree" ~ 2,
      Family.Respect == "Somewhat agree" ~ 3, Family.Respect == "Strongly agree" ~ 4, TRUE ~ NA_real_
    ),
    Similar.Values = case_when(
      Similar.Values == "Strongly disagree" ~ 1, Similar.Values == "Somewhat disagree" ~ 2,
      Similar.Values == "Somewhat agree" ~ 3, Similar.Values == "Strongly agree" ~ 4, TRUE ~ NA_real_
    ),
    Successful.Family = case_when(
      Successful.Family == "Strongly disagree" ~ 1, Successful.Family == "Somewhat disagree" ~ 2,
      Successful.Family == "Somewhat agree" ~ 3, Successful.Family == "Strongly agree" ~ 4, TRUE ~ NA_real_
    ),
    Trust = case_when(
      Trust == "Strongly disagree" ~ 1, Trust == "Somewhat disagree" ~ 2,
      Trust == "Somewhat agree" ~ 3, Trust == "Strongly agree" ~ 4, TRUE ~ NA_real_
    ),
    Loyalty = case_when(
      Loyalty == "Strongly disagree" ~ 1, Loyalty == "Somewhat disagree" ~ 2,
      Loyalty == "Somewhat agree" ~ 3, Loyalty == "Strongly agree" ~ 4, TRUE ~ NA_real_
    ),
    Family.Pride = case_when(
      Family.Pride == "Strongly disagree" ~ 1, Family.Pride == "Somewhat disagree" ~ 2,
      Family.Pride == "Somewhat agree" ~ 3, Family.Pride == "Strongly agree" ~ 4, TRUE ~ NA_real_
    ),
    Expression = case_when(
      Expression == "Strongly disagree" ~ 1, Expression == "Somewhat disagree" ~ 2,
      Expression == "Somewhat agree" ~ 3, Expression == "Strongly agree" ~ 4, TRUE ~ NA_real_
    ),
    Spend.Time.Together = case_when(
      Spend.Time.Together == "Strongly disagree" ~ 1, Spend.Time.Together == "Somewhat disagree" ~ 2,
      Spend.Time.Together == "Somewhat agree" ~ 3, Spend.Time.Together == "Strongly agree" ~ 4, TRUE ~ NA_real_
    ),
    Feel.Close = case_when(
      Feel.Close == "Strongly disagree" ~ 1, Feel.Close == "Somewhat disagree" ~ 2,
      Feel.Close == "Somewhat agree" ~ 3, Feel.Close == "Strongly agree" ~ 4, TRUE ~ NA_real_
    ),
    Togetherness = case_when(
      Togetherness == "Strongly disagree" ~ 1, Togetherness == "Somewhat disagree" ~ 2,
      Togetherness == "Somewhat agree" ~ 3, Togetherness == "Strongly agree" ~ 4, TRUE ~ NA_real_
    ),
    Religious.Attendance = case_when(
      Religious.Attendance == "Never" ~ 1, Religious.Attendance == "Seldom" ~ 2,
      Religious.Attendance == "A few times a year" ~ 3, Religious.Attendance == "Once or twice a month" ~ 4, TRUE ~ NA_real_
    ),
    Religious.Importance = case_when(
      Religious.Importance == "Not at all important" ~ 1, Religious.Importance == "Not very important" ~ 2,
      Religious.Importance == "Somewhat important" ~ 3, Religious.Importance == "Very important" ~ 4, TRUE ~ NA_real_
    ),
    Close.knit.Community = case_when(
      Close.knit.Community == "Strongly disagree" ~ 1, Close.knit.Community == "Disagree" ~ 2, Close.knit.Community == "Neutral" ~ 3,
      Close.knit.Community == "Agree" ~ 4, Close.knit.Community == "Strongly agree" ~ 5, TRUE ~ NA_real_
    ),
    Helpful.Community = case_when(
      Helpful.Community == "Strongly disagree" ~ 1, Helpful.Community == "Disagree" ~ 2, Helpful.Community == "Neutral" ~ 3,
      Helpful.Community == "Agree" ~ 4, Helpful.Community == "Strongly agree" ~ 5, TRUE ~ NA_real_
    ),
    Community.Shares.Values = case_when(
      Community.Shares.Values == "Strongly disagree" ~ 1, Community.Shares.Values == "Disagree" ~ 2, Community.Shares.Values == "Neutral" ~ 3,
      Community.Shares.Values == "Agree" ~ 4, Community.Shares.Values == "Strongly agree" ~ 5, TRUE ~ NA_real_
    ),
    Get.Along = case_when(
      Get.Along == "Strongly disagree" ~ 1, Get.Along == "Disagree" ~ 2, Get.Along == "Neutral" ~ 3,
      Get.Along == "Agree" ~ 4, Get.Along == "Strongly agree" ~ 5, TRUE ~ NA_real_
    ),
    Community.Trust = case_when(
      Community.Trust == "Strongly disagree" ~ 1, Community.Trust == "Disagree" ~ 2, Community.Trust == "Neutral" ~ 3,
      Community.Trust == "Agree" ~ 4, Community.Trust == "Strongly agree" ~ 5, TRUE ~ NA_real_
    ),
    Place.to.Live = case_when(
      Place.to.Live == "Poor" ~ 1, Place.to.Live == "Fair" ~ 2, Place.to.Live == "Good" ~ 3, Place.to.Live == "Excellent" ~ 4, TRUE ~ NA_real_
    ),
    Raising.Children = case_when(
      Raising.Children == "Poor" ~ 1, Raising.Children == "Fair" ~ 2, Raising.Children == "Good" ~ 3, Raising.Children == "Excellent" ~ 4, TRUE ~ NA_real_
    ),
    Place.to.Work = case_when(
      Place.to.Work == "Poor" ~ 1, Place.to.Work == "Fair" ~ 2, Place.to.Work == "Good" ~ 3, Place.to.Work == "Excellent" ~ 4, TRUE ~ NA_real_
    ),
    Small.Businesses = case_when( # Kolumna 115
      Small.Businesses == "Poor" ~ 1, Small.Businesses == "Fair" ~ 2, Small.Businesses == "Good" ~ 3, Small.Businesses == "Excellent" ~ 4, TRUE ~ NA_real_
    ),
    Place.to.Retire = case_when(
      Place.to.Retire == "Poor" ~ 1, Place.to.Retire == "Fair" ~ 2, Place.to.Retire == "Good" ~ 3, Place.to.Retire == "Excellent" ~ 4, TRUE ~ NA_real_
    ),
    Arts.and.Culture = case_when(
      Arts.and.Culture == "Poor" ~ 1, Arts.and.Culture == "Fair" ~ 2, Arts.and.Culture == "Good" ~ 3, Arts.and.Culture == "Excellent" ~ 4, TRUE ~ NA_real_
    ),
    Safety = case_when(
      Safety == "Poor" ~ 1, Safety == "Fair" ~ 2, Safety == "Good" ~ 3, Safety == "Excellent" ~ 4, TRUE ~ NA_real_
    ),
    Traffic = case_when(
      Traffic == "Poor" ~ 1, Traffic == "Fair" ~ 2, Traffic == "Good" ~ 3, Traffic == "Excellent" ~ 4, TRUE ~ NA_real_
    ),
    Quality_of_city_life = case_when( # Używamy nowej nazwy
      Quality_of_city_life == "Poor" ~ 1, Quality_of_city_life == "Fair" ~ 2,
      Quality_of_city_life == "Good" ~ 3, Quality_of_city_life == "Excellent" ~ 4, TRUE ~ NA_real_
    ),
    Quality.of.Service = case_when(
      Quality.of.Service == "Poor" ~ 1, Quality.of.Service == "Fair" ~ 2,
      Quality.of.Service == "Good" ~ 3, Quality.of.Service == "Excellent" ~ 4, TRUE ~ NA_real_
    ),
    Parks.and.Recs = case_when(
      Parks.and.Recs == "Never used" ~ 0, Parks.and.Recs == "Not at all satisfied" ~ 1,
      Parks.and.Recs == "Not very much satisfied" ~ 2, Parks.and.Recs == "Pretty much satisfied" ~ 3,
      Parks.and.Recs == "Very much satisfied" ~ 4, TRUE ~ NA_real_
    ),
    Libraries = case_when(
      Libraries == "Never used" ~ 0, Libraries == "Not at all satisfied" ~ 1,
      Libraries == "Not very much satisfied" ~ 2, Libraries == "Pretty much satisfied" ~ 3,
      Libraries == "Very much satisfied" ~ 4, TRUE ~ NA_real_
    ),
    Public.Safety = case_when(
      Public.Safety == "Never used" ~ 0, Public.Safety == "Not at all satisfied" ~ 1,
      Public.Safety == "Not very much satisfied" ~ 2, Public.Safety == "Pretty much satisfied" ~ 3,
      Public.Safety == "Very much satisfied" ~ 4, TRUE ~ NA_real_
    ),
    Airport = case_when(
      Airport == "Never used" ~ 0, Airport == "Not at all satisfied" ~ 1,
      Airport == "Not very much satisfied" ~ 2, Airport == "Pretty much satisfied" ~ 3,
      Airport == "Very much satisfied" ~ 4, TRUE ~ NA_real_
    ),
    Austin.Energy = case_when(
      Austin.Energy == "Never used" ~ 0, Austin.Energy == "Not at all satisfied" ~ 1,
      Austin.Energy == "Not very much satisfied" ~ 2, Austin.Energy == "Pretty much satisfied" ~ 3,
      Austin.Energy == "Very much satisfied" ~ 4, TRUE ~ NA_real_
    ),
    Court = case_when(
      Court == "Never used" ~ 0, Court == "Not at all satisfied" ~ 1,
      Court == "Not very much satisfied" ~ 2, Court == "Pretty much satisfied" ~ 3,
      Court == "Very much satisfied" ~ 4, TRUE ~ NA_real_
    ),
    Social.Services = case_when(
      Social.Services == "Never used" ~ 0, Social.Services == "Not at all satisfied" ~ 1,
      Social.Services == "Not very much satisfied" ~ 2, Social.Services == "Pretty much satisfied" ~ 3,
      Social.Services == "Very much satisfied" ~ 4, TRUE ~ NA_real_
    ),
    Visit.Frequency = case_when(
      Visit.Frequency == "Never" ~ 1, Visit.Frequency == "Rarely" ~ 2,
      Visit.Frequency == "Some of the time" ~ 3, Visit.Frequency == "Often" ~ 4, TRUE ~ NA_real_
    ),
    Activities = case_when(
      Activities == "Never" ~ 1, Activities == "Rarely" ~ 2,
      Activities == "Some of the time" ~ 3, Activities == "Often" ~ 4, TRUE ~ NA_real_
    ),
    Satisfaction.With.Housing. = case_when(
      Satisfaction.With.Housing. == "Not at all" ~ 1, Satisfaction.With.Housing. == "Not very much" ~ 2,
      Satisfaction.With.Housing. == "Pretty much" ~ 3, Satisfaction.With.Housing. == "Very much" ~ 4, TRUE ~ NA_real_
    ),
    Informed = case_when(
      Informed == "Not interested at all" ~ 1, Informed == "Not interested" ~ 2,
      Informed == "Somewhat interested" ~ 3, Informed == "Interested" ~ 4,
      Informed == "Very interested" ~ 5, TRUE ~ NA_real_
    ),
    City.Effort.Satisfaction = case_when(
      City.Effort.Satisfaction == "Very dissatisfied" ~ 1, City.Effort.Satisfaction == "Somewhat dissatisfied" ~ 2,
      City.Effort.Satisfaction == "Niether satisfied or dissatisfied" ~ 3, # Potencjalna literówka w danych
      City.Effort.Satisfaction == "Neither satisfied or dissatisfied" ~ 3, # Poprawiona wersja
      City.Effort.Satisfaction == "Somewhat satisfied" ~ 4, City.Effort.Satisfaction == "Very satisfied" ~ 5, TRUE ~ NA_real_
    )
    # Usunięto końcową sekcję z as.numeric() i as.factor() - te konwersje będą globalne
  )

# 3. Usunięcie wybranych kolumn (tak jak w oryginalnym kodzie)
if ("Other" %in% names(data_transformed)) {
  data_transformed <- data_transformed %>% select(-Other)
}
if ("Health.Info.Discription" %in% names(data_transformed)) {
  data_transformed <- data_transformed %>% select(-Health.Info.Discription)
}
if ("na_per_row" %in% names(data_transformed)) {
  data_transformed <- data_transformed %>% select(-na_per_row)
}


# 4. Najpierw konwersja wszystkich kolumn na factor (zgodnie z prośbą)
all_cols <- colnames(data_transformed)
data_transformed <- data_transformed %>%
  mutate(across(all_of(all_cols), as.factor))

# 5. Zdefiniowanie list kolumn do specyficznych typów
cols_to_numeric <- c(
  "Age", "Education.Completed", "Household.Size",
  "US.Residency", "City.Residency",
  "Quality.of.Life" # Ta kolumna (oryginalnie np. kolumna 71) ma być numeryczna
  # Różni się od 'Quality_of_city_life' która jest skalą porządkową
)
# Upewnij się, że wszystkie te kolumny istnieją w data_transformed
cols_to_numeric <- intersect(cols_to_numeric, colnames(data_transformed))


cols_to_ordered <- c(
  "Income", "English.Speaking", "English.Difficulties", "Familiarity.with.America",
  "Familiarity.with.Ethnic.Origin", "Identify.Ethnically", "Belonging",
  "Present.Health", "Present.Mental.Health", "Present.Oral.Health",
  "Satisfaction", # Kolumna 59
  "Ideal.Life", "Satisfied.With.Life", "Knowledge", "Superstition", "Family.Respect",
  "Similar.Values", "Successful.Family", "Trust", "Loyalty", "Family.Pride",
  "Expression", "Spend.Time.Together", "Feel.Close", "Togetherness",
  "Religious.Attendance", "Religious.Importance",
  "Close.knit.Community", "Helpful.Community", "Community.Shares.Values", "Get.Along", "Community.Trust",
  "Place.to.Live", "Raising.Children", "Place.to.Work",
  "Small.Businesses", # Kolumna 115
  "Place.to.Retire", "Arts.and.Culture", "Safety", "Traffic",
  "Quality_of_city_life", # To jest ta przekodowana ze skali tekstowej
  "Quality.of.Service",
  "Parks.and.Recs", "Libraries", "Public.Safety", "Airport", "Austin.Energy", "Court", "Social.Services",
  "Visit.Frequency", "Activities", "Satisfaction.With.Housing.", "Informed", "City.Effort.Satisfaction"
)
# Upewnij się, że wszystkie te kolumny istnieją w data_transformed
cols_to_ordered <- intersect(cols_to_ordered, colnames(data_transformed))


# 6. Konwersja wybranych kolumn na numeric
# Używamy as.numeric(as.character(.)) do poprawnej konwersji factor -> numeric
if (length(cols_to_numeric) > 0) {
  data_transformed <- data_transformed %>%
    mutate(across(all_of(cols_to_numeric), ~as.numeric(as.character(.))))
}

# 7. Modyfikacja wartości w kolumnie Quality.of.Life (numerycznej)
# Ta operacja powinna być wykonana PO tym, jak Quality.of.Life stała się numeryczna
if ("Quality.of.Life" %in% cols_to_numeric && "Quality.of.Life" %in% colnames(data_transformed)) {
  data_transformed$Quality.of.Life <- ifelse(
    !is.na(data_transformed$Quality.of.Life) & data_transformed$Quality.of.Life <= 5,
    5,
    data_transformed$Quality.of.Life
  )
}


# 8. Konwersja wybranych kolumn na ordered factor
# Poziomy faktora są już numeryczne ("1", "2", "3"...), więc ordered() powinno zadziałać poprawnie.
if (length(cols_to_ordered) > 0) {
  data_transformed <- data_transformed %>%
    mutate(across(all_of(cols_to_ordered), ordered))
}

summary(data_transformed)

save(data_transformed, file = "processed_data/data_transformed.RData")

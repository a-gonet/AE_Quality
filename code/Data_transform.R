library(dplyr)


##############################################################################################
######################## Exploratory Data Analysis and Transformation ########################
##############################################################################################

data <- read.csv("raw_data/Report_Quality_of_Life.csv")


# Step 1: Cleaning the data
clean_columns <- function(x) {
  if (is.factor(x)) x <- as.character(x)

  if (is.character(x)) {
    x[x == "0"] <- "No"
    x[grepl("^\\d+\\.maj$", x)] <- NA

    numeric_pattern <- "^\\d+(\\.\\d+)?$"
    numeric_matches <- grepl(numeric_pattern, x)
    x[numeric_matches] <- as.numeric(x[numeric_matches])
  }

  x[x == ""] <- NA

  # Convert whole column to numeric if all non-NA values are numeric
  if (all(is.na(x) | grepl("^\\d+(\\.\\d+)?$", x))) {
    x <- as.numeric(x)
  }

  return(x)
}

data_clean <- data %>%
  mutate(across(everything(), clean_columns)) %>%
  mutate(across(where(is.character), ~ {
    x <- .x
    numeric_pattern <- "^\\d+(\\.\\d+)?$"
    x[grepl(numeric_pattern, x)] <- NA
    x
  })) %>%
  mutate(across(where(~ length(unique(na.omit(.))) <= 2), as.factor))

data_clean$na_per_row <- apply(data_clean, 1, function(x) sum(is.na(x)))

data_filtered <- data_clean[data_clean$na_per_row <= 7 , ]
data_filtered <- data_filtered %>%
  select(-na_per_row, -Health.Info.Discription)

# Step 2: Transforming the data
data_transformed <- data_filtered %>%
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
    Quality_of_city_life = Qualtiy.of.Life, # For Ordinal Scale
    House_Ownership = Status.of.Ownership,
    Sibling_LivingWith = Brother.Sister,
    US.Residency = Duration.of.Residency,
    City.Residency = Residency
  )

numeric_columns <- select(data_transformed, where(is.numeric))

binary_factor_columns <- select(data_transformed, where(is.factor))

multifactor_columns <- data_transformed %>%
  select(setdiff(colnames(data_transformed), c(colnames(binary_factor_columns), colnames(numeric_columns))))

# Step 3: Encoding the data for Binary and Ordinal Scales
unique_values <- lapply(multifactor_columns, unique)
all_unique_values <- unique(unlist(unique_values))

unorder_columns <- c("Ethnicity", "Marital.Status", "Religion", "Language", "Religious.Attendance", "Housing")
data_transformed <- data_transformed %>%
  mutate(across(all_of(unorder_columns), as.factor))

ordered_columns <- multifactor_columns %>%
  select(-all_of(unorder_columns))

unique_values <- lapply(ordered_columns, unique)
all_unique_values <- unique(unlist(unique_values))

ordered_levels <- list(
  Income = c("$0 - $9.999", "$10.000 - $19.999", "$20.000 - $29.999", "$30.000 - $39.999", "$40.000 - $49.999", "$50.000 - $59.999", "$60.000 - $69.999", "$70.000 and over"),
  English.Speaking = c("Not at all", "Not well", "Well", "Very well", "NA"),
  English.Difficulties = c("Not at all", "Not much", "Much", "Very much"),
  Familiarity.with.America = c("Very low", "Low", "High", "Very high"), 
  Familiarity.with.Ethnic.Origin = c("Very low", "Low", "High", "Very high"),
  Identify.Ethnically = c("Not at all", "Not very close", "Somewhat close", "Very close"),
  Belonging = c("Not at all", "Not very much", "Somewhat", "Very much"),
  Present.Health = c("Poor", "Fair", "Good", "Very Good", "Excellent"),
  Present.Mental.Health = c("Poor", "Fair", "Good", "Very Good", "Excellent"),
  Present.Oral.Health = c("Poor", "Fair", "Good", "Very Good", "Excellent"),
  Satisfaction = c("Not at all", "Not very much", "Pretty much", "Very much"),
  Satisfaction.With.Housing = c("Not at all", "Not very much", "Pretty much", "Very much"), 
  Ideal.Life = c("Strongly disagree", "Disagree", "Slightly disagree", "Neither agree or disagree", "Slightly agree", "Agree", "Strongly agree"), 
  Satisfied.With.Life = c("Strongly disagree", "Disagree", "Slightly disagree", "Neither agree or disagree", "Slightly agree", "Agree", "Strongly agree"),
  Knowledge = c("Nothing at all", "Not very much", "Somewhat", "Very much"),
  Superstition = c("Strongly disagree", "Somewhat disagree", "Somewhat agree", "Strongly agree"),
  Family.Respect = c("Strongly disagree", "Somewhat disagree", "Somewhat agree", "Strongly agree"),
  Similar.Values = c("Strongly disagree", "Somewhat disagree", "Somewhat agree", "Strongly agree"),
  Successful.Family = c("Strongly disagree", "Somewhat disagree", "Somewhat agree", "Strongly agree"),
  Trust = c("Strongly disagree", "Somewhat disagree", "Somewhat agree", "Strongly agree"),
  Loyalty = c("Strongly disagree", "Somewhat disagree", "Somewhat agree", "Strongly agree"),
  Family.Pride = c("Strongly disagree", "Somewhat disagree", "Somewhat agree", "Strongly agree"),
  Expression = c("Strongly disagree", "Somewhat disagree", "Somewhat agree", "Strongly agree"),
  Spend.Time.Together = c("Strongly disagree", "Somewhat disagree", "Somewhat agree", "Strongly agree"),
  Feel.Close = c("Strongly disagree", "Somewhat disagree", "Somewhat agree", "Strongly agree"),
  Togetherness = c("Strongly disagree", "Somewhat disagree", "Somewhat agree", "Strongly agree"),
  Religious.Importance = c("Not at all important", "Not very important", "Somewhat important", "Very important"),
  Close.knit.Community = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"),
  Helpful.Community = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"),
  Community.Shares.Values = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"),
  Get.Along = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"),
  Community.Trust = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"),
  Place.to.Live = c("Poor", "Fair", "Good", "Excellent"),
  Raising.Children = c("Poor", "Fair", "Good", "Excellent"),
  Place.to.Work = c("Poor", "Fair", "Good", "Excellent"),
  Small.Businesses = c("Poor", "Fair", "Good", "Excellent"),
  Place.to.Retire = c("Poor", "Fair", "Good", "Excellent"),
  Arts.and.Culture = c("Poor", "Fair", "Good", "Excellent"),
  Safety = c("Poor", "Fair", "Good", "Excellent"),
  Traffic = c("Poor", "Fair", "Good", "Excellent"),
  Quality_of_city_life = c("Poor", "Fair", "Good", "Excellent"), # Quality_of_city_life
  Quality.of.Service = c("Poor", "Fair", "Good", "Excellent"), # Quality.of.Service
  Parks.and.Recs = c("Never used", "Not at all satisfied", "Not very much satisfied", "Pretty much satisfied", "Very much satisfied"), # Parks.and.Recs, Libraries, Public.Safety, Airport, Austin.Energy, Court, Social.Services
  Libraries = c("Never used", "Not at all satisfied", "Not very much satisfied", "Pretty much satisfied", "Very much satisfied"), # Libraries
  Public.Safety = c("Never used", "Not at all satisfied", "Not very much satisfied", "Pretty much satisfied", "Very much satisfied"), # Public.Safety
  Airport = c("Never used", "Not at all satisfied", "Not very much satisfied", "Pretty much satisfied", "Very much satisfied"), # Airport
  Austin.Energy = c("Never used", "Not at all satisfied", "Not very much satisfied", "Pretty much satisfied", "Very much satisfied"), # Austin.Energy
  Court = c("Never used", "Not at all satisfied", "Not very much satisfied", "Pretty much satisfied", "Very much satisfied"), # Court
  Social.Services = c("Never used", "Not at all satisfied", "Not very much satisfied", "Pretty much satisfied", "Very much satisfied"), # Social.Services
  Visit.Frequency = c("Never", "Rarely", "Some of the time", "Often"), # Visit.Frequency, Activities,
  Activities = c("Never", "Rarely", "Some of the time", "Often"), # Activities
  Informed = c("Not interested", "Not interested at all", "Somewhat interested", "Interested", "Very interested"), # Informed
  City.Effort.Satisfaction = c("Very dissatisfied", "Somewhat dissatisfied", "Niether satisfied or dissatisfied", "Somewhat satisfied", "Very satisfied")
)

data_transformed <- data_transformed %>%
  mutate(across(all_of(names(ordered_columns)), ~ factor(.x, levels = ordered_levels[[cur_column()]], ordered = TRUE)))

binary_transform <- function(col) {
  vals <- unique(na.omit(col))
  vals_upper <- toupper(vals)
  length(vals) == 2 && "NO" %in% vals_upper
}

binary_columns <- sapply(data_transformed, binary_transform)

data_transformed[binary_columns] <- lapply(data_transformed[binary_columns], function(col) {
  factor(ifelse(
    is.na(col), NA,
    ifelse(as.character(col) == "NO", 0, 1)
  ))
})


# Step 4. Checking for unnecessary columns
if ("Other" %in% names(data_transformed)) {
  data_transformed <- data_transformed %>% dplyr::select(-Other)
}
if ("Health.Info.Discription" %in% names(data_transformed)) {
  data_transformed <- data_transformed %>% dplyr::select(-Health.Info.Discription)
}
if ("na_per_row" %in% names(data_transformed)) {
  data_transformed <- data_transformed %>% dplyr::select(-na_per_row)
}

# Step 5. Defining final columns and checking existance
column_groups <- list(
  numeric = names(numeric_columns),
  binary = names(binary_factor_columns),
  multifactor = names(multifactor_columns)
)
common_cols_list <- list()
for (group_name in names(column_groups)) {
  common_cols_list[[group_name]] <- column_groups[[group_name]] %in% colnames(data_transformed)
}
common_cols_list

data_transformed["Quality.of.Life"]

# Step 6. Modifying Quality.of.Life to smaller order
data_transformed <- data_transformed %>%
  mutate(Quality.of.Life = factor(Quality.of.Life,
                                 levels = as.character(1:10),
                                 ordered = TRUE))

# Step 7. Saving the transformed data
summary(data_transformed)

save(data_transformed, file = "processed_data/data_transformed.RData")



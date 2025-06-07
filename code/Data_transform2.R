library(dplyr)


##############################################################################################
######################## Exploratory Data Analysis and Transformation ########################
##############################################################################################

#data <- read.csv("raw_data/Report_Quality_of_Life.csv")

data2 <- read.csv("AAQoL_original.csv")
data3 <- rbind(c(1:nrow(data2)),data2)

#przefiltrowane ręcznie kolumny z source

goodCols <- c(2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 16, 19, 21, 22, 25, 26, 27, 28, 29, 34, 35, 36, 37, 38, 39, 
              41, 42, 45, 46, 47, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 66, 69, 77, 
              91, 93, 126, 127, 128, 129, 130, 131, 142, 143, 182, 184, 186, 187, 188, 189, 190, 191, 192, 
              193, 194, 195, 196, 199, 200, 201)


dataRed <- data2[2:nrow(data2), goodCols]

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

dataRed <- dataRed %>%
  mutate(across(c(Disabled, Unemployed, Income, Heart.Disease, Stroke, Cancer,
                  Hepatitis, Kidney.Problem, Asthma, COPD), as.factor))


data_clean <- dataRed %>%
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
  select(-na_per_row)

data_transformed <- data_filtered

# Step 2: Transforming the data
data_transformed <- data_filtered %>%
  dplyr::rename(
    Alone = No.One,
    Spouse_LivingWith = Spouse,
    Children_LivingWith = Children,
    Parent_LivingWith = Parent,
    Friends_LivingWith = Friends,
    Full_Time_Employment_Status = Full.Time.Employment,
    Part_Time_Employment_Status = Part.Time.Employment,
    Homemaker_Status = Homemaker,
    US_Born_Status = US.Born,
    House_Ownership = Status.of.Ownership,
    US.Residency = Duration.of.Residency,
    Satisfaction.With.Housing = Satisfaction.With.Housing.,
    Satisfied.With.Life = Satisfied.With.Life.2,
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
  Income = c("$0 - $9,999", "$10,000 - $19,999", "$20,000 - $29,999", "$30,000 - $39,999", "$40,000 - $49,999", "$50,000 - $59,999", "$60,000 - $69,999", "$70,000 and over"),
  English.Speaking = c("Not at all", "Not well", "Well", "Very well"),
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
  Religious.Attendance = c("Never", "Seldom", "A few times a year", "Once or twice a month"),
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
  City.Effort.Satisfaction = c("Very dissatisfied", "Somewhat dissatisfied", "Niether satisfied or dissatisfied", "Somewhat satisfied", "Very satisfied"),
  See.Family = c("0", "1", "2", "3", "4", "5"),
  Close.Family = c("0", "1", "2", "3", "4", "5"),
  Helpful.Family = c("0", "1", "2", "3", "4", "5"),
  See.Friends = c("0", "1", "2", "3", "4", "5"),
  Close.Friends = c("0", "1", "2", "3", "4", "5"),
  Helpful.Friends = c("0", "1", "2", "3", "4", "5")
  
)



for(colname in names(ordered_levels)) {
  if (colname %in% names(data_transformed)) {
    data_transformed[[colname]] <- factor(data_transformed[[colname]], levels = ordered_levels[[colname]], ordered = TRUE)
    data_transformed[[colname]] <- as.numeric(data_transformed[[colname]])
    data_transformed[[colname]] <- as.factor(data_transformed[[colname]])
  }
}

binary_transform <- function(col) {
  vals <- unique(na.omit(col))
  vals_upper <- toupper(vals)
  length(vals) == 2 && "NO" %in% vals_upper
}

binary_columns <- sapply(data_transformed, binary_transform)

data_transformed[binary_columns] <- lapply(data_transformed[binary_columns], function(col) {
  factor(ifelse(
    is.na(col), NA,
    ifelse(as.character(col) == "No", 0, 1)
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

data_transformed <- data_transformed[!is.na(data_transformed$Quality.of.Life),]

summary(data_transformed["Quality.of.Life"])


# Step 5.2 Handling NA

most_frequent <- function(x, na.rm = TRUE) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

for (col in names(data_transformed)) {
  # Check if column is numeric
  if (is.numeric(data_transformed[[col]])) {
    # Replace NAs with median (ignoring NAs in median computation)
    data_transformed[[col]][is.na(data_transformed[[col]])] <- median(data_transformed[[col]], na.rm = TRUE)
  }
  if (is.factor(data_transformed[[col]])) {
    # Replace NAs with mode (most frequent value)
    data_transformed[[col]][is.na(data_transformed[[col]])] <- most_frequent(data_transformed[[col]], na.rm = TRUE)
  }
}

sapply(data_transformed[, sapply(data_transformed, is.numeric)], function(x) sum(is.na(x)))
sapply(data_transformed[, sapply(data_transformed, is.factor)], function(x) sum(is.na(x)))

# Step 5.3 Handling One Answer
df_for_modeling <- data_transformed[, sapply(data_transformed, function(col) {
  # Remove NA values
  vals <- na.omit(col)
  # Count unique values
  length(unique(vals)) > 1
})]


# Step 8. Removing correlated columns
correlated_cols <- c("Present.Health", "Present.Mental.Health", "Present.Oral.Health", "Arts.and.Culture", "Safety",
                     "Social.Services", "Parks.and.Recs", "Libraries", "Public.Safety", "Airport", "Austin.Energy", "Court", "Helpful.Community",
                     "Close.knit.Community", "Community.Shares.Values", "Get.Along", "Community.Trust", "Togetherness", "Spend.Time.Together",
                     "Feel.Close", "Family.Pride", "Expression", "Trust", "Loyalty", "Family.Respect", "Similar.Values", "Successful.Family",
                     "See.Family", "Helpful.Family", "Close.Family", "See.Friends", "Helpful.Friends", "Close.Friends", "Place.to.Live",
                     "Raising.Children", "Place.to.Retire", "Housing",  "Visit.Frequency", "Quality.of.Service",
                     "Student", "Retired", "Alone", "Primary.Language", "Familiarity.with.America",
                     "See.Family","Close.Family","Helpful.Family","See.Friends","Close.Friends","Helpful.Friends",
                     "Religious.Attendance", "Religious.Importance", "Religion", "Language")

health_combined <- c("Present.Health", "Present.Mental.Health")
relatives_combined <- c("See.Family","Close.Family","Helpful.Family","See.Friends","Close.Friends","Helpful.Friends")
religion_combined <- c("Religious.Attendance", "Religious.Importance")

pca_names_combined <- c(health_combined, relatives_combined, religion_combined)

library(stats)

for (colname in pca_names_combined) {
  col <- df_for_modeling[[colname]]

  # Convert factor/character to ordered numeric
  if (is.factor(col) || is.character(col)) {
    col <- as.numeric(factor(col, ordered = TRUE))
  }

  # Replace NAs with median
  if (is.numeric(col)) {
    median_val <- median(col, na.rm = TRUE)
    col[is.na(col)] <- median_val
  }

  df_for_modeling[[colname]] <- col
}


pca_health <- prcomp(df_for_modeling[, health_combined], center = TRUE, scale. = TRUE)
df_for_modeling$Health.Combined.Score <- pca_health$x[,1]

pca_relatives <- prcomp(df_for_modeling[, relatives_combined], center = TRUE, scale. = TRUE)
df_for_modeling$Relatives.Support.Score <- pca_relatives$x[,1]

pca_religion <- prcomp(df_for_modeling[, religion_combined], center = TRUE, scale. = TRUE)
df_for_modeling$Religiousness.Score <- pca_religion$x[,1]
# 
df_for_modeling <- df_for_modeling[, !(names(df_for_modeling) %in% correlated_cols)]

# Step 9. Removing variables with near 0 variance
library(caret)
nzv <- nearZeroVar(df_for_modeling, saveMetrics = TRUE)
df_for_modeling <- df_for_modeling[, !nzv$nzv]


# Step n. Saving the final data
summary(df_for_modeling)

df_for_modeling <- df_for_modeling[complete.cases(df_for_modeling), ]
df_for_modeling <- df_for_modeling[, sapply(df_for_modeling, function(x) length(unique(x)) > 1)]

save(df_for_modeling, file = "processed_data/df_for_modeling.RData")

for(col in colnames(df_for_modeling)) {
  # Skip target variable itself
  if (col == "Quality.of.Life") next
  
  # Only factor or character columns (categorical)
  if (is.factor(df_for_modeling[[col]]) || is.character(df_for_modeling[[col]])) {
    cat("Checking column:", col, "\n")
    print(table(df_for_modeling[[col]], df_for_modeling$Quality.of.Life))
    cat("\n---------------------------------\n")
  }
}

library(fastDummies)

# One-hot encoding wybranych kolumn
df_onehot <- dummy_cols(
  df_for_modeling,
  select_columns = c("Ethnicity"),
  remove_first_dummy = FALSE,
  remove_selected_columns = TRUE
)

# Zamień nowe kolumny (czyli te zawierające "_") na faktory
onehot_colnames <- grep("_", colnames(df_onehot), value = TRUE)

df_onehot[onehot_colnames] <- lapply(df_onehot[onehot_colnames], factor)

df_onehot[onehot_colnames] <- lapply(df_onehot[onehot_colnames], function(x) {
  if (length(unique(x)) > 2) {
    return(as.ordered(x))
  } else {
    return(as.factor(x))
  }
})

df_onehot$isPartnered <- 0
df_onehot$isPartnered[df_onehot$Marital.Status %in% c("Living with a partner", "Married")] = 1
df_onehot$Marital.Status <- NULL

df_onehot$GoodEnglish <- 0
df_onehot$GoodEnglish[df_onehot$English.Speaking %in% c("3", "4")] <- 1
df_onehot$English.Speaking <- NULL

df_onehot$EthnicFamiliar <- 0
df_onehot$EthnicFamiliar[df_onehot$Familiarity.with.Ethnic.Origin %in% c("3", "4")] <- 1
df_onehot$Familiarity.with.Ethnic.Origin <- NULL

df_onehot$DissatisfiedLife <- 0
df_onehot$DissatisfiedLife[df_onehot$Satisfied.With.Life %in% c("1", "2")] <- 1
df_onehot$SatisfiedLife <- 0
df_onehot$SatisfiedLife[df_onehot$Satisfied.With.Life %in% c("6", "7")] <- 1
df_onehot$Satisfied.With.Life <- NULL

df_onehot$HousingSatisfaction <- 0
df_onehot$HousingSatisfaction[df_onehot$Satisfaction.With.Housing %in% c("3", "4")] <- 1
df_onehot$Satisfaction.With.Housing <- NULL


df_onehot$highIncome <- 0
df_onehot$highIncome[df_onehot$Income %in% c("8")] <- 1
df_onehot$lowIncome <- 0
df_onehot$lowIncome[df_onehot$Income %in% c("1", "2", "3")] <- 1
df_onehot$Income <- NULL

df_onehot$badQuality <- 0
df_onehot$badQuality[df_onehot$Quality.of.Life %in% c("1", "2", "3", "4", "5")] <- 1

df_onehot$excellentQuality <- 0
df_onehot$excellentQuality[df_onehot$Quality.of.Life %in% c("10")] <- 1

df_onehot$Quality.of.Life <- NULL
df_onehot$Ethnicity_Other <- NULL

df_onehot$Spouse_LivingWith <- NULL
df_onehot$Children_LivingWith <- NULL
df_onehot$Parent_LivingWith <- NULL

cols_to_factor <- c("isPartnered", "GoodEnglish", "EthnicFamiliar", "DissatisfiedLife",
                    "SatisfiedLife", "HousingSatisfaction", "highIncome", "lowIncome", "badQuality",
                    "excellentQuality")

df_onehot <- df_onehot %>%
  mutate_at(vars(all_of(cols_to_factor)), as.factor)

df_onehot$US_Live_percent <- df_onehot$US.Residency/df_onehot$Age

save(df_onehot, file = "processed_data/df_onehot.RData")

# badLogit1 <- glm(badQuality~., data=df_onehot, family=binomial(link="logit"))
# excellentLogit1 <- glm(excellentQuality~., data=df_onehot, family=binomial(link="logit"))




#####################
#####################

# Creating important plots

library(ggplot2)

# Krok 1: Sprawdź, które kolumny są typu factor i mają levels będące liczbami
df_transformed <- df_onehot %>%
  mutate(across(where(is.factor), function(col) {
    # Sprawdź czy wszystkie levels są liczbowe (czyli dają się skonwertować na numeric bez NA)
    levels_are_numeric <- all(!is.na(as.numeric(levels(col))))
    
    # Jeśli tak, konwertuj na ordered factor z posortowanymi levels
    if (levels_are_numeric) {
      factor(col, levels = sort(as.numeric(levels(col))), ordered = TRUE)
    } else {
      col  # Zostaw oryginalnie
    }
  }))

# Krok 2: Wybierz kolumny numeric lub ordered
ordered_numeric_columns <- select(df_transformed, where(~ is.numeric(.) || is.ordered(.)))

# Krok 3: Zamień ordered na numeric (dla korelacji Spearmana)
df_ready <- ordered_numeric_columns %>%
  mutate(across(everything(), ~ if (is.ordered(.)) as.numeric(.) else . ))

numeric_columns <- select(df_onehot, where(is.numeric))

factor_columns <- select(df_onehot, where(is.factor))

# Loop through each numeric column
# for (col_name in names(numeric_columns)) {
  
#   # Histogram plot
#   hist_plot <- ggplot(df_for_modeling, aes_string(x = col_name)) +
#     geom_histogram(bins = 30, fill = "skyblue", color = "black") +
#     labs(title = paste("Histogram of", col_name), x = col_name, y = "Count") +
#     theme_minimal() +
#     theme(
#       panel.background = element_rect(fill = "white"),
#       plot.background = element_rect(fill = "white"),
#       panel.grid.major = element_line(color = "grey90"),
#       panel.grid.minor = element_line(color = "grey95")
#     )
#   
#   # Save histogram
#   ggsave(filename = paste0("plots/histograms/", col_name, "_histogram.png"),
#          plot = hist_plot, width = 6, height = 4, dpi = 300)
#   
#   # Boxplot
#   box_plot <- ggplot(df_for_modeling, aes_string(y = col_name)) +
#     geom_boxplot(fill = "lightgreen") +
#     labs(title = paste("Boxplot of", col_name), y = col_name) +
#     theme_minimal() +
#     theme(
#       panel.background = element_rect(fill = "white"),
#       plot.background = element_rect(fill = "white"),
#       panel.grid.major = element_line(color = "grey90"),
#       panel.grid.minor = element_line(color = "grey95")
#     )
#   
#   # Save boxplot
#   ggsave(filename = paste0("plots/boxplots/", col_name, "_boxplot.png"),
#          plot = box_plot, width = 4, height = 6, dpi = 300)
# }

library(reshape2)


# Calculate correlation matrix
corr_matrix_pearson <- cor(df_onehot[, names(numeric_columns)], method = "pearson", use = "pairwise.complete.obs")
corr_matrix_spearman <- cor(df_ready, method = "spearman", use = "pairwise.complete.obs")

# Melt correlation matrix to long format
melted_corr_p <- melt(corr_matrix_pearson)

# Plot heatmap with ggplot2
correlation_heatmap <- ggplot(melted_corr_p, aes(X1, X2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name = "Correlation") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_line(color = "grey95"),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) +
  coord_fixed() +
  labs(title = "Correlation Heatmap")

ggsave(filename = "plots/heatmaps/numeric_correlation_heatmapOH.png",
       plot = correlation_heatmap,
       width = 12, height = 9, dpi = 300)

# Plot heatmap with ggplot2
melted_corr_s <- melt(corr_matrix_spearman)

correlation_heatmap <- ggplot(melted_corr_s, aes(X1, X2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name = "Correlation") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_line(color = "grey95"),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) +
  coord_fixed() +
  labs(title = "Correlation Heatmap")

ggsave(filename = "plots/heatmaps/spearman_correlation_heatmapOH.png",
       plot = correlation_heatmap,
       width = 12, height = 9, dpi = 300)

library(lsr)


sapply(factor_columns, function(x) length(levels(x)))
factor_data_filtered <- factor_columns[, sapply(factor_columns, function(x) length(levels(x)) > 1)]

n <- ncol(factor_data_filtered)
cramers_v_matrix <- matrix(NA, n, n)
colnames(cramers_v_matrix) <- rownames(cramers_v_matrix) <- colnames(factor_data_filtered)

for(i in 1:n){
  for(j in 1:n){
    tbl <- table(factor_data_filtered[[i]], factor_data_filtered[[j]])
    # Only calculate if table has more than one level in both dimensions
    if (nrow(tbl) > 1 && ncol(tbl) > 1) {
      cramers_v_matrix[i, j] <- cramersV(tbl)
    } else {
      cramers_v_matrix[i, j] <- NA
    }
  }
}

melted_cramers <- melt(cramers_v_matrix, na.rm = TRUE)

cramers_heatmap <- ggplot(melted_cramers, aes(X1, X2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "darkred", name = "Cramér's V") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_line(color = "grey95"),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  coord_fixed() +
  labs(title = "Cramér's V Heatmap (Factor Variables)")

ggsave(filename = "plots/heatmaps/cramers_correlation_heatmapOH.png",
       plot = cramers_heatmap,
       width = 16, height = 12, dpi = 300)


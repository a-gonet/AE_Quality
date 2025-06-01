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

# Step 5.2 Handling NA
for (col in names(data_transformed)) {
  # Check if column is numeric
  if (is.numeric(data_transformed[[col]])) {
    # Replace NAs with median (ignoring NAs in median computation)
    data_transformed[[col]][is.na(data_transformed[[col]])] <- median(data_transformed[[col]], na.rm = TRUE)
  }
}
sapply(data_transformed[, sapply(data_transformed, is.numeric)], function(x) sum(is.na(x)))

for (col in names(data_transformed)) {
  if (is.factor(data_transformed[[col]])) {
    # Remove NAs before finding mode
    non_na_values <- data_transformed[[col]][!is.na(data_transformed[[col]])]

    # Only proceed if non-NA values exist
    if (length(non_na_values) > 0) {
      most_common <- names(sort(table(non_na_values), decreasing = TRUE))[1]
      data_transformed[[col]][is.na(data_transformed[[col]])] <- most_common
    }
  }
}
sapply(data_transformed[, sapply(data_transformed, is.factor)], function(x) sum(is.na(x)))

# Step 5.3 Handling One Answer
df_for_modeling <- data_transformed[, sapply(data_transformed, function(col) {
  # Remove NA values
  vals <- na.omit(col)
  # Count unique values
  length(unique(vals)) > 1
})]

# Step 6. Modifying Quality.of.Life to smaller order
df_for_modeling <- df_for_modeling %>%
  mutate(Quality.of.Life = factor(Quality.of.Life,
                                  levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"),
                                  ordered = TRUE))

numeric_columns <- select(df_for_modeling, where(is.numeric))
factor_columns <- select(df_for_modeling, where(is.factor))


# Step 7. Creating important plots

library(ggplot2)

# Loop through each numeric column
for (col_name in names(numeric_columns)) {
  
  # Histogram plot
  hist_plot <- ggplot(df_for_modeling, aes_string(x = col_name)) +
    geom_histogram(bins = 30, fill = "skyblue", color = "black") +
    labs(title = paste("Histogram of", col_name), x = col_name, y = "Count") +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white"),
      plot.background = element_rect(fill = "white"),
      panel.grid.major = element_line(color = "grey90"),
      panel.grid.minor = element_line(color = "grey95")
    )
  
  # Save histogram
  ggsave(filename = paste0("plots/histograms/", col_name, "_histogram.png"),
         plot = hist_plot, width = 6, height = 4, dpi = 300)
  
  # Boxplot
  box_plot <- ggplot(df_for_modeling, aes_string(y = col_name)) +
    geom_boxplot(fill = "lightgreen") +
    labs(title = paste("Boxplot of", col_name), y = col_name) +
    theme_minimal() +
      theme(
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey90"),
        panel.grid.minor = element_line(color = "grey95")
      )
    
  # Save boxplot
  ggsave(filename = paste0("plots/boxplots/", col_name, "_boxplot.png"),
         plot = box_plot, width = 4, height = 6, dpi = 300)
}

library(reshape2)


# Calculate correlation matrix
corr_matrix <- cor(df_for_modeling[, names(numeric_columns)], use = "pairwise.complete.obs")

# Melt correlation matrix to long format
melted_corr <- melt(corr_matrix)

# Plot heatmap with ggplot2
correlation_heatmap <- ggplot(melted_corr, aes(Var1, Var2, fill = value)) +
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

ggsave(filename = "plots/heatmaps/numeric_correlation_heatmap.png",
       plot = correlation_heatmap,
       width = 8, height = 6, dpi = 300)

library(lsr)

factor_cols <- sapply(df_for_modeling, is.factor)
factor_data <- df_for_modeling[, factor_cols]

sapply(factor_data, function(x) length(levels(x)))
factor_data_filtered <- factor_data[, sapply(factor_data, function(x) length(levels(x)) > 1)]

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

cramers_heatmap <- ggplot(melted_cramers, aes(Var1, Var2, fill = value)) +
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

ggsave(filename = "plots/heatmaps/cramers_correlation_heatmap.png",
       plot = cramers_heatmap,
       width = 8, height = 6, dpi = 300)

# Step 8. Removing correlated columns
correlated_cols <- c("Helpful.Family", "Close.Family", "Helpful.Friends", "Close.Friends", "Community.Trust", "Get.Along", "Community.Shares.Values", "Close.knit.Community", "Helpful.Community", "Trust", "Loyalty", "Family.Pride", "Expression", "Spend.Time.Together", "Feel.Close", "Togetherness", "Family.Respect", "Similar.Values", "Successful.Family")
numeric_columns <- numeric_columns[, !(names(numeric_columns) %in% correlated_cols)]
df_for_modeling <- df_for_modeling[, !(names(df_for_modeling) %in% correlated_cols)]

# Step 9. Saving the final data
summary(df_for_modeling)

save(df_for_modeling, file = "processed_data/df_for_modeling.RData")

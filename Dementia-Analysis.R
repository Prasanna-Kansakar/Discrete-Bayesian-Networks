remove(list = ls())

# Installing necessary Packages
library(dplyr)
library(ggplot2)
library(gridExtra)
library(caret)
library(bnlearn)
library(Rgraphviz)
library(corrplot)

# Reading csv file dementia_data-MRI-features.csv
dementia_df <- read.csv('Data/dementia_data-MRI-features.csv')
colnames(dementia_df)[1] <- "SubjectId"  # This renames the first column
glimpse(dementia_df)
summary(dementia_df)
sapply(dementia_df, function(x) length(unique(x)))
per_subject_df <- dementia_df %>% group_by(SubjectId) %>%
  summarise(
    count = n(),
    group = length(unique(Group)),
    Visit = sum(Visit)
  )
per_subject_df
summary(per_subject_df)

unique(dementia_df$SES)

#Dealing with NaNs and ensuring the values are numeric
dementia_df <- dementia_df %>%
  mutate(
      Visit = as.numeric(Visit),
      MMSE =  ifelse(is.na(MMSE), 0, MMSE),
      Age = as.numeric(Age),
      EDUC = as.numeric(EDUC),
      eTIV = as.numeric(eTIV),
      nWBV = as.numeric(nWBV),
      ASF = as.numeric(ASF),
      SES = ifelse(is.na(SES), 0, SES),
  )

# Create individual histograms
hist_age <- ggplot(dementia_df, aes(x = Age)) +
  geom_histogram(bins = 15, fill = "skyblue", color = "black") +
  labs(x = "Age", y = "Frequency", title = "Age Histogram")

hist_educ <- ggplot(dementia_df, aes(x = EDUC)) +
  geom_histogram(bins = 12, fill = "skyblue", color = "black") +
  labs(x = "Years of education", y = "Frequency", title = "Education Histogram")

hist_mmse <- ggplot(dementia_df, aes(x = MMSE)) +
  geom_histogram(bins = 12, fill = "skyblue", color = "black") +
  labs(x = "Mini-Mental State Examination score", y = "Frequency", title = "MMSE Histogram")

hist_etiv <- ggplot(dementia_df, aes(x = eTIV)) +
  geom_histogram(bins = 15, fill = "skyblue", color = "black") +
  labs(x = "Estimated total intracranial volume", y = "Frequency", title = "eTIV Histogram")

hist_nwbv <- ggplot(dementia_df, aes(x = nWBV)) +
  geom_histogram(bins = 15, fill = "skyblue", color = "black") +
  labs(x = "Normalized whole-brain volume", y = "Frequency", title = "nWBV Histogram")

hist_asf <- ggplot(dementia_df, aes(x = ASF)) +
  geom_histogram(bins = 15, fill = "skyblue", color = "black") +
  labs(x = "Atlas scaling factor", y = "Frequency", title = "ASF Histogram")

hist_ses <- ggplot(dementia_df, aes(x = SES)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(x = "Group", y = "Count", title = "Count of Visits") +
  theme_minimal()
hist_group <- ggplot(dementia_df, aes(x = Group)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(x = "Group", y = "Count", title = "Count of Categories in Group") +
  theme_minimal()

hist_ses <- ggplot(dementia_df, aes(x = SES)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(x = "Group", y = "Count", title = "Count of Categories in Group") +
  theme_minimal()

hist_cdr <- ggplot(dementia_df, aes(x = CDR)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(x = "Group", y = "Count", title = "Count of Categories in Group") +
  theme_minimal()

# Arrange plots in a grid
grid.arrange(hist_age, hist_educ, hist_mmse, hist_etiv, hist_nwbv, hist_asf,
             hist_ses, hist_group, hist_ses, hist_cdr, ncol = 2)


# Function to discretize and format dementia data
discretize_continuous_data <- function(df) {
  df %>%
    mutate(
      Age = cut(Age, breaks = seq(60, 100, by = 10), labels = c("60-70", "70-80", "80-90", "90-100"), right = FALSE),
      Visit = case_when(
        Visit <= quantile(Visit, 0.025) ~ "low",
        Visit >= quantile(Visit, 0.975) ~ "high",
        TRUE ~ "medium"
      ),
      MMSE = case_when(
        MMSE <= quantile(MMSE, 0.025) ~ "low",
        MMSE >= quantile(MMSE, 0.975) ~ "high",
        TRUE ~ "medium"
      ),
      EDUC = case_when(
        EDUC <= quantile(EDUC, 0.025) ~ "low",
        EDUC >= quantile(EDUC, 0.975) ~ "high",
        TRUE ~ "medium"
      ),
      eTIV = case_when(
        eTIV <= quantile(eTIV, 0.025) ~ "low",
        eTIV >= quantile(eTIV, 0.975) ~ "high",
        TRUE ~ "medium"
      ),
      nWBV = case_when(
        nWBV <= quantile(nWBV, 0.025) ~ "low",
        nWBV >= quantile(nWBV, 0.975) ~ "high",
        TRUE ~ "medium"
      ),
      ASF = case_when(
        ASF <= quantile(ASF, 0.025) ~ "low",
        ASF >= quantile(ASF, 0.975) ~ "high",
        TRUE ~ "medium"
      )
    ) %>%
    mutate(
      Visit = as.factor(Visit),
      MMSE = as.factor(MMSE),
      EDUC = as.factor(EDUC),
      eTIV = as.factor(eTIV),
      nWBV = as.factor(nWBV),
      ASF = as.factor(ASF),
      Age = as.factor(Age),
      SES = as.factor(SES),
      CDR = as.factor(CDR),
      Group = as.factor(Group)
    ) %>%
    select(Visit, Age, EDUC, SES, MMSE, CDR, eTIV, nWBV, ASF, Group)
}

# Usage
discretized_data <- discretize_continuous_data(dementia_df)
str(discretized_data)

# Step 2: Create the network structure using Hill-Climbing
network_structure <- model2network("[Visit][Age|Visit][EDUC|Visit][SES|Visit][MMSE|Age:EDUC:SES:Visit][CDR|Age:EDUC:MMSE][eTIV|Age:SES][nWBV|eTIV][ASF|nWBV][Group|MMSE:CDR:Age:EDUC:ASF:Visit]")
graphviz.plot(network_structure, main = "Bayesian Network Structure")

# Step 3: Fit the model
fitted_model <- bn.fit(network_structure, discretized_data, method = "bayes")

calculate_quantiles <- function(df) {
  quantiles <- list(
    Visit = quantile(df$Visit, c(0.025, 0.975)),
    MMSE = quantile(df$MMSE, c(0.025, 0.975)),
    EDUC = quantile(df$EDUC, c(0.025, 0.975)),
    eTIV = quantile(df$eTIV, c(0.025, 0.975)),
    nWBV = quantile(df$nWBV, c(0.025, 0.975)),
    ASF = quantile(df$ASF, c(0.025, 0.975))
  )
  return(quantiles)
}

# Function to discretize data
discretize_single_data <- function(data, quantiles) {
  # Discretize Age into 10-year intervals
  data$Age <- cut(data$Age, breaks = seq(60, 100, by = 10), labels = c("60-70", "70-80", "80-90", "90-100"), right = FALSE)

  # Discretize other variables based on quantiles
  data$Visit <- case_when(
    data$Visit <= quantiles$Visit[1] ~ "low",
    data$Visit >= quantiles$Visit[2] ~ "high",
    TRUE ~ "medium"
  )

  data$MMSE <- case_when(
    data$MMSE <= quantiles$MMSE[1] ~ "low",
    data$MMSE >= quantiles$MMSE[2] ~ "high",
    TRUE ~ "medium"
  )

  data$EDUC <- case_when(
    data$EDUC <= quantiles$EDUC[1] ~ "low",
    data$EDUC >= quantiles$EDUC[2] ~ "high",
    TRUE ~ "medium"
  )

  data$eTIV <- case_when(
    data$eTIV <= quantiles$eTIV[1] ~ "low",
    data$eTIV >= quantiles$eTIV[2] ~ "high",
    TRUE ~ "medium"
  )

  data$nWBV <- case_when(
    data$nWBV <= quantiles$nWBV[1] ~ "low",
    data$nWBV >= quantiles$nWBV[2] ~ "high",
    TRUE ~ "medium"
  )

  data$ASF <- case_when(
    data$ASF <= quantiles$ASF[1] ~ "low",
    data$ASF >= quantiles$ASF[2] ~ "high",
    TRUE ~ "medium"
  )
  return(data)
}


# Step 4: Perform conditional probability queries
# Perform inference to calculate the probability distribution of 'Group'
# Test 1: P(Group=nondemented | visit=2, Age=88,
# EDUC=14, SES=2, MMSE=30, CDR=0, eTIV=2004,
# nWBV=0.681, ASF=0.876)
test_no_1 <- list(Visit = 2, Age = 88, EDUC = 14, SES = 2, MMSE = 30, CDR = 0, eTIV = 2004, nWBV = 0.681, ASF = 0.876)
quantiles <- calculate_quantiles(dementia_df)
test_no_1_df <- as.data.frame(test_no_1)
test_no_1_evidence <- as.list(discretize_single_data(test_no_1_df, quantiles))
test_no_1_evidence$SES <- as.factor(test_no_1_evidence$SES)
test_no_1_evidence$CDR <- as.factor(test_no_1_evidence$CDR)
print(test_no_1_evidence)

cpquery(fitted_model, event = (Group == "Nondemented"), evidence = test_no_1_evidence, method = "lw")

#Test 2:   P(Group=demented | visit=3, Age=80,
# EDUC=12, MMSE=22, CDR=0.5, eTIV=1698, nWBV=0.701,
# ASF= 1.034)
test_no_2 <- list(Visit = 2, Age = 80, EDUC = 12, MMSE = 22, CDR = 0.5, eTIV = 1698, nWBV = 0.701, ASF = 1.034)
test_no_2_df <- as.data.frame(test_no_2)
test_no_2_evidence <- as.list(discretize_single_data(test_no_1_df, quantiles))
test_no_2_evidence$CDR <- as.factor(test_no_2_evidence$CDR)
print(test_no_2_evidence)

cpquery(fitted_model, event = (Group == "Demented"), evidence = test_no_2_evidence, method = "lw")





# README

## Overview
This R script performs data preprocessing, visualization, and Bayesian network analysis on a dementia dataset with MRI features. The script focuses on understanding relationships between various features, discretizing data, and making probabilistic inferences using Bayesian networks.

## Features


### Bayesian Network Analysis
1. **Network Structure Learning:**
    - Constructs a Bayesian network structure using domain knowledge.
    - Visualizes the network structure with `Rgraphviz`.

2. **Model Fitting:**
    - Fits the Bayesian network model to the data using the `bnlearn` package.

3. **Conditional Probability Queries:**
    - Calculates the probability of outcomes (e.g., dementia group classification) based on given evidence.
    - Uses the `cpquery` function for inference with probabilistic evidence.

## Prerequisites

### Required R Packages
- `dplyr`
- `ggplot2`
- `gridExtra`
- `caret`
- `bnlearn`
- `Rgraphviz`
- `corrplot`

### Installation
```R
install.packages(c("dplyr", "ggplot2", "gridExtra", "caret", "bnlearn", "Rgraphviz", "corrplot"))
```

### Dataset
- The script requires the `dementia_data-MRI-features.csv` file in the `Data` folder.

## Instructions
1. **Load Necessary Libraries:**
    - Ensure all required packages are installed.

2. **Prepare Data:**
    - Place the `dementia_data-MRI-features.csv` file in a folder named `Data`.

3. **Run the Script:**
    - Execute the script to perform data cleaning, visualization, Bayesian modeling, and inference.

4. **Outputs:**
    - Visualization plots.
    - Bayesian network structure.
    - Probabilities from conditional queries.

## Key Sections of the Script

### Data Preprocessing
- Prepares the dataset for analysis by converting variables to numeric or categorical formats and imputing missing values.

### Visualization
- Provides insights into the distributions of key variables using histograms and bar plots.

### Bayesian Network Construction
- Defines the network structure based on prior knowledge:
  ```
  [Visit][Age|Visit][EDUC|Visit][SES|Visit][MMSE|Age:EDUC:SES:Visit][CDR|Age:EDUC:MMSE]
  [eTIV|Age:SES][nWBV|eTIV][ASF|nWBV][Group|MMSE:CDR:Age:EDUC:ASF:Visit]
  ```

- Fits the network using the `bn.fit()` function.

### Conditional Probability Queries
- Example queries:
    1. Probability of being `Nondemented` given specific evidence:
       ```R
       cpquery(fitted_model, event = (Group == "Nondemented"), evidence = test_no_1_evidence, method = "lw")
       ```

    2. Probability of being `Demented` given specific evidence:
       ```R
       cpquery(fitted_model, event = (Group == "Demented"), evidence = test_no_2_evidence, method = "lw")
       ```

## Customization
- Modify the Bayesian network structure based on additional domain knowledge.
- Adjust discretization thresholds or ranges for variables to suit specific requirements. .


# Island Scrub Jay Habitat Data (2008)

## 📄 Dataset Overview
This dataset was collected during the fall of 2008 on Santa Cruz Island, California, focusing on the habitat of **Island Scrub Jays**. The data includes environmental and spatial features observed at **307 survey sites** and provides a foundation for analyzing the habitat preferences and predicting the presence of Island Scrub Jays.

### Dataset Summary
- **Total Dataset:** 5625 observations
- **Variables:**
  1. **isj:** Presence of Island Scrub Jay (`1`: Present, `0`: Absent)
  2. **x:** X-coordinate of the survey site
  3. **y:** Y-coordinate of the survey site
  4. **elev:** Elevation (meters above sea level)
  5. **forest:** Proportion of forest cover in the survey site
  6. **chap:** Proportion of chaparral (shrubland) cover in the survey site
- **Missing Data:** 
  - Observations containing NA: **5322 rows**

### Data Characteristics
- The dataset aims to explore the spatial and environmental characteristics of Island Scrub Jay habitats.
- Missing values account for a significant portion of the dataset and require thoughtful preprocessing.

---

## 🎯 Analysis Goals

### Goal 1: Understand Terrain Preferences of Scrub Jays
- Explore the relationship between the presence of Island Scrub Jays (`isj`) and environmental variables such as **elevation**, **forest proportion**, and **chaparral proportion** to identify preferred terrain types.

### Goal 2: Predict Scrub Jay Presence Across the Island
Build and compare two predictive models based on the dataset, including both existing and newly defined variables:
1. **Model with Lowest Misclassification Rate:** 
   - Identify the combination of newly defined variables that minimizes the misclassification rate.
   - Use these variables along with existing variables to build a predictive model.
2. **Model with Lowest AIC:**
   - Evaluate combinations of new and existing variables.
   - Select the variable combination with the lowest Akaike Information Criterion (AIC).
   - Use these variables to build a second predictive model.
---
## 🛠️ Analysis Workflow

### Step 1: Generate New Variables
- Add **polynomial terms** and **interaction terms** to the dataset to account for potential non-linear relationships and interactions between variables.
- Ensure that newly defined variables provide meaningful information regarding Scrub Jay presence.

### Step 2: Split Dataset
- Split the data into **train (70%)** and **test (30%)** sets.

### Step 3: Model Selection
1. **Model 3-1: Variables Minimizing Misclassification Rate**
   - Use a for-loop to test various combinations of the newly defined variables while retaining the original variables.
   - Fit models on the training set and evaluate misclassification rates on the test set to identify the optimal combination.
   
2. **Model 3-2: Variables Minimizing AIC**
   - Use a for-loop to test combinations of new variables (excluding original variables).
   - Fit models on the training set and select the combination that results in the lowest AIC value.

### Step 4: Hypothesis Testing
- Conduct hypothesis tests to identify variables with significant effects on the response variable (`isj`).

### Step 5: Final Model and Visualization
- Compare the results of **Model 3-1** and **Model 3-2**.
- Select the model with the lowest misclassification rate.
- Visualize the results and apply the selected model to the dataset of 2484 observations for final predictions.




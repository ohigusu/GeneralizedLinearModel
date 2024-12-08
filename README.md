# Island Scrub Jay Habitat Data (2008)

## 📄 Dataset Overview

This dataset was collected during the fall of 2008 on Santa Cruz Island, California, focusing on the habitat of **Island Scrub Jays**. The data includes environmental and spatial features observed at **307 survey sites** and provides a foundation for analyzing the habitat preferences and predicting the presence of Island Scrub Jays.

### Dataset Summary
- **Total Records:** 5625 observations
- **Variables:**
  1. **isj:** Presence of Island Scrub Jay (`1`: Present, `0`: Absent)
  2. **x 위치:** X-coordinate of the survey site
  3. **y 위치:** Y-coordinate of the survey site
  4. **고도:** Elevation (meters above sea level)
  5. **forest 비율:** Proportion of forest cover in the survey site
  6. **chaparral 비율:** Proportion of chaparral (shrubland) cover in the survey site
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

## 🧰 Methodology and Workflow

### Data Preprocessing
- Handle missing values (5322 rows with NA).
- Create new derived variables based on the given features to capture additional patterns.

### Exploratory Data Analysis (EDA)
- Analyze relationships between variables and scrub jay presence.
- Visualize spatial distribution using **x** and **y** coordinates.

### Model Development
1. **Misclassification-Optimized Model:**
   - Test combinations of new variables to find those that minimize misclassification rate.
   - Build and validate the model using the best variable set.
2. **AIC-Optimized Model:**
   - Evaluate combinations of new and existing variables.
   - Select the variable combination with the lowest AIC to develop the model.

### Evaluation Metrics
- Accuracy, precision, recall, and F1-score for classification models.
- Comparison of misclassification rate and AIC between models.

---

## 📂 Repository Structure


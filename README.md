# Business Intelligence & Advanced Analytics — End-to-End Project (R & Python)

## Overview
This repository contains a comprehensive Business Intelligence and Advanced Analytics project developed across multiple academic modules.  
The project covers the full analytics lifecycle, from raw data exploration and big data preprocessing to predictive modeling, text & social analytics, and deep learning.

The main use case throughout the project is **flight delay analysis and prediction**, enriched with external datasets such as weather, passenger volumes, aircraft information, and social/sentiment data.

The work is implemented using **R and Python**, combining classical analytics techniques with machine learning and deep learning approaches.

---

## Project Structure

---

## Data
Raw datasets are **not included** in this repository to avoid large files and potential licensing restrictions.

The project relies on publicly available datasets related to:
- Flight operations and delays
- Weather conditions
- Aircraft and airline information
- Passenger volumes
- Social media / satisfaction data

To reproduce results, datasets should be placed locally in a `data/` folder and file paths adjusted accordingly.

---

## Modules Breakdown & Conclusions

---

### Module 1 — Dataset Exploration (R)
**Objective:**  
Understand the structure, size, and quality of the raw flight datasets.

**Key Work:**
- Initial loading of flight data
- Inspection of dimensions, data types, missing values
- Basic descriptive statistics

**Conclusion:**  
This step highlighted the **scale and complexity** of the data and revealed the need for extensive preprocessing.  
It established a solid foundation for later feature engineering and modeling by identifying missing values and data inconsistencies early.

---

### Module 2 — Big Data & Analytics (R)
**Objective:**  
Integrate and preprocess multiple large datasets to enrich flight data.

**Key Work:**
- Ingestion of auxiliary datasets (weather, aircraft inventory, passengers, employees, airport coordinates)
- Data cleaning and normalization
- Feature engineering (seat capacity, passenger metrics, weather indicators)

**Conclusion:**  
Combining heterogeneous datasets significantly increased the **analytical value** of the data.  
This module demonstrates the importance of **data integration at scale** and careful preprocessing in Big Data environments.

---

### Module 3 — Data Warehousing & Enterprise Reporting (R)
**Objective:**  
Create a structured, analytics-ready dataset following data warehousing principles.

**Key Work:**
- Monthly processing of flight data
- Creation of engineered features (concurrent flights, aircraft age, historical delay rates)
- Lookup tables by carrier, airport, time block
- Generation of a consolidated dataset (`train_val.csv`)

**Conclusion:**  
This module represents the **core data engineering layer** of the project.  
A well-designed analytical dataset enables reliable downstream modeling and ensures consistency across analyses.

---

### Module 4 — Predictive Analytics (R)
**Objective:**  
Predict flight delays using classical machine learning models.

**Key Work:**
- Feature selection and preprocessing
- Model training and evaluation:
  - Logistic Regression
  - Decision Trees
  - Naive Bayes
  - Neural Networks (nnet)
  - Random Forest (extended version)
- Model comparison using accuracy and confusion matrices
- Parallel processing for performance optimization

**Conclusion:**  
Predictive performance improves substantially when combining operational, weather, and historical features.  
Tree-based and neural models outperform simpler baselines, highlighting the **non-linear nature** of flight delay prediction.

---

### Module 5 — Text & Social Analytics (Python + R)
**Objective:**  
Incorporate unstructured data and social information into the analytical pipeline.

**Key Work:**
- Sentiment analysis of social media data (Python / Colab)
- Integration of satisfaction and sentiment metrics with structured flight data (R)
- Exploratory analysis linking social signals with operational performance

**Conclusion:**  
Text and social data provide **complementary insights** not captured by structured variables alone.  
This module demonstrates how unstructured data can enrich traditional BI analyses.

---

### Module 6 — Predictive Maintenance (Python)
**Objective:**  
Apply predictive modeling techniques to maintenance-related problems.

**Key Work:**
- Preparation of structured inputs for maintenance prediction
- Neural network–based modeling
- Evaluation of classification performance

**Conclusion:**  
Predictive maintenance approaches can anticipate operational issues before they occur.  
This module illustrates how analytics can support **proactive decision-making** beyond descriptive reporting.

---

### Module 7 — Deep Learning with LSTM (Python)
**Objective:**  
Explore deep learning techniques for flight delay prediction.

**Key Work:**
- Data scaling and encoding
- LSTM-based binary classification
- Evaluation on:
  - Original (imbalanced) dataset
  - Upsampled (balanced) dataset
- Comparison of performance under different class distributions

**Conclusion:**  
Balancing the dataset improves model stability and recall for delayed flights.  
While each record is modeled as a single-step sequence, the module demonstrates the **applicability of deep learning** to complex operational datasets.

---

## Overall Conclusions
- Flight delay prediction benefits from **multi-source data integration**
- Feature engineering and data warehousing are as important as model choice
- Classical machine learning models remain strong baselines
- Text, social, and deep learning methods add additional analytical depth
- The project showcases an **end-to-end BI & analytics pipeline**, from raw data to advanced modeling

---

## Tech Stack
- **R**: tidyverse, data.table, caret, corrplot, nnet
- **Python**: pandas, numpy, scikit-learn, TensorFlow/Keras
- **Tools**: Jupyter Notebook, Google Colab

---

## Author
Biel Cantera


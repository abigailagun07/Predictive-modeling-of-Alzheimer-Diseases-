# Predictive Modeling of Alzheimer's Disease
This repository contains code and analysis for a predictive modeling project on Alzheimer's disease (AD) using R. The goal is to analyse AD-related features and develop a model to predict dementia status.

## Dataset 
The dataset contains 317 observations with the following features:
- Gender: Male/Female
- Age: The age of the subject 
- Education (EDUC): Number of years of education
- Socioeconomic status (SES)
- Mini-Mental State Exam (MMSE): Cognitive function test score
- Clinical Dementia Rating (CDR): Dementia severity score 
- Estimated total intracranial volume (eTIV)
- Normalised whole brain volume (nWBV) 
- Atlas scaling factor (ASF)
- Group: Demented/Non-demented (response variable)

## Analysis
The analysis workflow includes:
- Data cleaning and preprocessing
- Exploratory data analysis and visualisation
  - Summary statistics
  - Correlation analysis 
- K-means clustering
- Feature selection using wrapper methods
- Logistic regression modeling with cross-validation

## Key findings:
- CDR, MMSE, education, and brain volume measures are important predictors
- K-means reveals two distinct clusters separating demented and non-demented
- Logistic model achieves 100% accuracy but potential for overfitting

## Contents
- R scripts for data cleaning, EDA, modeling
- Dataset
- README file
- Report PDF

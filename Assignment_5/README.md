# Assignment 5 Description: Machine Learning-Based Matching Analysis

## Value
**15% of Final Grade**

## Description
In this assignment, you will perform a machine learning-based matching analysis using the Can Path Student Dataset. Matching is a technique often used to balance datasets or control for confounding variables, especially in causal inference. This assignment will require you to apply a machine learning approach to match cases and controls (or equivalent groups), analyze the quality of the matches, and interpret the results.

## Assignment Objectives
By completing this assignment, you will:
- Learn to apply machine learning algorithms for matching analysis.
- Develop skills in preprocessing and feature engineering for matching.
- Assess the quality of matches using statistical and visual methods.
- Interpret and report findings from a matching analysis.

## Assignment Tasks

### Data Exploration and Preprocessing
1. Load and explore the Can Path Student Dataset.
2. Identify key variables for matching, ensuring they are relevant to your analysis goals.
3. Perform preprocessing, including scaling, encoding, or imputing missing values, to prepare data for the matching algorithm.

### Machine Learning-Based Matching
1. Apply the Propensity Score Matching (PSM) method.
2. Train the model to predict group membership based on the selected features.
3. Use the model’s predictions or distance measures to match cases and controls (or equivalent groups).

### Match Quality Assessment
1. Evaluate the quality of the matches using metrics such as standardized mean differences, balance statistics, or graphical methods.
2. Document improvements in balance between groups after matching.

### Analysis of Matched Data
1. Conduct an exploratory or inferential analysis on the matched dataset.
2. Compare results between the unmatched and matched datasets to highlight the impact of the matching process.

### Interpretation and Reporting
1. Summarize the steps and results of your matching analysis.
2. Discuss the implications of your findings and any limitations of the matching approach.

## Deliverables

### Analysis Report
Submit a structured report in `.Rmd` format and upload it to your personal GitHub page in a new repository. The report should include:
- A description of the dataset and variables selected for matching.
- Evaluation of match quality and analysis results on the matched dataset.
- Interpretation of findings and a discussion of limitations.

### Code and Documentation
- Submit all scripts or notebooks used for the analysis.
- Ensure the code is well-documented, with comments explaining each step.

### Visualizations
- Include visual representations of match quality and analysis results.

## Assessment Criteria
- **Dataset Preparation (15%)**
  - Completeness and accuracy in preprocessing the data for matching.
- **Matching Implementation (30%)**
  - Correctness and rigor in applying the machine learning-based matching technique.
- **Match Quality Assessment (20%)**
  - Thoroughness in evaluating the quality of matches and achieving balance.
- **Analysis of Matched Data (20%)**
  - Depth and clarity of insights derived from the matched dataset.
- **Documentation and Presentation (15%)**
  - Quality of the report, code, and visualizations, with clear and professional formatting.

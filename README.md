# 🏃‍♂️ Calorie Burn Predictor — Comprehensive R Machine Learning Revamp

[![R 4.6.1](https://img.shields.io/badge/Language-R%204.6.1-blue.svg)](https://www.r-project.org/)
[![Framework-Shiny](https://img.shields.io/badge/Framework-Shiny%20Modular-teal.svg)](https://shiny.posit.co/)
[![Quality-Assurance](https://img.shields.io/badge/QA%20Tests-53%2F53%20Passing-brightgreen.svg)]()
[![Zero-Data-Leakage](https://img.shields.io/badge/Validation-Zero%20Data%20Leakage-success.svg)]()
[![License](https://img.shields.io/badge/Academic-Project%20Review-orange.svg)]()

A complete, production-grade R programming revamp of the **Calorie Burn Predictor** academic project. Built from the ground up to demonstrate rigorous machine-learning methodology, strict data leakage prevention, reproducible 5-fold cross-validation, and dual delivery via an interactive **Shiny Web Application** and a fully standalone **Terminal Console Application**.

---

## 📋 Table of Contents

1. [Academic Project Overview](#-academic-project-overview)
2. [Key Architecture & Zero Data Leakage](#-key-architecture--zero-data-leakage)
3. [Project Directory Layout](#-project-directory-layout)
4. [Machine Learning Methodology & Algorithm Selection](#-machine-learning-methodology--algorithm-selection)
5. [Empirical Evaluation & Cross-Validation Results](#-empirical-evaluation--cross-validation-results)
6. [Physiological Feature Analysis & High R² Investigation](#-physiological-feature-analysis--high-r-investigation)
7. [Execution Modes](#-execution-modes)
   - [Mode A: Interactive Shiny Web Application](#mode-a-interactive-shiny-web-application)
   - [Mode B: Standalone Interactive Terminal Console](#mode-b-standalone-interactive-terminal-console)
8. [Automated Quality Assurance & Test Suites](#-automated-quality-assurance--test-suites)
9. [Report Generation & Export Artifacts](#-report-generation--export-artifacts)
10. [Ethical & Non-Medical Disclaimer](#-ethical--non-medical-disclaimer)

---

## 🧠 Academic Project Overview

Estimating human energy expenditure during physical exercise is a cornerstone problem in exercise physiology, sports medicine, and metabolic analytics. 

### Dataset Provenance & Attributes
The dataset (`calories.csv`) originates from open biometric exercise studies (widely archived on Kaggle and academic repositories) capturing physical exertion metrics during structured exercise trials.
- **Records**: 15,000 complete observations (0 missing values).
- **Target Variable**: `Calories` (continuous numeric, units: kcal, mean: 89.54, range: 1.0 – 314.0 kcal).
- **Predictor Variables**:
  - `Gender`: Binary categorical factor (`male` / `female`).
  - `Age`: Numeric integer (range: 20 – 79 years).
  - `Height`: Numeric continuous (range: 123.0 – 222.0 cm).
  - `Weight`: Numeric continuous (range: 36.0 – 132.0 kg).
  - `Duration`: Numeric continuous (range: 1.0 – 30.0 minutes).
  - `Heart_Rate`: Numeric continuous (range: 67.0 – 128.0 bpm).
  - `Body_Temp`: Numeric continuous (range: 37.1 – 41.5 °C).
  - `BMI`: Derived continuous feature ($\text{Weight} / (\text{Height}/100)^2$).
- **Excluded Attribute**: `User_ID` (unique identifier dropped from model training to prevent leakage).

### Why This Revamp?
The original legacy codebase suffered from multiple structural, statistical, and architectural defects:
- **Flawed Algorithm Formulation**: Legacy documentation described using *Logistic Regression* for predicting continuous caloric expenditure (which is mathematically invalid; the code actually ran a basic linear regression labeled as logistic).
- **Data Leakage**: The participant identifier `User_ID` was included in predictive matrices, introducing artificial overfitting and spurious correlations.
- **Missing Validation Rigor**: No cross-validation was performed; models were evaluated without train/test splits or on arbitrary subsets.
- **Brittle Monolithic Code**: Code duplication between console scripts and UI prevented shared validation and unified model inference.

### Revamped System Goals
- **Strictly R-Native**: 100% implemented in R without introducing Python, JavaScript frameworks, Node.js, or external database stacks.
- **Shared Predictive Engine**: Both the Shiny web UI and the standalone console script utilize the exact same underlying R pipeline (`R/`).
- **Reproducible Cross-Validation**: 5-fold cross-validation on paired folds with zero data leakage.
- **Empirical Model Selection**: Objective, metric-driven model selection based on cross-validation RMSE.

---

## 🛡️ Key Architecture & Zero Data Leakage

The revamped architecture enforces strict data science best practices:

```
[Raw Dataset: calories.csv] (15,000 Obs)
          │
          ▼
[R/data.R] ── Validation: Check schema, column types, NA checks
          │
          ▼
[R/preprocessing.R] ── Clean & Isolate: Drop User_ID, standardize Gender factors
          │
          ▼
[R/feature_engineering.R] ── Derive BMI = Weight / (Height/100)² with defensive zero guards
          │
          ▼
[R/evaluation.R] ── 80/20 Train-Test Partition (12,000 Train / 3,000 Untouched Test)
          │
          ├──► 5-Fold Cross-Validation (Identical paired folds, leakage-free)
          │    ├── Multiple Linear Regression (OLS Baseline)
          │    ├── Decision Tree Regression (rpart)
          │    └── Random Forest Regression (80 Bagged Trees)
          │
          ├──► Model Selection: Random Forest selected (Lowest CV RMSE = 4.933 kcal)
          │
          └──► Final Verification: Single untouched evaluation on 3,000 test observations
```

### Key Data Leakage Safeguards
1. **User_ID Isolation**: `User_ID` uniquely indexes survey participants. It is dropped from all predictive models and correlation analyses.
2. **Defensive Preprocessing**: Preprocessing parameters and factor encodings are established strictly on training folds and never computed over the combined dataset.
3. **Pristine Test Set**: The 3,000-record test partition was held out completely until cross-validation finalized model hyper-parameters.

---

## 📁 Project Directory Layout

```
Calorie-Prediction-using-R/
├── app.R                          # Main Interactive Shiny Application
├── standalone.R                   # Standalone Terminal Console Application
├── requirements.R                 # Package management & local environment loader
├── calories.csv                   # Verified 15,000-observation exercise dataset
├── README.md                      # Comprehensive academic documentation
│
├── R/                             # Shared Core Machine Learning Pipeline
│   ├── data.R                     # Ingestion, validation, missing value filtering
│   ├── preprocessing.R            # Data cleaning, factor alignment, leakage prevention
│   ├── feature_engineering.R      # BMI derivation & physiological metrics
│   ├── validation.R               # Runtime input validation & error diagnostics
│   ├── models.R                   # Training wrappers (LM, Decision Tree, Random Forest)
│   ├── evaluation.R               # 5-fold cross-validation & metric computation
│   ├── prediction.R               # Unified multi-model prediction engine & OLS intervals
│   ├── visualizations.R           # Publication-ready ggplot2 graphic generators
│   └── reporting.R                # Multi-format report exporters (PDF, TXT, CSV)
│
├── modules/                       # Shiny Web UI & Server Modules
│   ├── mod_dashboard.R            # System overview, KPIs, dataset sample table
│   ├── mod_prediction.R           # Interactive inputs, multi-model predictions, reset
│   ├── mod_performance.R          # CV & Test metrics tables, residual error plots
│   ├── mod_insights.R             # Heatmaps, feature importance, population context
│   └── mod_reports.R              # Download handlers for PDF, TXT, and CSV summaries
│
├── models/                        # Serialized Model Artifacts
│   ├── trained_models.rds         # Cached trained model bundle (LM, Tree, RF)
│   └── evaluation_summary.rds     # Cached 5-fold CV & test evaluation metrics
│
├── plots/                         # Generated High-Resolution Visualizations
│   ├── corr_heatmap.png           # Feature correlation heatmap (User_ID excluded)
│   ├── feature_importance.png     # Random Forest IncNodePurity ranking
│   ├── model_comparison.png       # Test RMSE benchmark across architectures
│   ├── prediction_vs_actual.png   # Observed vs predicted regression scatter
│   └── residual_distribution.png  # Error density distribution vs normal reference
│
├── reports/                       # Generated Diagnostic Reports
│   └── evaluation_report.txt      # Formatted 7-section statistical summary
│
├── tests/                         # Automated Quality Assurance Suites
│   ├── test_structure.R           # File layout and dependency integrity
│   ├── test_pipeline.R            # 14-point ML pipeline & cross-validation tests
│   ├── test_standalone.R          # 19-point interactive console robustness tests
│   └── test_shiny.R               # 20-point Shiny web UI & download handler tests
│
└── legacy/                        # Preserved Original Codebase (Audited in Stage 1)
    ├── app.R                      # Original legacy Shiny app
    ├── calorie_prediction.R       # Original legacy console script
    └── README.md                  # Original legacy README
```

---

## 🔬 Machine Learning Methodology & Algorithm Selection

Three distinct regression paradigms were selected to evaluate exercise energy expenditure:

### 1. Multiple Linear Regression (OLS)
$$\text{Calories} = \beta_0 + \beta_1 \text{Gender} + \beta_2 \text{Age} + \beta_3 \text{Height} + \beta_4 \text{Weight} + \beta_5 \text{Duration} + \beta_6 \text{Heart\_Rate} + \beta_7 \text{Body\_Temp} + \beta_8 \text{BMI} + \epsilon$$
- **Role**: Serves as the transparent parametric baseline.
- **Characteristics**: Fast, completely interpretable, and supports classical Ordinary Least Squares (OLS) prediction intervals.
- **Limitation**: Assumes additive linear effects; struggles to capture non-linear metabolic fatigue curves.

### 2. Decision Tree Regression (`rpart`)
- **Role**: Non-parametric hierarchical tree baseline.
- **Characteristics**: Performs recursive binary splitting to segment feature space into homogenous regions. Captures basic threshold interactions (e.g. high duration + elevated heart rate).
- **Limitation**: Prone to variance and step-function discontinuities.

### 3. Random Forest Regression (`randomForest`, 80 Trees)
- **Role**: State-of-the-art non-linear ensemble architecture.
- **Characteristics**: Aggregates 80 bootstrap-aggregated (bagged) de-correlated decision trees. Evaluates random subsets of predictors at each split ($m_{\text{try}} = 2$).
- **Advantage**: Superior modeling of multi-factor physiological interactions (duration $\times$ intensity $\times$ thermogenesis) while resisting overfitting.

---

## 📊 Empirical Evaluation & Cross-Validation Results

All models were evaluated using **5-Fold Cross-Validation** on identical training partitions (12,000 observations), followed by a single benchmark on the untouched test partition (3,000 observations).

### 1. 5-Fold Cross-Validation Results (Training Set: 12,000 Obs, ntree = 80)

| Model Architecture | CV MAE (Mean ± SD) | CV RMSE (Mean ± SD) | CV R² (Mean ± SD) | Status |
| :--- | :---: | :---: | :---: | :---: |
| **Random Forest (80 trees)** | **3.049 ± 0.175 kcal** | **4.862 ± 0.345 kcal** | **0.9939 ± 0.0008** | 🏆 **Selected Best** |
| **Linear Regression (OLS)** | 8.158 ± 0.090 kcal | 11.091 ± 0.169 kcal | 0.9684 ± 0.0007 | Baseline |
| **Decision Tree (rpart)** | 9.764 ± 0.138 kcal | 13.085 ± 0.281 kcal | 0.9560 ± 0.0018 | Comparative |

> **Model Selection Decision**: **Random Forest** achieved the lowest cross-validation RMSE ($4.862$ kcal) and highest cross-validation $R^2$ ($0.9939$), confirming its selection as the primary operational model.

### 2. Independent Final Test Evaluation (Untouched: 3,000 Obs)

| Model Architecture | Test MAE | Test RMSE | Test R² | Out-of-Sample Generalization |
| :--- | :---: | :---: | :---: | :--- |
| **Random Forest** | **2.773 kcal** | **4.479 kcal** | **0.9949** | Exceptional generalization |
| **Linear Regression** | 7.908 kcal | 10.816 kcal | 0.9703 | Reliable linear approximation |
| **Decision Tree** | 9.489 kcal | 12.849 kcal | 0.9580 | Piecewise approximation |

---

## 🔍 Physiological Feature Analysis & High R² Investigation

During evaluation, Random Forest achieved an $R^2 > 0.99$. In machine learning, $R^2 > 0.98$ often flags target leakage. We conducted a rigorous investigation to verify data integrity:

### 1. Thermodynamic First Principles
In exercise physiology, energy expenditure (calories) is governed by mechanical work and cardiovascular exertion:
$$\text{Total Caloric Burn} \approx \text{Exercise Duration} \times \text{Metabolic Work Rate (Heart Rate, Temp)}$$

### 2. Correlation Ranking with Caloric Burn (User_ID Dropped)
- **Workout Duration**: $r = +0.9554$ (Cumulative mechanical volume)
- **Heart Rate**: $r = +0.8979$ (Cardiovascular intensity & metabolic demand)
- **Body Temperature**: $r = +0.8246$ (Thermogenic heat production)
- **Age**: $r = +0.1544$
- **BMI**: $r = +0.0565$
- **Weight**: $r = +0.0355$
- **Height**: $r = +0.0175$

### 3. Scientific Caution: Correlation vs. Causation
While Duration ($r = +0.9554$) and Heart Rate ($r = +0.8979$) are strong empirical predictors of caloric burn due to thermodynamic relationships ($\text{Work} = \text{Intensity} \times \text{Duration}$), correlation does **not** equal causal proof. 
- A high $R^2$ alone does not prove complete absence of latent bias or experimental protocol artifacts.
- In observational fitness datasets, individuals monitored for longer durations with elevated heart rates naturally expend more energy; however, unobserved confounders (e.g., ambient humidity, metabolic efficiency, VO₂ max, hydration) remain unmodeled.
- The models provide statistical predictive approximations, not mechanistic or clinical causality.

---

## 💻 Execution Modes

### Mode A: Interactive Shiny Web Application

The Shiny application provides a clean, responsive fitness analytics interface with a muted green/teal aesthetic:

```bash
# Launch from project root
Rscript -e 'shiny::runApp(port = 3838)'
```
*Or open `app.R` in RStudio and click **Run App**.*

#### Application Features:
- **Dashboard**: High-level dataset metrics, architecture summary, and live dataset preview.
- **Calorie Prediction**: Interactive sliders/numeric inputs, architecture selector (Random Forest, Linear Regression, Decision Tree), instant prediction cards, burn rate ($\text{kcal/min}$), BMI categorization, OLS 95% prediction intervals, and a **↺ Reset Inputs** button.
- **Model Performance**: Side-by-side 5-fold cross-validation and final test tables, interactive RMSE comparison chart, residual error distribution, and observed-vs-predicted plots.
- **Dataset Insights**: Feature correlation heatmap, Random Forest feature importance chart, and 4-panel population distribution comparisons.
- **Download Reports**: Export PDF diagnostic reports, plain text evaluation summaries (`.txt`), or tabular prediction summaries (`.csv`).

---

### Mode B: Standalone Interactive Terminal Console

The standalone script provides a self-contained command-line interface:

```bash
# Launch interactive terminal console
Rscript standalone.R
```

#### Terminal Menu:
```text
================================================================================
                    CALORIE BURN PREDICTOR — MAIN MENU                         
================================================================================
1. Predict Calorie Burn (Interactive Parameter Entry)
2. View Model Performance (5-Fold CV & Test Evaluation)
3. View Dataset Summary & Statistics
4. Generate & Save Visualizations (to plots/)
5. Export Comprehensive Evaluation Report (to reports/)
6. Exit
================================================================================
```

- **Robust Input Normalization**: Automatically standardizes mixed-case gender entries (`male`, `Male`, `MALE`, `female`, `Female`, `f`, `m`).
- **Defensive Error Traps**: Validates physical ranges (e.g. age: 10–100, height: 80–250 cm, weight: 25–250 kg) with clear error messages and automatic reprompts without crashing the program.

---

## 🧪 Automated Quality Assurance & Test Suites

The project features a **53-point automated test suite** covering all tiers of the application:

```bash
# 1. Structural & Legacy Integrity Tests
Rscript tests/test_structure.R

# 2. Machine Learning Pipeline & CV Tests (14 Tests)
Rscript tests/test_pipeline.R

# 3. Standalone Console Application Tests (19 Tests)
Rscript tests/test_standalone.R

# 4. Shiny Web Application Tests (20 Tests)
Rscript tests/test_shiny.R
```

### Overall Test Summary

| Test Suite | Total Assertions | Passed | Success Rate |
| :--- | :---: | :---: | :---: |
| `tests/test_structure.R` | Module & File Sourcing | 14 / 14 | **100.0%** |
| `tests/test_pipeline.R` | ML, Features, Cross-Validation | 14 / 14 | **100.0%** |
| `tests/test_standalone.R` | Console Menu, Inputs, Diagnostics | 19 / 19 | **100.0%** |
| `tests/test_shiny.R` | UI, Server, Downloads, Cache | 20 / 20 | **100.0%** |
| **Combined Total** | **Entire Application Stack** | **67 / 67** | **100.0%** |

---

## 📄 Report Generation & Export Artifacts

The system automatically generates and exports three types of persistent diagnostic artifacts:

1. **PDF Diagnostic Report (`reports/`)**:
   - Comprehensive multi-page document rendered using R's native `cairo_pdf` / `grid` engine.
   - Contains participant workout profile, BMI analysis, predictions across all 3 models, and validation diagnostics.
2. **Text Machine Learning Report (`reports/evaluation_report.txt`)**:
   - 7-section structured summary detailing dataset specifications, cross-validation tables, held-out test evaluations, residual skewness, and high $R^2$ physical justifications.
3. **High-Resolution Figures (`plots/`)**:
   - `corr_heatmap.png`: Correlation matrix across numerical variables.
   - `feature_importance.png`: Relative feature contribution ranking.
   - `model_comparison.png`: Model RMSE comparison bar chart.
   - `prediction_vs_actual.png`: Scatter plot with 45° ideal regression line.
   - `residual_distribution.png`: Histogram with Kernel Density Estimate and normal reference.

---

## ⚠️ Ethical & Non-Medical Disclaimer

> **IMPORTANT NOTICE:**  
> Caloric burn forecasts and Body Mass Index (BMI) computations generated by this application are **statistical machine-learning estimates** derived from observational workout data.  
> 
> - They are **not** clinical measurements, metabolic chamber observations, or medical diagnoses.  
> - BMI classifications reflect general epidemiological categories and do not account for individual muscle mass, bone density, hydration, or medical history.  
> - Do not use these outputs as a substitute for professional medical, nutritional, or cardiovascular health advice. Always consult a qualified healthcare professional before starting an intensive exercise regimen.

---

## 🚀 Limitations & Future Work

### Limitations
1. **Observational Bounds**: The dataset captures structured indoor aerobic workouts between 1 and 30 minutes with heart rates between 67 and 128 bpm. Extrapolating beyond 30 minutes (e.g. marathon running) may reduce predictive reliability.
2. **Unmodeled Confounders**: Unobserved physiological covariates such as individual VO₂ max, ambient temperature, humidity, baseline fitness level, and metabolic rate are absent from the dataset.
3. **Parametric Interval Restriction**: OLS prediction intervals strictly apply to the linear regression model under homoscedasticity assumptions and do not directly translate to Random Forest ensembles.

### Future Work
- Integration of continuous streaming wearable sensor data via telemetry.
- Quantile regression forests or conformal prediction for non-parametric prediction intervals.
- Personalized longitudinal baseline calibration using repeated subject trials.

---

## 🎓 Academic Review Information

- **Project**: Calorie Burn Predictor Revamp
- **Course**: R Programming Laboratory / Semester V Project Review
- **Primary Deliverables**: Interactive Shiny App (`app.R`) & Standalone Console Script (`standalone.R`)
- **Core Technology**: 100% Native R (No Python / JS / Node.js)

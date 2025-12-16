# Trend Analysis of Study Permits Over Time (2015-2024)

## 📌 Project Overview
This project is an **R Shiny Dashboard** designed to analyze and visualize detailed trends in study permits issued between **2015 and 2024**. It provides an interactive interface for policymakers, researchers, and stakeholders to explore the data through dynamic visualizations, statistical summaries, and advanced predictive modeling.

## 🚀 Key Features

### 1. 📊 Statistics Dashboard
*   View key statistical metrics (**Mean**, **Median**, **Max**) for study permits.
*   Filter data dynamically by **Country of Citizenship** and **Year**.

### 2. 📈 Interactive Visualizations
*   **Line Charts:** Compare yearly trends between multiple countries (e.g., India vs. China).
*   **Bar Charts:** Analyze quarterly breakdowns for specific years.
*   **Pie Charts:** Visualize the proportion of permits issued per quarter.
*   **Scatter Plots:** Observe yearly distribution correlations.

### 3. 🤖 Advanced Modeling & Machine Learning
*   **DBSCAN Clustering:** Unsupervised learning algorithm to group countries based on permit volume similarities.
    *   *Includes interactive cluster plots and data tables.*
*   **SARIMA Forecasting:** Time-series forecasting model to predict future study permit trends.
    *   *Provides 80% and 95% confidence intervals for predictions.*

## 🛠️ Technology Stack
*   **Language:** R
*   **Framework:** [R Shiny](https://shiny.rstudio.com/)
*   **Libraries Used:**
    *   `ggplot2`, `plotly` (Visualization)
    *   `forecast` (Time-series modeling)
    *   `dbscan` (Clustering)
    *   `dplyr`, `tidyr`, `readxl` (Data Manipulation)
    *   `DT` (Interactive Tables)

## ⚙️ Installation & Setup

### Prerequisites
Ensure you have **R** and **RStudio** installed on your machine.

### 1. Clone the Repository
```bash
git clone https://github.com/Tarun-22/Trend-Analysis-of-Study-Permits-Over-Time-2015-2024-.git
cd Trend-Analysis-of-Study-Permits-Over-Time-2015-2024-
```

### 2. Install Required Packages
Open R or RStudio and run the following command to install dependencies:

```r
install.packages(c("shiny", "readxl", "forecast", "ggplot2", "dplyr", "tidyr", "plotly", "dbscan", "DT"))
```

### 3. Run the App
You can run the application directly from your terminal:

```bash
Rscript DAgroup05.R
```
*Or, open `DAgroup05.R` in RStudio and click the **"Run App"** button.*

## 📂 Project Structure
```
├── DAgroup05.R          # Main R Shiny application code
├── OriginalFile.xlsx    # Dataset (Source of study permit data)
└── README.md            # Project documentation
```

## 👥 Contributors
*   **Group 05** - Data Analytics Project

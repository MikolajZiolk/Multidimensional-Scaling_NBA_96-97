This project, developed as part of the "Statistical Data Analysis" course, focuses on analyzing NBA team statistics from the 1996-97 season. The analysis includes Exploratory Data Analysis (EDA) and two dimensionality reduction techniques: Principal Component Analysis (PCA) and Multidimensional Scaling (MDS). 
Results are compared, visualized, and thoroughly discussed.

## Exploratory Data Analysis (EDA):
1. Descriptive statistics.
2. Variable correlations.
3. Data visualization (boxplots, scatter plots).

## PCA Analysis (Principal Component Analysis):
1. Bartlett's test and KMO measure to validate PCA applicability.
2. Dimensionality reduction and component selection (explaining >80% of variance).
3. Factor loadings and result interpretation.

## MDS Analysis (Multidimensional Scaling):
1. Classical MDS (STRESS = 0.13 – poor fit).
2. Sammon's Mapping (STRESS = 0.02 – very good fit).
3. Visualization and grouping of teams based on performance.

## Summary:
Strengths and weaknesses of PCA and MDS. Comparison of results and interpretation of team relationships. PCA effectively reduces data dimensions while retaining 85.4% of total variance in 3 components. Sammon's Mapping MDS provides better local structure preservation compared to classical MDS. The analysis reveals clusters of teams with similar statistics, as well as standout teams based on shooting efficiency and performance metrics.

### Technologies
- R/RStudio 
- Quarto (.qmd)

Libraries: ggplot2, plotly, psych, MASS, rgl, dplyr, tidyr

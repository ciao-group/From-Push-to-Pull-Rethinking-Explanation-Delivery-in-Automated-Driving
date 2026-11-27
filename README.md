# From Push to Pull: Rethinking Explanation Delivery in Automated Driving

This repository contains scripts of our publication "From Push to Pull: Rethinking Explanation Delivery in Automated Driving". The code is used to preprocess, analyze, and visualize data collected during the study.

## Requirements

The language of the scripts is Python / Jupyter Notebooks and R Scripts.

Install the required packages with:

```bash
pip install -r requirements.txt
```

## Files

```text
preprocess_score_data.ipynb     # preprocess raw questionnaires into preprocessed dataframes
functions.py                    # Helper functions
rq1_rq2_conditions.ipynb        # Analyses experimental condition effects for RQ1 and RQ2.
rq3_correlations.ipynb          # Correlation analyses for RQ3 using Spearman correlation matrices
exploratory_barplot.ipynb       # Exploratory Barplot of pull explanations

rq2_lmms.R                      # Linear mixed models for RQ2 to investigate push vs. pull contrasts
rq3_lmms.R                      # Linear mixed models for RQ3 to investigate individual differences
rq4_lmms.R                      # Linear mixed models for RQ4 to examine potential order effects
exploratory_pull_analysis.R     # Logistic mixed models for exploratory pull analysis

requirements.txt
README.md
```

## Data

The experimental data used in this project can be found in the corresponding [OSF Repository](https://doi.org/10.17605/OSF.IO/S52MU).


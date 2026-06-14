# NKEFZpaper-Replication
Replication materials for Aging &amp; Mental Health
This repository contains code and supporting files for reproducing the analyses reported in:

Zhang, J. (2026). Do ecosystem improvements enhance the cognitive function of older adults: Quasi-experimental evidence from china. Aging and Mental Health. https://doi.org/10.1080/13607863.2026.2689586

## Files

### do/01_create_dataset.do

Creates the analysis dataset.

### do/02_analysis.do

Runs the statistical analyses reported in the paper.

### data/policy.xls

Input file required for dataset construction.

## Replication instructions

1. Place all required data files in the working directory.
2. Run `01_create_dataset.do`.
3. Run `02_analysis.do`.

## Data availability

City-level covariates are publicly available from their original source but are not redistributed through this repository because permission to share the data has not been obtained.

Users should obtain these data directly from the original provider and place them in the appropriate directory before running the code.

# Multi-Ancestry Hypertrophic Cardiomyopathy (HCM) Polygenic Risk Score (PRS) Analysis Pipeline

This repository contains the complete analytical pipeline, computational scripts, and visualization code used to evaluate a multi-ancestry polygenic risk score (PRS) for hypertrophic cardiomyopathy (HCM).

## Overview
* **Objective:** To construct and evaluate a multi-ancestry PRS for HCM using PRS-CSx and clumping-and-thresholding (C+T) frameworks, assessing disease penetrance, prevalence, lifetime incidence, ancestry-specific performance, and longitudinal cardiovascular outcomes.
* **Testing Dataset:** All of Us Research Program (AoURP) (~258,361 participants across European, African, Admixed, and Asian ancestries).
* **Validation Cohort:** UK Biobank.
* **Base Data Sources:** Summary statistics from Biobank Japan, the Million Veteran Program (MVP), and European case-control meta-analyses.

## Repository Structure & Master Execution
* `run`: Master shell script that orchestrates the execution sequence.
* `code/Codes_to_run/Code_for_PRScsx_C_T_Generation_07242026.sh`: Primary shell script executing PRScsx and C+T generation pipelines.
* `code/Codes_to_run/Code_for_Association_Downstream_Manuscript_07242026.R`: Comprehensive R script containing data wrangling, logistic regression, interval-censored survival analysis (`icenReg`), ancestry stratification, and figure generation routines.
* `data/Summary_Statistics/`: Directory containing input summary statistics and harmonized datasets.
    * `GCST90435254_07242026.txt`
    * `META_BBJ_07242026.txt`
    * `meta_mvp_meta_bbj_v1_07242026.txt`
    * `MVP_HOCM_07242026.txt`
    * `MVP_NHCM_07242026.txt`
* `data/Genotypes/`: Directory containing genotype data files (~136.5 MB).
* `data/LD_reference/`: Directory containing linkage disequilibrium (LD) reference panels (~204.45 MB).
* `environment/`: Environment configuration containing Dockerfile and `environment.yml`.
* `metadata/`: Metadata configurations including `metadata.yml`.
* `results/output/`: Directory designated for generated outputs and pipeline results.

## System & Software Requirements
* **Languages:** Bash, R (v4.0+)
* **External Tools:** PLINK v1.9 / v2.0, Python / PRScsx framework
* **R Packages:** `parallel`, `fmsb`, `pROC`, `dplyr`, `stats`, `icenReg`, `purrr`, `doParallel`, `tidyr`, `knitr`, `kableExtra`, `ggplot2`, `cowplot`, `ggpubr`, `gridExtra`, `plotrix`

## Execution Instructions
To run the full reproducible pipeline from scratch via the master script, execute:
```bash
bash run
```

## Citation
If you use this pipeline or data in your research, please cite our corresponding publication: 

Bal et al, 2026: Polygenic Risk Modifies Penetrance and Outcomes in Hypertrophic Cardiomyopathy: Insights from a US-Based Multi-Ancestry Cohort


## Contact
For questions, feedback, or issues regarding this repository, please contact Akhil Pampana at the University of Alabama at Birmingham.

## License
This project is licensed under the terms of the MIT License. See the LICENSE file for details.

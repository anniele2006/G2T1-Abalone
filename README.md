# G2T1-Abalone

This project analyses the Abalone dataset using multiple linear regression and random forest models, with a focus on non-destructive age prediction.

## Repository contents

- `abalone_report.Rmd` — source file for the final report
- `abalone_report.pdf` — final rendered report
- `references.bib` — bibliography for the report
- `LinearModelAlive.R` — non-destructive linear regression analysis
- `LinearModel.R` — linear regression analysis
- `LinearModelDead.R` — destructive-model analysis
- `randomForestModel.R` — random forest analysis
- `abalone/` — dataset files

## Report summary

The final selected non-destructive linear regression model used infant status, diameter, height, and whole weight as predictors of log-transformed ring count. Its in-sample \(R^2\) was 0.5097 and its cross-validated R^2 was 0.5096. A non-destructive random forest benchmark achieved a lower cross-validated R^2 of 0.402.

## Notes

This repository was created for coursework submission and includes both the report source and supporting analysis scripts.

## GitHub repository

[https://github.com/anniele2006/G2T1-Abalone](https://github.com/anniele2006/G2T1-Abalone)

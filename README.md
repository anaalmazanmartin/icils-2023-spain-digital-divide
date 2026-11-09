# Socioeconomic inequality in digital competence in Spain (ICILS 2023)

R code that reproduces the tables and figures in my writing sample *[PAPER TITLE]*,
an analysis of the socioeconomic gap in computer and information literacy (CIL) and
ICT self-efficacy among Spanish eighth-grade students, using the IEA's International
Computer and Information Literacy Study (ICILS) 2023.

## Research questions

1. How large is the socioeconomic gap in CIL (mean score and percentage below Proficiency Level 2)?
2. Are the gaps in access, experience and self-efficacy as large as the gap in measured CIL?
3. How closely does self-efficacy correspond to measured CIL?

## Method

Design-based analysis using the IEA's recommended procedure for ICILS: the final
student weight and 75 jackknife replicate weights for standard errors, and the five
plausible values of CIL combined under Rubin's rules.

## Data

The analysis uses the Spanish student file from the public ICILS 2023 international
database (`BSGESPI3.sav`), available from the IEA at
https://www.iea.nl/data-tools/repository. The data file is **not** included in this
repository and must be downloaded from the IEA.

## Files

- `proyecto_ana_script.R` — full analysis: data preparation, survey design, and the
  code that produces every table and figure.
- `figure1.png`–`figure3.png` — output figures.
- `table1.csv`–`table3.csv`, `tableA1.csv` — output tables.

## Requirements

R (≥ 4.0) with the packages `haven`, `survey`, `mitools`, `dplyr` and `ggplot2`:

```r
install.packages(c("haven", "survey", "mitools", "dplyr", "ggplot2"))
```

## How to run

1. Download `BSGESPI3.sav` from the IEA and place it in the working directory.
2. Run `proyecto_ana_script.R`. It writes the tables (`.csv`) and figures (`.png`)
   to the same folder.

## Author

Ana Almazán Martín

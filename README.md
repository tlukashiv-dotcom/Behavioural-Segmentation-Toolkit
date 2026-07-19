# Behavioural Segmentation Toolkit

![R](https://img.shields.io/badge/R-4.5%2B-blue.svg)
![License](https://img.shields.io/badge/License-MIT-green.svg)
![Status](https://img.shields.io/badge/status-stable-brightgreen.svg)

Behavioural Segmentation Toolkit is an open-source toolkit for developing statistically validated behavioural segmentation models from survey data. It integrates data preprocessing, feature engineering, clustering, model validation, automated reporting, and presentation generation into a fully reproducible workflow.

The toolkit is designed for applications in **public health**, **market research**, **social science**, and other domains requiring robust behavioural segmentation and interpretable client-ready deliverables.

---

## Highlights

- Fully reproducible end-to-end analytical pipeline
- Automated model selection and validation
- Client-ready reports and presentations
- Modular architecture for easy customization
- Designed for research and applied behavioural analytics

## Key Features

### Data Preparation

- Data quality assessment
- Automated preprocessing
- Missing value handling
- Variable type detection
- Data dictionary generation

### Segmentation Modelling

- Feature quality assessment
- Feature selection
- Distance metric comparison
- Multiple clustering algorithms
- Model comparison
- Bootstrap stability validation
- Final model selection

### Interpretation

- Automatic segment profiling
- Behavioural segment naming
- Persona generation
- Intervention recommendations
- KPI framework generation

### Reporting

- HTML report generation
- Microsoft Word report generation
- PowerPoint presentation generation
- Execution logging
- Fully reproducible analytical pipeline

---

## Workflow

The toolkit implements a fully automated workflow consisting of eighteen sequential stages.

```text
                           Raw Survey Data
                                  │
                                  ▼
                     01 Load & Audit
                                  │
                                  ▼
                  02 Data Preprocessing
                                  │
                                  ▼
                  03 Data Dictionary
                                  │
                                  ▼
         04 Exploratory Segmentation Check
                                  │
                                  ▼
          05 Feature Quality Assessment
                                  │
                                  ▼
              06 Feature Selection
                                  │
                                  ▼
            07 Distance Comparison
                                  │
                                  ▼
             08 Model Comparison
                                  │
                                  ▼
          09 Bootstrap Validation
                                  │
                                  ▼
           10 Final Model Selection
                                  │
                                  ▼
           11 Final Segment Profiles
                                  │
                                  ▼
      12 Segment Naming & Narratives
                                  │
                                  ▼
          13 Final Segment Tables
                                  │
                                  ▼
            14 Persona Generation
                                  │
                                  ▼
     15 Intervention Recommendations
                                  │
                                  ▼
            16 Report Generation
                                  │
                                  ▼
        17 Presentation Generation
                                  │
                                  ▼
         18 Complete Pipeline Runner
```

---

## Repository Structure

```text
Behavioural-Segmentation-Toolkit/

├── data/
├── outputs/
├── *.R
├── README.md
├── LICENSE
└── CITATION.cff
```

---

## Requirements

The toolkit has been tested with:

- R 4.5.1 or later
- Windows
- macOS
- Linux

### Main Dependencies

- dplyr
- tidyr
- readr
- readxl
- ggplot2
- cluster
- janitor
- purrr
- stringr
- openxlsx
- officer
- flextable
- rmarkdown
- knitr

---

## Installation

Clone the repository:

```bash
git clone https://github.com/YOUR_USERNAME/Behavioural-Segmentation-Toolkit.git

cd Behavioural-Segmentation-Toolkit
```

Install the required R packages:

```r
install.packages(c(
  "dplyr",
  "tidyr",
  "readr",
  "readxl",
  "ggplot2",
  "cluster",
  "janitor",
  "purrr",
  "stringr",
  "openxlsx",
  "officer",
  "flextable",
  "rmarkdown",
  "knitr"
))
```

---

## Quick Start

Open the project in **RStudio** (recommended) or set the working directory to the repository root.

Run the complete analytical workflow with a single command:

```r
source("18_run_complete_pipeline.R")
```

The pipeline automatically executes all analytical stages, validates intermediate outputs, records execution logs, and generates all final deliverables.

> **Note:** The first execution may take several minutes depending on the size of the input dataset and the selected validation settings.

---

## Outputs

Upon successful execution, the toolkit generates the following directory structure:

```text
outputs/

├── figures/
├── final/
├── logs/
├── presentation/
├── report/
└── tables/
```

Generated outputs include:

- HTML report
- Microsoft Word report
- PowerPoint presentation
- Segment profiles
- Personas
- Intervention recommendations
- KPI framework
- Summary tables
- Pipeline execution logs

---

## Pipeline Stages

| Stage | Description |
|------:|-------------|
|01|Load and audit|
|02|Data preprocessing|
|03|Data dictionary|
|04|Exploratory segmentation check|
|05|Feature quality assessment|
|06|Feature selection|
|07|Distance comparison|
|08|Model comparison|
|09|Bootstrap stability validation|
|10|Final model selection|
|11|Final segment profiles|
|12|Segment naming and narratives|
|13|Final segment tables|
|14|Persona generation|
|15|Intervention recommendations|
|16|Report generation|
|17|Presentation generation|
|18|Complete pipeline runner|

---

## Reproducibility

The toolkit has been validated using:

- R 4.5.1
- macOS Sequoia
- Apple Silicon

The toolkit is designed to ensure transparent and reproducible analyses. All results can be reproduced directly from the repository using the provided scripts and the corresponding input datasets.

---

## Citation

If you use this toolkit in academic research or applied projects, please cite the software using the metadata provided in **CITATION.cff**.

---

## License

This project is licensed under the **MIT License**.

See the **LICENSE** file for details.

---

## Author

Taras Lukashiv

---

## Contributing

Contributions are welcome.

Bug reports, feature requests, documentation improvements, and pull requests are encouraged.

Please open an issue before submitting substantial changes.
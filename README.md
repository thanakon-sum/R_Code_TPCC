# TPCC Visualization Scripts

This repository contains R scripts for generating comprehensive visualizations of climate mitigation scenarios, analyzing carbon management strategies, energy system transitions, and feasibility assessments across multiple dimensions.

## Overview

These scripts produce publication-quality figures analyzing climate mitigation pathways through integrated assessment modeling. The analysis covers:

- **Carbon Management**: CCS and CDR deployment patterns compared to IPCC AR6 scenarios
- **Energy Systems**: Energy flow diagrams and sectoral energy consumption patterns
- **Feasibility Assessment**: Multi-dimensional feasibility analysis across geological, economic, technological, and socio-cultural dimensions
- **Sensitivity Analysis**: Comparative assessment of different technological pathway variants

## Repository Structure

```
.
├── README.md
├── Figure_2_CCS_CDR_comparison.R
├── Figure_3_Energy_flows.R
├── Figure_4_Feasibility_assessment.R
├── Figure_S_Feasibility_sensitivity.R
└── data/
    ├── IAMCTemplate_Iteon_global.gdx
    ├── AR6_Scenarios_Database_metadata_indicators_v1.1.xlsx
    ├── AR6_Scenarios_Database_World_v1.1.csv
    ├── winsolpotential2.gdx
    ├── biopotential.gdx
    └── gidden_et_al_geologic_carbon_storage_2.csv
```

## Scripts Description

### 1. Figure 2: CCS and CDR Comparison (`Figure_2_CCS_CDR_comparison.R`)

**Purpose**: Compares study scenarios against the IPCC AR6 scenarios database for CCS and CDR deployment.

**Outputs**: 
- 6-panel figure (300x360 mm, 300 dpi)
- Panels analyze: cumulative deployment, peak annual rates, and 2100 values

**Required Data**:
- `IAMCTemplate_Iteon_global.gdx`
- `AR6_Scenarios_Database_metadata_indicators_v1.1.xlsx`
- `AR6_Scenarios_Database_World_v1.1.csv`

**Scenarios Analyzed**: HCC, LCC, TPCC

---

### 2. Figure 3: Energy Flows and Sector Analysis (`Figure_3_Energy_flows.R`)

**Purpose**: Visualizes energy transformation pathways and sectoral consumption patterns.

**Outputs**: 
- 6-panel figure (350x280 mm, 300 dpi)
- Panels a-d: Sankey diagrams showing energy flows
- Panels e-f: Sectoral energy consumption (absolute and percentage)

**Required Data**:
- `IAMCTemplate_Iteon_global.gdx`

**Scenarios Analyzed**: HCC, LCC, TPCC

---

### 3. Figure 4: Feasibility Assessment (`Figure_4_Feasibility_assessment.R`)

**Purpose**: Comprehensive multi-dimensional feasibility assessment of mitigation pathways.

**Outputs**: 
- 23-panel figure per region (350x450 mm, 500 dpi)
- Organized in 6 rows covering:
  - Row 1: Geological feasibility (biomass, wind, solar, CCS potential)
  - Row 2: Economic feasibility (GDP loss, carbon price, investments)
  - Rows 3-4: Technological feasibility (renewable scale-up, carbon management)
  - Rows 5-6: Socio-cultural feasibility (energy demand, land use)

**Required Data**:
- `IAMCTemplate_Iteon_global.gdx`
- `winsolpotential2.gdx`
- `biopotential.gdx`
- `gidden_et_al_geologic_carbon_storage_2.csv`

**Scenarios Analyzed**: HCC, LCC, TPCC

**Regions**: World (default), can be expanded to R5 regions

---

### 4. Figure S: Feasibility Sensitivity Analysis (`Figure_S_Feasibility_sensitivity.R`)

**Purpose**: Sensitivity analysis comparing multiple TPCC scenario variants.

**Outputs**: 
- Same 23-panel structure as Figure 4
- One figure per region analyzing 6 scenario variants

**Required Data**: Same as Figure 4

**Scenarios Analyzed**:
- TPCC (base scenario)
- TPCC_a (alternative parameterization)
- TPCC_DAChigh (high direct air capture deployment)
- TPCC_hydhigh (high hydrogen deployment)
- TPCC_noBECCS (no bioenergy with CCS)
- TPCC_noEW (no enhanced weathering)

**Regions**: World, R5OECD90+EU, R5ASIA, R5LAM, R5MAF, R5REF

---

## Data Download

### Primary Data Repository

All required data files are available on Zenodo:

**DOI**: [10.5281/zenodo.17667313](https://doi.org/10.5281/zenodo.17667313)

### Data Files

Download and place the following files in the `data/` directory:

1. **IAMCTemplate_Iteon_global.gdx** (Required for all scripts)
   - Primary scenario results from integrated assessment model
   - Contains all variables for emissions, energy, land use, and feasibility indicators

2. **AR6_Scenarios_Database_metadata_indicators_v1.1.xlsx** (Figure 2 only)
   - IPCC AR6 metadata and climate indicators
   - Source: [IPCC AR6 Scenario Database](https://data.ece.iiasa.ac.at/ar6/)

3. **AR6_Scenarios_Database_World_v1.1.csv** (Figure 2 only)
   - IPCC AR6 world-level scenario data
   - Source: [IPCC AR6 Scenario Database](https://data.ece.iiasa.ac.at/ar6/)

4. **winsolpotential2.gdx** (Figures 4 & S only)
   - Wind and solar renewable energy potential by region
   - Includes capacity factors and geographic resolution

5. **biopotential.gdx** (Figures 4 & S only)
   - Bioenergy potential data by region and grade

6. **gidden_et_al_geologic_carbon_storage_2.csv** (Figures 4 & S only)
   - Geological carbon storage potential by country
   - Source: Gidden et al. geologic carbon storage dataset

---

## Installation

### System Requirements

- **R version**: 4.0.0 or higher
- **GAMS**: Required for reading GDX files
- **Operating System**: Windows, macOS, or Linux

### R Package Dependencies

Install all required packages:

```r
# Core packages
install.packages(c(
  "tidyverse",      # Data manipulation and visualization
  "gdxrrw",         # GAMS GDX file interface
  "cowplot",        # Plot composition
  "RColorBrewer",   # Color palettes
  "readxl",         # Excel file reading
  "readr",          # CSV file reading
  "zoo"             # Time series manipulation
))

# Additional packages for specific scripts
install.packages(c(
  "ggalluvial",     # For Figure 3 Sankey diagrams
  "ggpubr"          # For Figure 3 legend extraction
))
```

### GAMS Setup

1. Install GAMS from [https://www.gams.com/](https://www.gams.com/)
2. Locate your GAMS installation directory
3. Update the `igdx()` path in each script

**Common GAMS paths**:
- **macOS**: `/Library/Frameworks/GAMS.framework/Versions/XX/Resources`
- **Windows**: `C:/GAMS/XX`
- **Linux**: `/opt/gams/gamsXX_linux_x64_64_sfx`

---

## Usage

### Quick Start

1. **Download data files** from [Zenodo](https://doi.org/10.5281/zenodo.17667313)

2. **Create directory structure**:
```bash
mkdir data
# Place all downloaded files in data/
```

3. **Update file paths** in each script:
```r
# Change this line in each script:
data_dir <- "userdirectory/data"
# To your actual path, e.g.:
data_dir <- "~/Documents/climate_analysis/data"
```

4. **Update GAMS path** in each script:
```r
# Change this line:
igdx("/Library/Frameworks/GAMS.framework/Versions/44/Resources")
# To your GAMS installation path
```

5. **Run scripts**:
```r
source("Figure_2_CCS_CDR_comparison.R")
source("Figure_3_Energy_flows.R")
source("Figure_4_Feasibility_assessment.R")
source("Figure_S_Feasibility_sensitivity.R")
```

### Customization Options

#### Modifying Scenarios

To analyze different scenarios, update the scenario list in each script:

```r
# In Figure_2_CCS_CDR_comparison.R:
study_scenarios <- c("TPCC", "LCC", "HCC")  # Modify as needed

# In Figure_4_Feasibility_assessment.R:
cmain_plot <- c("HCC", "LCC", "TPCC")  # Modify as needed
```

#### Changing Regions

To generate figures for multiple regions (Figures 4 & S):

```r
# In Figure_4_Feasibility_assessment.R:
regions_to_plot <- c("World")  # Default

# Uncomment to add R5 regions:
# regions_to_plot <- c("World", "R5OECD90+EU", "R5ASIA", "R5LAM", "R5MAF", "R5REF")
```

#### Adjusting Output Settings

Modify figure dimensions and resolution:

```r
ggsave(
  filename = "output/figure_name.png",
  plot = final_plot,
  width = 300,    # Width in mm
  height = 360,   # Height in mm
  units = "mm",
  dpi = 300       # Resolution
)
```

---

## Output Files

All scripts create an `output/` directory automatically and save figures in both PNG and PDF formats:

### Figure 2 Outputs
```
output/
└── combined_6panel_ccs_cdr.png
└── combined_6panel_ccs_cdr.pdf
```

### Figure 3 Outputs
```
output/
└── combined_energy_figure.png
└── combined_energy_figure.pdf
```

### Figure 4 Outputs
```
output/
└── g_feasibility_combined_World.png
└── g_feasibility_combined_R5OECD90_EU.png  (if enabled)
└── g_feasibility_combined_R5ASIA.png       (if enabled)
└── ...
```

### Figure S Outputs
```
output/
└── g_feasibility_combined_World.png
└── g_feasibility_combined_R5OECD90_EU.png
└── g_feasibility_combined_R5ASIA.png
└── g_feasibility_combined_R5LAM.png
└── g_feasibility_combined_R5MAF.png
└── g_feasibility_combined_R5REF.png
```

---

## Troubleshooting

### Common Issues

**1. "GDX file not found" error**
- **Solution**: Verify the `data_dir` path is correct and all files are in the `data/` folder
- Check file names match exactly (case-sensitive on Linux/macOS)

**2. "GAMS not found" or gdxrrw errors**
- **Solution**: Update the `igdx()` path to match your GAMS installation
- Ensure GAMS is properly installed and licensed

**3. Missing package errors**
- **Solution**: Install all required packages listed in Installation section
- Use `install.packages("package_name")` for any missing packages

**4. Memory errors with large datasets**
- **Solution**: Increase R memory limit:
```r
memory.limit(size = 16000)  # Windows only
```
- Consider processing regions one at a time for Figure 4/S

**5. Plot rendering issues**
- **Solution**: Update graphics device settings:
```r
options(bitmapType = 'cairo')  # For Linux systems
```

### Getting Help

If you encounter issues:

1. Check that all data files are correctly downloaded and placed
2. Verify GAMS installation and R package versions
3. Review error messages for specific file or variable names
4. Ensure R version is 4.0.0 or higher

---

## Technical Details

### Regional Classifications

The scripts use multiple regional aggregation schemes:

- **AIM17**: 17 regions used in the AIM integrated assessment model
- **R5**: 5 regions (OECD90+EU, ASIA, LAM, MAF, REF)
- **World**: Global aggregation

### Threshold Calculations

Feasibility scripts (Figures 4 & S) use region-specific thresholds calculated as:

```
Regional Threshold = Global IPCC Threshold × (Regional Potential / Global Potential)
```

This approach ensures thresholds are proportional to each region's renewable energy and carbon storage capacity.

### Data Processing

- **Unit conversions**: 
  - Energy: GWh → EJ (multiply by 0.00000036)
  - Carbon: MtCO2 → GtCO2 (divide by 1000)
- **Temporal aggregation**: Trapezoidal rule for cumulative calculations
- **Interpolation**: Linear interpolation for missing years using `zoo::na.approx()`

---

## Citation

If you use these scripts or data in your research, please cite:

```bibtex
@dataset{TPCC_2025,
  author    = {[Fujimori et al., 2025]},
  title     = {Climate Mitigation Scenario Analysis Data and Visualization Scripts},
  year      = {2025},
  publisher = {Zenodo},
  doi       = {10.5281/zenodo.17667313},
  url       = {https://doi.org/10.5281/zenodo.17667313}
}
```

---

## License



---


## Contact

For questions or issues:
- Open an issue in the repository
- Contact: https://sites.google.com/athehost.env.kyoto-u.ac.jp/eng

---

## Acknowledgments

- IPCC AR6 Scenario Database for reference data
- GAMS Development Corporation for GAMS software
- R Core Team and package developers

---

**Last Updated**: Fri 21 November 2025

**Version**: 1.0.0

# README

This repository contains the analysis code for the meta-analysis on diversity and team performance. It can be used to reproduce the findings in the paper, though some runtimes are very long (and therefore, cached intermediate results provided here). This is complemented by a [separate repository](https://github.com/LukasWallrich/diversity_meta_analysis) that describes the search and screening process, and includes reusable code.

The files here are divided into the following steps:

- `1_prep_and_describe_data.Rmd` downloads the data, prepares it (including the adjustment for attenuation) and creates sample descriptives. The full results can be seen in `1_prep_and_describe_data.html`
- `1a_prisma_flowchart_data.Rmd` pull together the data for the PRISMA flowchart documenting search and screening, with results in `1a_prisma_flowchart_data.html`
- `2_main_meta.Rmd` estimates the main effect meta-analyses,  with results in `2_main_meta.html`
- `4_publication-bias.Rmd` runs publication bias tests (Eggers and selection models), with results in `4_publication-bias.html`
- `5_exploration.Rmd` runs exploratory analyses, with results in `5_exploration.html`
- `6_robustness.Rmd` runs robustness checks, with results in `6_robustness.Rmd`


## Acknowledgements

- This project benefited substantially from the [RMarkdown template for Correlational studies meta-analysis in Psychology](https://osf.io/f85uy/), developed by Adrien Fillon and Gilad Feldman.
- Some of the code (particularly for plotting) is also derived from Nils Reimer's code for the [Ironic effects of intergroup contact meta-analysis](https://github.com/nilsreimer/ironic-effects-meta-analysis/tree/4c056714b4b8fd291892ca416fb77333d05b2a5e)
- We are also grateful to the ATTENTUATION and METACART authors who provided us with helpful guidance when technical issues arose.


# Overview of SM

# A - Search, Screening and Coding

## Data:
- Deduplicated results
- Post screening
- Post coding

# B - Analysis

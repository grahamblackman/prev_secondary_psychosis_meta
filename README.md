Analysis code and results for the study

# Secondary causes of psychosis: A systematic review and meta-analysis 

<p align="center">
	<a href="https://en.wikipedia.org/wiki/R_(programming_language)"><img
		alt="R Programming Language"
		src="https://img.shields.io/badge/Language-R-%232268BB.svg"></a>
	<a href="https://opensource.org/licenses/MIT"><img
		alt="MIT License"
		src="https://img.shields.io/badge/license-MIT-blue.svg"></a>
</p>

This archive contains the code and data for the analysis reported in the above study. The code is written in the [R](https://en.wikipedia.org/wiki/R_(programming_language)) programming language.

This repository contains the following files

0. [psych_data.xlsx](https://github.com/grahamblackman/prev_secondary_psychosis_meta/blob/main/psych_data.xlsx) - data extracted for meta-analysis used for this analysis
1. [meta_sec_psy_master.R](https://github.com/grahamblackman/prev_secondary_psychosis_meta/blob/main/meta_sec_psy_master.R) - the master script that calls other scripts in the following order to reproduce the results reported in the manuscript
2. [meta_sec_psy_pro.R ](https://github.com/grahamblackman/prev_secondary_psychosis_meta/blob/main/meta_sec_psy_pro.R) - pre-processing step that ensures key variables are set to the correct data types and subsets the main dataframes into dataframes that contain specific study types for later analysis
3. [meta_sec_psy_desc.R ](https://github.com/grahamblackman/prev_secondary_psychosis_meta/blob/main/meta_sec_psy_desc.R) - calculates and displays descriptive statistics
4. [meta_sec_psy_prev.R](https://github.com/grahamblackman/prev_secondary_psychosis_meta/blob/main/meta_sec_psy_prev.R) - main meta-analysis of prevalence and meta-analysis with moderators
5. [meta_sec_psy_prev_subtype.R](https://github.com/grahamblackman/prev_secondary_psychosis_meta/blob/main/meta_sec_psy_prev_subtype.R) - meta-analysis of specific secondary causes of psychosis
6. [meta_sec_psy_prev_investigation.R](https://github.com/grahamblackman/prev_secondary_psychosis_meta/blob/main/meta_sec_psy_prev_investigation.R) - meta-analysis by investigation type
7. [meta_sec_psy_vis_file.R](https://github.com/grahamblackman/prev_secondary_psychosis_meta/blob/main/meta_sec_psy_vis_file.R) - produces primary visualisations of prevalence
8. [meta_sec_psy_vis_subtypes_file.R](https://github.com/grahamblackman/prev_secondary_psychosis_meta/blob/main/meta_sec_psy_vis_subtypes_file.R) - visualisations of proportion of each subtype within a single forest plot
9. [meta_sec_psy_vis_investigations_file.R](https://github.com/grahamblackman/prev_secondary_psychosis_meta/blob/main/meta_sec_psy_vis_investigations_file.R) - visualisations of proportion of secondary psychosis by investigation type within a single forest plot

---

### Platform and package versions

R language version, and package versions used to generate the results are:

R Version 4.2.2<br>
Package version for meta is 6.5.0<br>
Package version for metafor is 4.4.0<br>
Package version for readxl is 1.4.3<br>
Package version for skimr is 2.1.5<br>
Package version for tidyverse is 2.0.0<br>
Package version for lubridate is 1.9.3<br>
Package version for MASS is 7.3.60<br>
Package version for janitor is 2.2.0<br>
Package version for devtools is 2.4.5<br>
Package version for lme4 is 1.1.34<br>
Package version for expss is 0.11.6<br>
Package version for scales is 1.2.1<br>
Package version for forester is 0.2.0<br>

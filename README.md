# Hazardous heat exposure in Native Lands in the United States

Jackson Chen, Cascade Tuholske, Robbie M. Parks

## Introduction

## Code

### Data preparation (data_prep) list:

a_01_prepare_wbgt_native_data - load WBGT prison data

a_02_prepare_wbgt_native_summary_data - prepare WBGT prison summary data

a_03_prepare_wbgt_state_data - load WBGT state data

a_04_prepare_wbgt_state_summary_data - prepare WBGT state summary data

### Data exploration (data_exploration) list:

b_00_native_explore - Basic facts about native territories

b_01_figure_1 - Plot Figure 1

b_02_figure_2 - Plot Figure 2

b_03_native_lands_stats - Calculate values for main manuscript

b_04_state_maps - Plot individual state maps


## Other stuff

Note: Please run create_folder_structure.R first to create folders that may not be there when first loaded. \
Run b_00_native_explore first, as it sets up the environment 

Note: to run an R Markdown file from the command line, run\
Rscript -e "rmarkdown::render('SCRIPT_NAME.Rmd')"

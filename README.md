# hgl_2010_replication
Materials from my replication of Hunt & Gauthier-Loiselle (2010) (hereafter HGL).

link to working paper: https://www.econstor.eu/bitstream/10419/265508/1/I4R-DP004.pdf

folder "data" contains the dataset "finaldata.dta" obtained from original authors' replication files: https://www.openicpsr.org/openicpsr/project/114172/version/V1/view

"2010_hgl_replication.Rproj" is the R project file for the analysis

folder "code" contains the code used in the replication.
- "recreate_t7iv.R" contains code to rerun the instrumental variables estimates in R from HGL's table 7, and provide estimates when recreating the instrument from scratch.
- "recreate_table8.R" contains the code used to rerun the instrumental variables estimates in R from HGL's table 8, and provide estimates when recreating the instrument from scratch.
- "bw_analysis_t8.R" contains the code to check the robustness of HGL's results against the new diagnostic tests.
- "bw.cpp" contains the C++ code used to implement methods from Goldsmith-Pinkham et al. (2020)
- "tables.do" contains the Stata code used by HGL for their estimates in a single do file

R packages used: tidyverse, haven, janitor, kableExtra, Rcpp, fixest, modelsummary

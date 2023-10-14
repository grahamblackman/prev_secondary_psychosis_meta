########################################################################################
#                                                                                      #
# meta_sec_psy_desc.R from Blackman et al                                              #
#                                                                                      #
# "How common are secondary causes of psychosis                                        #
#  A meta-analysis of prevalence"                                                      #
#                                                                                      #                                                                                      
#     meta analysis and sub group analysis                                             #
#                                                                                      #
#                                                                                      #
########################################################################################
#
# This script calculates the meta analysis of proportions...
#
# queries: 
# add in predication interval - needs to be in meta format
# restrict meta analyses to those using >2 causes of psychosis?

###################  meta analysis of proportion [using metafor]

# sensitivity analysis: investigation --------------------------------------


# Urine analysis [k=9]
pes_sec_psych_summary_by_urine_all <- meta::metaprop(tot_sec_psych, tot_sample, author_year, data = data[data$urine_all == '1',], sm = "PFT",
                                                    method.tau = "DL", method.ci = "NAsm")

sprintf("results of meta analysis after restricting to studies routinely performing urine analysis")
pes_sec_psych_summary_by_urine_all

# Bloods [k=11]
pes_sec_psych_summary_by_blood_all <- meta::metaprop(tot_sec_psych, tot_sample, author_year, data = data[data$blood_all == '1',], sm = "PFT",
                                                    method.tau = "DL", method.ci = "NAsm")

sprintf("results of meta analysis after restricting to studies routinely performing blood analysis")
pes_sec_psych_summary_by_blood_all

# MRI [k=2]
pes_sec_psych_summary_by_mri_all <- meta::metaprop(tot_sec_psych, tot_sample, author_year, data = data[data$mri_all == '1',], sm = "PFT",
                                                  method.tau = "DL", method.ci = "NAsm")

sprintf("results of meta analysis after restricting to studies routinely performing mri analysis")
pes_sec_psych_summary_by_mri_all

# CSF [k=3]
pes_sec_psych_summary_by_csf_all <- meta::metaprop(tot_sec_psych, tot_sample, author_year, data = data[data$csf_all == '1',], sm="PFT",
                                                  method.tau = "DL", method.ci = "NAsm")

sprintf("results of meta analysis after restricting to studies routinely performing csf analysis")
pes_sec_psych_summary_by_csf_all

# UDS + Bloods  [n=5]
pes_sec_psych_summary_by_urine_blood_all <- meta::metaprop(tot_sec_psych, tot_sample, author_year, data = data[data$urine_blood_all == '1',], sm = "PFT",
                                                          method.tau = "DL", method.ci = "NAsm")

sprintf("results of meta analysis after restricting to studies routinely performing urine and blood analysis")
pes_sec_psych_summary_by_urine_blood_all

# UDS + MRI [n=2]
pes_sec_psych_summary_by_urine_mri_all <- meta::metaprop(tot_sec_psych, tot_sample, author_year, data = data[data$urine_mri_all == '1',], sm = "PFT",
                                                        method.tau = "DL", method.ci = "NAsm")

sprintf("results of meta analysis after restricting to studies routinely performing urine and mri analysis")
pes_sec_psych_summary_by_urine_mri_all

# Bloods + MRI [n=2]
pes_sec_psych_summary_by_blood_mri_all <- meta::metaprop(tot_sec_psych, tot_sample, author_year, data = data[data$blood_mri_all == '1',], sm = "PFT",
                                                        method.tau = "DL", method.ci = "NAsm")

sprintf("results of meta analysis after restricting to studies routinely performing blood and mri analysis")
pes_sec_psych_summary_by_blood_mri_all

# UDS+ Bloods + MRI[n=2]
pes_sec_psych_summary_by_urine_blood_mri_all <- meta::metaprop(tot_sec_psych, tot_sample, author_year, data = data[data$urine_blood_mri_all == '1',], sm = "PFT",
                                                              method.tau = "DL", method.ci = "NAsm")

sprintf("results of meta analysis after restricting to studies routinely performing urine analysis, blood and MRI")
pes_sec_psych_summary_by_urine_blood_mri_all

# leave investiatoin out SA

# Then we defined a comprehensive investigation to comprise urine analysis, blood analysis and an MRI head in all patients.
# We perform several analyses to estimate the problems of secondary psychosis in the different conditions.

# urineanalysis [k=9]
pes_sec_psych_summary_by_urine_none <- meta::metaprop(tot_sec_psych, tot_sample, author_year, data = data[data$urine_none == '1',], sm = "PFT",
                                                     method.tau = "DL", method.ci = "NAsm")

sprintf("results of meta analysis after restricting to studies routinely performing urine analysis")
pes_sec_psych_summary_by_urine_none

# Bloods [k=11]
pes_sec_psych_summary_by_blood_none <- meta::metaprop(tot_sec_psych, tot_sample, author_year, data = data[data$blood_none == '1',], sm = "PFT",
                                                     method.tau = "DL", method.ci = "NAsm")

sprintf("results of meta analysis after restricting to studies routinely performing blood analysis")
pes_sec_psych_summary_by_blood_none

# MRI [k=2]
pes_sec_psych_summary_by_mri_none <- meta::metaprop(tot_sec_psych, tot_sample, author_year, data = data[data$mri_none == '1',], sm = "PFT",
                                                   method.tau = "DL", method.ci = "NAsm")

sprintf("results of meta analysis after restricting to studies routinely performing mri analysis")
pes_sec_psych_summary_by_mri_none

# UDS + Bloods  [n=5]
pes_sec_psych_summary_by_urine_blood_none <- meta::metaprop(tot_sec_psych, tot_sample, author_year, data = data[data$urine_blood_none == '1',], sm="PFT",
                                                           method.tau = "DL", method.ci = "NAsm")

sprintf("results of meta analysis after restricting to studies routinely performing urine and blood analysis")
pes_sec_psych_summary_by_urine_blood_none

# UDS + MRI [n=2]
pes_sec_psych_summary_by_urine_mri_none <- meta::metaprop(tot_sec_psych, tot_sample, author_year, data = data[data$urine_mri_none == '1',], sm = "PFT",
                                                         method.tau = "DL", method.ci = "NAsm")

sprintf("results of meta analysis after restricting to studies routinely performing urine and mri analysis")
pes_sec_psych_summary_by_urine_mri_none

# Bloods + MRI [n=2]
pes_sec_psych_summary_by_blood_mri_none <- meta::metaprop(tot_sec_psych, tot_sample, author_year, data = data[data$blood_mri_none == '1',], sm = "PFT",
                                                         method.tau = "DL", method.ci = "NAsm")

sprintf("results of meta analysis after restricting to studies routinely performing blood and mri analysis")
pes_sec_psych_summary_by_blood_mri_none

# UDS+ Bloods + MRI
pes_sec_psych_summary_by_urine_blood_mri_none <- meta::metaprop(tot_sec_psych, tot_sample, author_year, data = data[data$urine_blood_mri_none == '1',], sm = "PFT",
                                                               method.tau = "DL", method.ci = "NAsm")

sprintf("results of meta analysis after restricting to studies routinely performing urine analysis, blood and MRI")
pes_sec_psych_summary_by_urine_blood_mri_none

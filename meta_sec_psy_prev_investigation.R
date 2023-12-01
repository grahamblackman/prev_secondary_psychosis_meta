########################################################################################
#                                                                                      #
# meta_sec_psy_desc.R from Blackman et al                                              #
#                                                                                      #
# Secondary causes of psychosis: A systematic review and meta-analysis                 #
#                                                                                      #
#                                                                                      #                                                                                      
#     Sensitivity analyses based on investigations performed                           #
#                                                                                      #
#                                                                                      #
########################################################################################
#
# This script summarizes the estimated proportion for all causes of secondary psychosis restricted 
# to studies that performed MRI, blood, CSF, EEG and urine analysis on all patients
#

###################  meta analysis of proportion [metafor package]

# sensitivity analysis: investigation --------------------------------------

# Urine analysis 
pes_sec_psych_summary_by_urine_all <- meta::metaprop(tot_sec_psych, tot_sample, author_year, data = data[data$urine_all == '1',], sm = "PFT",
                                                    method.tau = "DL", method.ci = "NAsm")

sprintf("results of meta analysis after restricting to studies routinely performing urine analysis")
pes_sec_psych_summary_by_urine_all

# Bloods
pes_sec_psych_summary_by_blood_all <- meta::metaprop(tot_sec_psych, tot_sample, author_year, data = data[data$blood_all == '1',], sm = "PFT",
                                                    method.tau = "DL", method.ci = "NAsm")

sprintf("results of meta analysis after restricting to studies routinely performing blood analysis")
pes_sec_psych_summary_by_blood_all

# MRI 
pes_sec_psych_summary_by_mri_all <- meta::metaprop(tot_sec_psych, tot_sample, author_year, data = data[data$mri_all == '1',], sm = "PFT",
                                                  method.tau = "DL", method.ci = "NAsm")

sprintf("results of meta analysis after restricting to studies routinely performing mri analysis")
pes_sec_psych_summary_by_mri_all

# CSF 
pes_sec_psych_summary_by_csf_all <- meta::metaprop(tot_sec_psych, tot_sample, author_year, data = data[data$csf_all == '1',], sm="PFT",
                                                  method.tau = "DL", method.ci = "NAsm")

sprintf("results of meta analysis after restricting to studies routinely performing csf analysis")
pes_sec_psych_summary_by_csf_all

# EEG 
pes_sec_psych_summary_by_eeg_all <- meta::metaprop(tot_sec_psych, tot_sample, author_year, data = data[data$eeg_all == '1',], sm="PFT",
                                                   method.tau = "DL", method.ci = "NAsm")

sprintf("results of meta analysis after restricting to studies routinely performing EEG analysis")
pes_sec_psych_summary_by_eeg_all


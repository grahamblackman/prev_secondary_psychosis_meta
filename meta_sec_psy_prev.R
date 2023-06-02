########################################################################################
#                                                                                      #
# meta_sec_psy_desc.R from Blackman et al                                              #
#                                                                                      #
# "How common are secondary causes of psychosis                                        #
#  A meta-analysis of prevalence"                                                      #
#                                                                                      #                                                                                      #
#     meta analysis and sub group analysis                                             #
#                                                                                      #
#                                                                                      #
########################################################################################
#
# This script calculates the meta analysis of proportions...
#


###################  meta analysis of proportion [using metafor]

# Calculate effect sizes and sampling variance for proportion of those with any secondary cause of psychosis
# Using Freeman-Tukey double arcsine transformed proportion (measure="PFT")
ies_da_sec_psych <- escalc(xi = tot_sec_psych, ni = tot_sample, data = data, measure="PFT", add=0) 

# Calculate pooled effect size for overall proportion of those with secondary psychosis abnormality compared to total sample
# Using transformed double-arcsine transformed summary effect size and random effects model using DL estimator
pes_da_sec_psych <- rma(yi, vi, data = ies_da_sec_psych, method = "DL", level = 95) 

# Convert to non-transformed measurement scale (i.e., proportion) and yield a true summary proportion
pes_sec_psych <- stats::predict(pes_da_sec_psych, transf = transf.ipft.hm, targ=list(ni = data$tot_sample)) #class: list.rma
summary(pes_sec_psych)
print(pes_sec_psych, digits=2)
prop_sec_psych <- pes_sec_psych$pred
per_sec_psych <- label_percent()(prop_sec_psych)
sprintf("Meta analytic estimate of proportion of secondary psychosis among all cases of psychosis: %s", per_sec_psych)

prop_LCI_sec_psych <- pes_sec_psych$ci.lb
prop_UCI_sec_psych <- pes_sec_psych$ci.ub
per_LCI_sec_psych <- label_percent()(pes_sec_psych$ci.lb)
per_UCI_sec_psych <- label_percent()(pes_sec_psych$ci.ub)

# Calculate 'number needed to scan'
nns_sec_psych <- (ceiling(1/pes_sec_psych$pred)) 
# Number needed to scan upper and lower estimate
nns_LCI_sec_psych <- (ceiling(1/pes_sec_psych$ci.ub)) 
nns_UCI_sec_psych <- (ceiling(1/pes_sec_psych$ci.lb))
# NB based on transformed data
isqu_sec_psych <- pes_da_sec_psych$I2 

###################  Subgroup analysis [using metaprop]

# Arguments
# sm="PFT",  [summary measure = Freeman-Tukey Double arcsine transformation]
# method.tau="DL" [method is used to estimate the between-study variance= DerSimonian-Laird estimator  ]
# method.ci="WS" [method  used to calculate confidence = wlson score ]

# by cohort (FEP only: y/n) -------------------------------------------------------

data_FEP <- data %>% 
  filter(FEP == "yes" | FEP == "no")

pes_sec_psych_summary_by_FEP <- meta::metaprop(tot_sec_psych, tot_sample, author_year, data = data_FEP, sm = "PFT",
                                               method.tau = "DL", method.ci = "WS", subgroup = FEP) 
pes_sec_psych_summary_by_FEP

# by study design (prospective: y/n) -----------------------------------------------------

pes_sec_psych_summary_by_design <- meta::metaprop(tot_sec_psych, tot_sample, author_year, data = data, sm = "PFT",
                                                  method.tau = "DL", method.ci = "WS", subgroup = design) 
pes_sec_psych_summary_by_design

# by clinical setting (A&E, inPsych, outPsych, GP, mixed etc)  ----------------------------------------------------------------

pes_sec_psych_summary_by_setting <- meta::metaprop(tot_sec_psych, tot_sample, author_year, data = data, sm = "PFT",
                                                   method.tau = "DL", method.ci = "WS", subgroup = setting)
pes_sec_psych_summary_by_setting

# by disorder (psychotic disorder or psychotic symptom)  ----------------------------------------------------------------

pes_sec_psych_summary_by_disorder <- meta::metaprop(tot_sec_psych, tot_sample, author_year, data = data, sm = "PFT",
                                                    method.tau = "DL", method.ci = "WS", subgroup = disorder)
pes_sec_psych_summary_by_disorder


# # by investigation  (UDS: y/n)  ----------------------------------------------------------------

pes_sec_psych_summary_by_UDS <- meta::metaprop(tot_sec_psych, tot_sample, author_year, data = data, sm = "PFT",
                                               method.tau = "DL", method.ci = "WS", subgroup = urine_freq)
pes_sec_psych_summary_by_UDS

# # by investigation  (blood: y/n)  ----------------------------------------------------------------

pes_sec_psych_summary_by_blood <- meta::metaprop(tot_sec_psych, tot_sample, author_year, data = data, sm = "PFT",
                                                 method.tau = "DL", method.ci = "WS", subgroup = blood_freq)
pes_sec_psych_summary_by_blood

# by investigation  (MRI: y/n)  ----------------------------------------------------------------

pes_sec_psych_summary_by_mri <- meta::metaprop(tot_sec_psych, tot_sample, author_year, data = data, sm = "PFT",
                                               method.tau = "DL", method.ci = "WS", subgroup = mri_freq)
pes_sec_psych_summary_by_mri

# by investigation  (CSF: y/n)  ----------------------------------------------------------------

pes_sec_psych_summary_by_csf <- meta::metaprop(tot_sec_psych, tot_sample, author_year, data = data, sm = "PFT",
                                               method.tau = "DL", method.ci = "WS", subgroup = csf_freq)
pes_sec_psych_summary_by_csf

# by investigation  (physical exam: y/n)  ----------------------------------------------------------------

pes_sec_psych_summary_by_physical_exam <- meta::metaprop(tot_sec_psych, tot_sample, author_year, data = data, sm = "PFT",
                                                         method.tau = "DL", method.ci = "WS", subgroup = physical_exam_freq)
pes_sec_psych_summary_by_physical_exam 

###################  # meta regression [based on metafor package]---------------------------------------------------------

# publication year --------------------------------------------------------------------

pes_sec_psych_summary_metareg_year <- rma(yi, vi, data = ies_da_sec_psych, mods = ~year, method = "DL")
print(pes_sec_psych_summary_metareg_year)
sec_psych_metareg_pval_year <- pes_sec_psych_summary_metareg_year$pval[2]
sec_psych_metareg_pval_year

# sample age ----------------------------------------------------------------

pes_sec_psych_summary_metareg_age <- rma(yi, vi, data = ies_da_sec_psych, mods = ~mean_age, method = "DL")
print(pes_sec_psych_summary_metareg_age)
sec_psych_metareg_pval_age <- pes_sec_psych_summary_metareg_age$pval[2]
sec_psych_metareg_pval_age

###################  outlier study detection [using studentized residuals]

# # identify studentized residuals larger than 2 (or 3 if enough studies) 
# # R comment: 'rstudent' function requires object of class "rma". multi-array average (RMA)
# 
# stud.res <- rstudent(pes_da_fep_ab) # calc Residual Values (only works on DA transformation - need to convert back?)
# abs.z <- abs(stud.res$z) # standardized residuals (externally standardized for rstudent).
# stud.res[order(-abs.z)] # ordered

###################  influential amd outlier detection 

# resources:
#  Viechtbauer, Wolfgang, and Mike W-L Cheung. 2010. “Outlier and Influence Diagnostics for Meta-Analysis.” Research Synthesis Methods 1 (2). Wiley Online Library: 112–25.
#  Baujat, Bertrand, Cédric Mahé, Jean-Pierre Pignon, and Catherine Hill. 2002. “A Graphical Method for Exploring Heterogeneity in Meta-Analyses: Application to a Meta-Analysis of 65 Trials.” Statistics in Medicine 21 (18). Wiley Online Library: 2641–52.

# Leave one out (L1O) sensitivity analysis 
pes_sec_psych_summary <- meta::metaprop(tot_sec_psych, tot_sample, author_year, data = data, sm = "PFT",
                                        method.tau = "DL", method.ci = "WS") 
precision <- sqrt(ies_da_sec_psych$vi) 
leave1out_sec_psych <- metainf(pes_sec_psych_summary, pooled = "random") 
forest(leave1out_sec_psych)

# Outlier and Influential Case Diagnostics
influence_sec_psych <- influence(pes_da_sec_psych)
print(influence_sec_psych) #  asterix for influential cases -   based on 1 o 4 criteria (see help page for details)


#################### Optional: Rerun meta analysis excluding outliers:  manually specify outlier [commented out]

# pes_fep_ab_noutlier_summary <- meta::metaprop(fep_abnormal, fep_total, author_year, data = data[-c(6),], sm = "PFT",
#                                               method.tau = "DL", method.ci = "NAsm")
# forest(pes_fep_ab_noutlier_summary)


########## publication bias  ########## 

# Egger's regression test (Regression Test for Funnel Plot Asymmetry)
eggers_reg_sec_psych <- regtest(pes_da_sec_psych, model = "rma", predictor = "sei")
print(eggers_reg_sec_psych)
funnel(pes_da_sec_psych)

########################################################################################
#                                                                                      #
# meta_mri_processing.R from Blackman et al                                            #
#                                                                                      #
# Secondary causes of psychosis: A systematic review and meta-analysis                 #
#                                                                                      #                                                                                      
#                                                                                      #
#    pre-processing script                                                             #
#                                                                                      #                                                                                      
#                                                                                      #
########################################################################################

#
# This script ensures key variables are set to the correct data types and subsets the main
# dataframes into dataframes that contain specific study types for later analysis
#

####### preparing data for analyses ######


# # Set  multiple variables to character type
subtype_char_vars <- c('author_year','setting', 
                       'continent', 'design', 'FEP','multiple_sec_psych_types', 
                       'mri_freq', 'ct_freq', 'nuclear_freq', 'eeg_freq', 
                       'csf_freq', 'blood_freq', 'urine_freq', 'psych_assess_freq', 'psych_assess_stan_tool_freq', 'physical_exam_freq', 
                       'cog_assess_freq', 'cog_assess_stan_tool_freq', 
                       'other_assess_freq', 'not_spec_assess_freq',
                       'disorder','diagnostic_tool',	'class_system')

data <- data %>%
  mutate_at(subtype_char_vars, as.character)

# Set all multiple variables to numeric type
subtype_num_vars <- c('year', 'gender_male_freq','gender_female_freq', 'mean_age', 'mean_age_sec', 'ethnicity_black', 'ethnicity_white','ethnicity_asian','ethnicity_other',   
                      'DUP','DUP_pri','DUP_sec','tot_sample', 'tot_sec_psych',  
                      'auto_freq', 'congen_freq', 'toxic_freq', 'iatrogenic_freq', 'cerebrovas_freq', 'space_oc_freq', 'metabolic_freq', 'dietary_freq', 'infec_freq', 'degen_freq', 'demye_freq', 'seizure_freq', 'sleep_freq','head_freq','endocrine_freq', 'other_sub_freq', 'unknown_sub_freq', 
                      'tot_rob_score')

data <- data %>%
   mutate_at(subtype_num_vars, as.numeric)

# First episode psychosis patients
data <- data %>%
  mutate("tot_pri_psych" = tot_sample - tot_sec_psych,na.rm = TRUE)


# rename factor levels ----------------------------------------------------


data$setting <- recode_factor(data$setting, "inpat_psych" = 'Psychiatric Hospital',
                                            "outpat_psych" = 'Psychiatric Clinic',
                                            "gen_hospital" = 'General Hospital',
                                            "mixture" = '2 or more settings')

# subsets of studies ------------------------------------------------------


# Create dataframe that includes only studies reporting FEP...
data_fep <- data %>%
  subset(FEP == "yes")

# Create dataframe based on clinical setting...

# Inpatient
data_inpat_psych <- data %>%
  subset(setting == "Psychiatric Hospital")

# Outpatient
data_outpat_psych <- data %>% 
  subset(setting == "Psychiatric Clinic")

# Accident and Emergency
data_gen_hos <- data %>% 
  subset(setting == "General Hospital")

# Mixture
data_mixture <- data %>% 
  subset(setting == "mixture")



# create indicator for age ------------------------------------------------


data <- data %>%
  mutate(age_over_35= ifelse(mean_age > 35, "0", "1"))


subtype_char_vars <- c('age_over_35')

data <- data %>%
  mutate_at(subtype_char_vars, as.character)




# create age bins (<24, 25-35, 35+) ----------------------------------------------


data <- data %>%
  mutate(age_bins= 
           ifelse(mean_age > 35, "over_35",
                  ifelse(mean_age > 25, "25_35",
                         "under_25")))

# filter by age
data_under_25 <- data %>% 
  subset(age_bins == "under_25")

data_25_35 <- data %>% 
  subset(age_bins == "25_35")

data_over_35 <- data %>% 
  subset(age_bins == "over_35")

data_under_35 <- data %>% 
  subset(age_bins != "over_35")


# create binary variable for psychiatric setting (psychiatric/general)------------------------------------------------

data <- data %>%
  mutate(Setting= 
           ifelse(setting == "Psychiatric Hospital" | setting == "Psychiatric Clinic", "Psychiatric", 
                  ifelse(setting == "General Hospital", "General Hospital",
                         "Mixture")))

# create indicator for performing investigations (1= performed in all patients)------------------------------------------------

# Urine analysis
data <- data %>%
  mutate(urine_all = ifelse(data$urine_freq == "all", "1", "0"))

# Bloods 
data <- data %>%
  mutate(blood_all = ifelse(data$blood_freq == "all", "1", "0"))

# MRI
data <- data %>%
  mutate(mri_all = ifelse(data$mri_freq == "all", "1", "0"))

# CSF
data <- data %>%
  mutate(csf_all = ifelse(data$csf_freq == "all", "1", "0"))


# UDS + Bloods  
data <- data %>%
  mutate(urine_blood_all = ifelse(data$urine_freq == "all" & data$blood_freq == "all", "1", "0"))


# UDS + MRI 
data <- data %>%
  mutate(urine_mri_all = ifelse(data$urine_freq == "all" & data$mri_freq == "all", "1", "0"))

# Bloods + MRI 
data <- data %>%
  mutate(blood_mri_all = ifelse(data$blood_freq == "all" & data$mri_freq=="all", "1", "0"))


# UDS+ Bloods + MRI
data <- data %>%
  mutate(urine_blood_mri_all = ifelse(data$urine_freq == "all" & data$blood_freq == "all" & data$mri_freq == "all", "1", "0"))




# create indicator for absence investigations (1 = not done in any patients, or not reported)------------------------------------------------

# Urine analysis
data <- data %>%
  mutate(urine_none = ifelse(data$urine_freq == "NS", "1", "0"))

# Bloods 
data <- data %>%
  mutate(blood_none = ifelse(data$blood_freq == "NS", "1", "0"))

# MRI
data <- data %>%
  mutate(mri_none = ifelse(data$mri_freq == "NS", "1", "0"))

# UDS + Bloods  [n=5]
data <- data %>%
  mutate(urine_blood_none = ifelse(data$urine_freq == "NS" & data$blood_freq == "NS", "1", "0"))

# UDS + MRI [n=2]
data <- data %>%
  mutate(urine_mri_none = ifelse(data$urine_freq == "NS" & data$mri_freq == "NS", "1", "0"))

# Bloods + MRI [n=2]
data <- data %>%
  mutate(blood_mri_none = ifelse(data$blood_freq == "NS" & data$mri_freq=="NS", "1", "0"))

# UDS + Bloods + MRI
data <- data %>%
  mutate(urine_blood_mri_none = ifelse(data$urine_freq == "NS" & data$blood_freq == "NS" & data$mri_freq == "NS", "1", "0"))



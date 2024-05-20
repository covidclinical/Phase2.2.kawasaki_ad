library(dplyr)
library(ggplot2)
library(lubridate)
library(table1)

folder_4ce_files <- "/4ceData/Input/4CEAllAdminChortsOct2023/"
obfuscation =  FALSE
dateFormat <- "%Y-%m-%d"
data_update_date <- "2023-10-01"

demo_raw <-  data.table::fread(paste0( folder_4ce_files, "LocalPatientSummary.csv"), sep = ',',skip = 0, fill=TRUE )
obs_raw <-  data.table::fread(paste0( folder_4ce_files, "LocalPatientObservations.csv"), sep = ',',skip = 0, fill=TRUE)
clinical_raw <-  data.table::fread(paste0( folder_4ce_files, "LocalPatientClinicalCourse.csv"), sep = ',',skip = 0, fill=TRUE)

### Extract the patient summary and observation information. 
# arrange in a consistent way to make sure we are not assuming the order of some sites
# select only the study period
demo_raw <- demo_raw %>% 
  dplyr::mutate( age = trunc( age ) ) %>%
  dplyr::filter(cohort == "AllAdm", 
                age >= 0,
                as.Date(admission_date, format = dateFormat) >= ymd('2019-01-01'),
                as.Date(admission_date, format = dateFormat) <= ymd('2023-03-31')) %>%
  dplyr::arrange(patient_num, as.Date(admission_date, format = dateFormat), days_since_admission)
obs_raw <- obs_raw %>% 
  dplyr::filter(cohort == "AllAdm") %>%
  dplyr::arrange(patient_num, days_since_admission)
clinical_raw <- clinical_raw %>% 
  dplyr::filter(cohort == "AllAdm",
                as.Date(calendar_date, format = dateFormat) >= ymd('2019-01-01'),
                as.Date(calendar_date, format = dateFormat) <= ymd('2023-03-31')) %>%
  dplyr::arrange(patient_num, as.Date(calendar_date, format = dateFormat), days_since_admission)


#remove potential duplicates
clinical_raw <- clinical_raw %>% unique()
demo_raw <- demo_raw %>% unique()
obs_raw <- obs_raw %>% unique()

# Filtering on inpatients here (in_hospital == 1), to reduce subsequent compute time of the encounter length calculation 
clinical_raw <- dplyr::filter(clinical_raw, in_hospital == 1)

obs_raw <- as.data.frame( obs_raw )
demo_raw <- as.data.frame( demo_raw )
clinical_raw <- as.data.frame( clinical_raw )

obs_raw_age <- left_join(obs_raw, demo_raw[c("patient_num", "age", "dead", "death_date")], by = c("patient_num")) %>%
  dplyr::mutate( concept_code = gsub("\\.", "",concept_code )) %>%
  dplyr::inner_join(clinical_raw[, c("patient_num", "days_since_admission", "calendar_date")], 
                    by = c("patient_num", "days_since_admission"))

obs_raw_age <- obs_raw_age %>%
  dplyr::mutate( age_time_diagnosis = age - floor(as.numeric(as.Date(data_update_date, format = dateFormat) - as.Date(calendar_date, format = dateFormat)) / 365.25)) %>%
  dplyr::select( -dead, -death_date )

obs_filtered_inclusion_criteria <- obs_raw_age %>% 
  dplyr::filter(concept_type == 'DIAG-ICD10',
                days_since_admission >= 0, 
                age_time_diagnosis < 18 )


### select disorder of inerest based on ICD codes
#diseaseOfInterest <- c("M303")
diseaseOfInterest <- autoimmune_data %>%
  dplyr::mutate( concept_code = gsub("[.]", "", autoimmune_data$ICD_Code))


obs_disease_interest <- obs_filtered_inclusion_criteria %>%
  dplyr::filter( concept_code %in% diseaseOfInterest$concept_code ) %>%
  dplyr::left_join( diseaseOfInterest, by = "concept_code") %>%
  dplyr::select( - ICD_Code )

## for the disease and patients of interest get the hospitalization information
source("/4ceData/Phase2.2.bacterial_infections/R/count_sequences_hospitalisation.R")

clinical_raw_doi <- clinical_raw %>%
  dplyr::filter( patient_num %in% obs_disease_interest$patient_num )

clinical_raw_doi <- count_hosp(clinical_raw_doi) %>%
  dplyr::ungroup() %>%
  dplyr::select( patient_num, days_since_admission, n_hospitalization, length_hospitalization, admit_date )

toPlot_formated <- obs_disease_interest %>%
  dplyr::left_join( clinical_raw_doi, by=c("patient_num", "days_since_admission")) %>%
  dplyr::mutate( time_p = as.Date(cut( as.Date(admit_date), breaks = "months"))) %>%
  dplyr::filter( as.Date(time_p) >= ymd('2019-01-01'),
                 as.Date(time_p) <= ymd('2023-02-01'))
  
toPlot_counts <- toPlot_formated %>%
#  dplyr::group_by( time_p ) %>%
  dplyr::group_by( time_p, Category ) %>%
  dplyr::summarise( n = n_distinct( patient_num ))

## remove categories just with 1 case 
toPlot_counts <- toPlot_counts %>%
  dplyr::filter( ! Category %in% c("Ankylosing Spondylitis", "Pemphigus Vulgaris", "Sarcoidosis"))

# plot by month
ggplot2::ggplot(data=toPlot_counts, aes(x=time_p, y=n)) +
  ggplot2::facet_wrap( ~Category, scales = "free" ) + 
  ggplot2::geom_line() +
  ggplot2::scale_x_date(date_labels="%b-%y",date_breaks  ="3 month")+
  ggplot2::labs(y = "Number of patients",
                x = "Date (by month)",
                title = "Number of monthly hospitazations with Autoimmune Diseases") +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.x = element_text(angle = 60, hjust = 1))

demog_table <- toPlot %>%
  dplyr::left_join( demo_raw %>% dplyr::select( patient_num, sex ), by = "patient_num" ) %>%
  dplyr::select( patient_num, sex, age_time_diagnosis, length_hospitalization ) %>%
  unique()

table1(~ sex + age_time_diagnosis +  length_hospitalization, 
       data = demog_table, caption = "Table 1. Kawasaki Disease Patients (BCH)")


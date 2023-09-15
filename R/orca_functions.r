#API GET DATA FUNCTION FOR ORCA SURVEYS PROJECT

url <- 'https://redcap.nyu.edu/api/'

#' @title Pulling ORCA Data
#' @description This function retrieves data from a REDCap project using the API.
#' @param token The API token for the project
#' @param form The name of the REDCap form to retrieve data from
#' @param raw_v_label The label for raw data fields
#' @param form_complete Indicating whether you want to return all responses or just ones marked as complete (default is all)
#' @return A data frame with the retrieved data
#' @export

get_orca_data <- function(token = token, form = form, raw_v_label = 'raw', form_complete = T) {
  if (form_complete) {
    record_filter = paste("[", form, "_complete]=2", sep = "")
  } else {
    record_filter = ""
  }
  formData <- list(uri = url,
                   "token"=token,
                   content='record',
                   format='csv',
                   type='flat',
                   csvDelimiter='',
                   'fields[0]'='record_id', 
                   'forms[0]'=form,
                   rawOrLabel=raw_v_label,
                   rawOrLabelHeaders=raw_v_label,
                   exportCheckboxLabel='false',
                   exportSurveyFields='true',
                   exportDataAccessGroups='false',
                   returnFormat='csv',
                   filterLogic=record_filter)
  
  response <- httr::POST(url, body = formData, encode = "form")
  df <- httr::content(response)
  df[df == -888] = NA
  df[df == 888] = NA
  df[df == 8888] = NA
  df[df == -999] = NA
  df[df == 999] = NA
  df[df == 9999] = NA # NOTE values can be changed, or new ones added, to account for different missing data flags
  df <- dplyr::filter(df, !stringr::str_detect(record_id, "TEST"))
  return (df)
}

#' @title Pulling ORCA Field
#' @description PULLS RECORD ID AND INDIVIDUAL FIELD
#' @param token Unique REDCap token ID
#' @param field Name of the specific field to be downloaded
#' @param raw_v_label Whether raw data or labels are requested
#' @return A data frame for the completed record_ids, redcap_event_name and field
#' @export
get_orca_field <- function(token = token, field=field, raw_v_label = 'raw') {
  formData <- list(uri = url,
                   "token"=token,
                   content='record',
                   format='csv',
                   type='flat',
                   csvDelimiter='',
                   'fields[0]'='record_id',
                   'fields[1]' = field,
                   rawOrLabel=raw_v_label,
                   rawOrLabelHeaders=raw_v_label,
                   exportCheckboxLabel='false',
                   exportSurveyFields='false',
                   exportDataAccessGroups='false',
                   returnFormat='csv')
  
  response <- httr::POST(url, body = formData, encode = "form")
  df <- httr::content(response)
  df[df == -888] = NA
  df[df == 888] = NA
  df[df == 8888] = NA
  df[df == -999] = NA
  df[df == 999] = NA
  df[df == 9999] = NA
  df <- dplyr::select(df, record_id, redcap_event_name, field)
  df <- df[!is.na(df[[field]]),]
  df <- dplyr::filter(df, !stringr::str_detect(record_id, "TEST"))
  return (df)
}

#' @title Pulling ORCA Data quality
#' @description RETURNS DATA QUALITY SCORES FOR EACH TASK SPECIFIED
#'
#' @param token The API token for the project
#' @param tasks The tasks you wish to export data quality for - if multiple, place in vector
#' @return A data frame with the retrieved data
#' @export

get_orca_dq <- function(token, tasks = c("cecile", "maap", "memory", "vpc")) {
  dq <- get_orca_data(token, form = "data_quality")
  notes <- dplyr::select(dq, record_id, cecile_dq_notes, memory_dq_notes, vpc_dq_notes, maap_dq_notes)
  notes <- dplyr::rename(notes, cecile = cecile_dq_notes, memory = memory_dq_notes, vpc = vpc_dq_notes, maap = maap_dq_notes)
  dq <- dplyr::rename(dq, cecile = cecile_dq, maap = maap_dq, memory = memory_dq, vpc = vpc_dq)
  dq <- dplyr::select(dq, record_id, tasks)
  notes <- dplyr::select(notes, record_id, tasks)
  dq <- dplyr::left_join(dq, notes, by="record_id")
  return (dq)
}


#' @title Replacing Multiple Values Function
#' @description ALLOWS DATA TO BE REPLACED IN A COLUMN
#' @param data The dataframe
#' @param column_name The column you wish to work on
#' @param old_values Vector of string variables you wish to replace
#' @param new_values Vector of string variables you wish to replace the old ones with 
#' @return the data frame with old strings in specified column replaced with new strings
#' @export
#' 
replace_multiple <- function(data, column_name, old_values, new_values) {
  data[[column_name]] <- new_values[match(data[[column_name]], old_values)]
  return(data)
}


#' @title Pulls sociodemographic cleaned responses 
#' @description This cleans and pulls demographic information
#' @param token The API token for the project
#' @return the completed sociodemographic responses with numeric categories replaced with character
#' @export

get_orca_demo <- function(token) {
  demo <- get_orca_data(token, form = "sociodemographic", form_complete = T)
  demo <- dplyr::select(demo, record_id, sociodemographic_timestamp, child_dob:socio_comments)
  demo <- replace_multiple(demo, "caregiver_ed", old_values = c(1,2,3,4,5), new_values = c("less than hs", "high school degree/ged","partial college", "bachelor's degree", "graduate degree"))
  demo <- replace_multiple(demo, "marital_status", old_values = c(1,2,3,4,5,6,7), new_values = c("married", "single living together", "single never married", "separated", "divorced", "widowed", "other"))
  return(demo)
}


#' @title Process CHAOS data
#'
#' @description This function will download and compute total scores for the CHAOS scale
#'
#' @param token Unique REDCap token ID
#' @return A data frame for the completed surveys
#' @export
get_orca_chaos <- function(token) {
  chaos = get_orca_data(token, "chaos", form_complete = T)
  pos_items <- c("chaos_commotion","chaos_findthings","chaos_ontop","chaos_interrupt","chaos_relax", "chaos_calm")
  chaos <- dplyr::mutate_at(chaos, dplyr::vars(all_of(pos_items)), ~ abs(5 - .))
  chaos$total_score =  rowMeans(chaos[,c("chaos_commotion", "chaos_findthings", "chaos_rushed", "chaos_ontop",
                                         "chaos_late", "chaos_zoo", "chaos_interrupt", "chaos_fuss", "chaos_plans",
                                         "chaos_think", "chaos_arguement", "chaos_relax", "chaos_telephone","chaos_calm")], na.rm=T)
  chaos = chaos[,c("record_id", "chaos_timestamp", "total_score")]
  return (chaos)
}

#' @title Process EPDS postpartum depression data
#'
#' @description This function will download and return the totalsummed scores for the EPDS. Only total scores, and not cutoff values, are returned.
#'
#' @param token Unique REDCap token ID
#' @return A data frame for the completed surveys
#' @export
get_orca_epds <- function(token, timepoint = 'orca_visit_1_arm_1') {
  epds = get_orca_data(token, "epds", form_complete = T)
  epds <- dplyr::filter(epds, redcap_event_name == timepoint)
  epds$epds_1_v2 = 3 - epds$epds_1_v2
  epds$epds_2_v2 = 3 - epds$epds_2_v2
  epds$epds_all = epds$epds_1_v2 + epds$epds_2_v2 + epds$epds_3_v2 + epds$epds_4_v2 + epds$epds_5_v2 +
    epds$epds_6_v2 + epds$epds_7_v2 + epds$epds_8_v2 + epds$epds_9_v2 + epds$epds_10_v2
  
  epds$epds_dep = epds$epds_1_v2 + epds$epds_2_v2 + epds$epds_3_v2 +
    epds$epds_7_v2 + epds$epds_8_v2 + epds$epds_9_v2 + epds$epds_10_v2
  
  epds$epds_anx = epds$epds_4_v2 + epds$epds_5_v2 + epds$epds_6_v2
  
  #epds$epds_avg = rowSums(epds[,5:14], na.rm=T)
  epds = epds[,c("record_id", "epds_all", "epds_dep", "epds_anx")]
  return (epds)
}


#' @title Process Material Deprivation data
#'
#' @description  This function will download and return the total summed scores 
#'
#' @param token Unique REDCap token ID
#' @return A data frame with the calculated md score for each ID
#' @export
get_orca_md <- function(token) {
  md <- get_orca_data(token, "material_deprivation", form_complete = T)
  md_columns <- dplyr::select(md, md_snap:md_overtime)
  md$material_deprivation <- rowSums(md_columns)
  md <- dplyr::select(md, record_id, md_score)
  return (md)
}

#' @title Process Social Support data
#'
#' @description This function will download and return the total summed scores 
#'
#' @param token Unique REDCap token ID
#' @return A data frame with the calculated social support score for each ID
#' @export
get_orca_social_support <- function(token, timepoint = 'orca_visit_1_arm_1') {
  ss <- get_orca_data(token, "social_support", form_complete = T)
  ss <- dplyr::filter(ss, redcap_event_name == timepoint)
  ss_columns <- dplyr::select(ss, support3:support10)
  ss$social_support <- rowSums(ss_columns, na.rm=T)
  ss <- dplyr::select(ss, record_id, social_support)
  return (ss)
}

#ORCA SCREENER FUNCTIONS

#' @title Pulling raw orca screener form
#' @description This function pulls raw forms from ORCA screener redcap project
#' @param token REDCap API token for ORCA screener redcap project
#' @param form name of form from the ORCA screener redcap project
#' @param raw_v_label whether to pull raw or labelled responses 
#' @return a dataframe of the form pulled
#' @export
get_orca_screener <- function(token = token, form = form, raw_v_label = 'raw') {
  record_filter = paste("[", form, "_complete]=2", sep = "")
  formData <- list(uri = url,
                   "token"=token,
                   content='record',
                   format='csv',
                   type='flat',
                   csvDelimiter='',
                   'fields[0]'='record_id', # NOTE this is the subject ID for COPE, change if different
                   'forms[0]'=form,
                   rawOrLabel=raw_v_label,
                   rawOrLabelHeaders=raw_v_label,
                   exportCheckboxLabel='false',
                   exportSurveyFields='true',
                   exportDataAccessGroups='false',
                   returnFormat='csv',
                   filterLogic=record_filter)
  
  response <- httr::POST(url, body = formData, encode = "form")
  df <- httr::content(response)
  df[df == -888] = NA
  df[df == 888] = NA
  df[df == 8888] = NA
  df[df == -999] = NA
  df[df == 999] = NA
  df[df == 9999] = NA # NOTE values can be changed, or new ones added, to account for different missing data flags
  return (df)
}

#' @title Cleaning screener race responses
#' @description This function cleans race responses 
#' @param data data frame to act upon
#' @return a dataframe with the race responses cleaned
#' @export
clean_race <- function(data) {
  data$rec___1 <- gsub("1", "American Indian/Alaska Native", data$rec___1)
  data$rec___2 <- gsub("1", "Asian", data$rec___2)
  data$rec___3 <- gsub("1", "Black/African American/African", data$rec___3)
  data$rec___4 <- gsub("1", "Hispanic/Latino", data$rec___4)
  data$rec___5 <- gsub("1", "Middle Eastern/North African", data$rec___5)
  data$rec___6 <- gsub("1", "Native Hawaiian", data$rec___6)
  data$rec___7 <- gsub("1", "White", data$rec___7)
  data$rec___8 <- gsub("1", "None fully describe", data$rec___8)
  data$rec___9 <- gsub("1", "Prefer not to answer", data$rec___9)
  data <- unite(data, "race", rec___1:rec___9)
  data$race <- gsub("0", "", data$race)
  data$race <- gsub("_", "", data$race)
  return (data)
}

#' @title Cleaning screener education responses
#' @description This function cleans education responses 
#' @param data data frame to act upon
#' @return a dataframe with the education responses cleaned
#' @export
clean_education <- function(data) {
  data$rec_education_level___1 <- gsub("1", "High School or Less", data$rec_education_level___1)
  data$rec_education_level___2 <- gsub("1", "Less than College", data$rec_education_level___2)
  data$rec_education_level___3 <- gsub("1", "College Degree", data$rec_education_level___3)
  data$rec_education_level___4 <- gsub("1", "Graduate Degree", data$rec_education_level___4)
  data <- unite(data, "education", rec_education_level___1:rec_education_level___4)
  data$education <- gsub("0", "", data$education)
  data$education <- gsub("_", "", data$education)
  return (data)
}


#' @title ZIP CODE AND LOCATION FUNCTION
#' @description This function pulls public US zipcode data and cleans it
#' @return a dataframe with the race responses cleaned
#' @export
zips <- function() {
  zipcodeR::download_zip_data(force=T)
  zip_info <- select(zipcodeR::zip_code_db, zipcode, major_city, state, timezone)
  zip_info$zipcode <- as.integer(zip_info$zipcode)
  zip_info <- mutate(zip_info,
                     final_city = ifelse(zipcode >= 10001 & zipcode <= 10282, "New York",
                                         ifelse(zipcode >= 10301 & zipcode <= 10314, "New York",
                                                ifelse(zipcode >= 10451 & zipcode <= 10475, "New York",
                                                       ifelse(zipcode >= 11004 & zipcode <= 11109, "New York",
                                                              ifelse(zipcode >= 11351 & zipcode <= 11697, "New York",
                                                                     ifelse(zipcode >= 11201 & zipcode <= 11256, "New York", major_city)))))))
  zip_info$location <- str_c(zip_info$final_city, " ", zip_info$state)
  zip_info <- select(zip_info, zipcode, location, timezone)
  zip_info <- data.frame(zip_info)
  return(zip_info)
}

#' @title Pulling City Data
#' @description this function pulls us city population data
#' @param data data frame to act upon
#' @return a dataframe with the race responses cleaned
#' @export
city_info <- function() {
  cities <- maps::us.cities
  cities <- select(cities, name, pop)
  cities <- rename(cities, location = name, population=pop)
  cities <- data.frame(cities)
  return(cities)
}


#' @title Pulling cleaned screener responses 
#' @description this function pulls and cleans screener data for import to excel tracker 
#' @param token Unique REDCap API token
#' @param min_date_time The minimum timestamp to pull responses after
#' @return a dataframe with the race responses cleaned
#' @export
get_orca_screener_clean <- function(token, min_date_time = "2022-01-01 00:00:00") {
  library(tidyverse)
  screener = get_orca_data(token, "orca_screener_survey")
  screener = select(screener, -redcap_survey_identifier, -rec_unique_record_id, -orca_screener_survey_complete, -record_id)
  screener = rename(screener, 
                    timestamp = orca_screener_survey_timestamp,
                    language = rec_language_preference,
                    caregiver_name = rec_caregiver_name,
                    phone = rec_phone_number,
                    texting_okay = rec_phone_number_text,
                    email = rec_caregiver_email,
                    over_18 = rec_over_18,
                    zipcode = rec_address_zipcode,
                    race_other = rec_race_other,
                    children_amount = rec_child_number,
                    child_dob_1 = rec_babydob_one,
                    child_dob_2 = rec_babydob_two,
                    child_dob_3 = rec_babydob_three,
                    child_dob_4 = rec_babydob_four,
                    child_dob_5 = rec_babydob_five,
                    multiple_child_dobs = rec_babydob_sixplus)
  screener$timestamp = strptime(screener$timestamp, format = "%Y-%m-%d %H:%M:%S") 
  min_date_time = strptime(min_date_time, format = "%Y-%m-%d %H:%M:%S") 
  screener = filter(screener, timestamp > min_date_time)
  screener = clean_race(screener)
  screener = clean_education(screener)
  screener$zipcode <- as.integer(screener$zipcode)
  zip_info <- zips()
  cities <- city_info()
  screener <- left_join(screener, zip_info, by="zipcode")
  screener <- left_join(screener, cities, by="location")
  screener <- filter(screener, is.na(bot_check) | bot_check == 3)
  screener <- filter(screener, is.na(bot_pic_answer) | bot_pic_answer == 4)
  screener <- mutate(screener,
                     rural = ifelse(is.na(population) & !is.na(location), "Y",
                                    ifelse(population >= 50000, "N",
                                           ifelse(population < 50000, "Y", "CHECK"))))
  screener <- mutate(screener, non_white = ifelse(race == "White", "N", "Y"))
  screener <- mutate(screener, low_ses = ifelse(education == "Graduate degree", "N",
                                                ifelse(education == "College degree", "N", "Y")))
  screener$texting_okay <- gsub(1, "Yes", screener$texting_okay)
  screener$texting_okay <- gsub(0, "No", screener$texting_okay)
  screener$over_18 <- gsub(1, "Yes", screener$over_18)
  screener$over_18 <- gsub(0, "No", screener$over_18)
  screener$child_name <- ""
  screener$number_valid <- ""
  col_order <- c("caregiver_name", "child_name", "language", "phone", "number_valid", "texting_okay", "email", "over_18", "zipcode", "race", "race_other", "education", "children_amount", "child_dob_1", "child_dob_2", "child_dob_3", "child_dob_4", "child_dob_5", "multiple_child_dobs", "timestamp", "timezone", "location", "rural", "non_white", "low_ses")
  screener <- screener[, col_order]
  screener <- dplyr::arrange(screener, timestamp)
  return (screener)
}

#' @title INELIGIBLE AGES
#' @description Flags any child ages older than 380 days from today
#' @param data The data frame you wish to act on
#' @param threshold_date The date threshold you wish to compare - default is 380 days prior to today
#' @return A dataframe with a new column marking 1 for anybody too old to participate
#' @export
flag_ineligible_age <- function(data, threshold_date = Sys.Date() - 380) {
  library(dplyr)
  cols <- c("child_dob_1", "child_dob_2", "child_dob_3", "child_dob_4", "child_dob_5")
  for (p in 1:nrow(data)) {
    date_vec <- na.omit(as.numeric(data[p, cols]))
    date_vec <- as.Date(date_vec, origin="1970-01-01") 
    data[p, "age_ineligible"] <- ifelse(all(date_vec <= threshold_date), 1, 0)
  }
  return(data)
}

#' @title Duplicate contacts
#' @description Flags any participant with a duplicate phone or email
#' @param data The data frame you wish to act on
#' @return A dataframe with 2 new columns marking 1 for anybody with a duplicate phone or email
#' @export
flag_duplicate_contacts <- function(data) {
  library(dplyr)
  data <- data %>%
    group_by(email) %>%
    mutate(duplicate_email = ifelse(!is.na(email) & n() > 1, 1, 0)) %>%
    ungroup() %>%
    group_by(phone) %>%
    mutate(duplicate_phone = ifelse(!is.na(phone) & n() > 1, 1, 0)) %>%
    ungroup()
  return(data)
}

#' @title Lowercase Names
#' @description Flags any participant where their name is all lowercase
#' @param data The data frame you wish to act on
#' @return A dataframe with a new column marking 1 for anybody with an all lowercase names
#' @export
flag_lowercase_names <- function(data) {
  library(dplyr)
  data <- data %>%
    mutate(lower_name_flag = ifelse(!grepl("[[:upper:]]", caregiver_name), 1, 0))
  return(data)
}

#' @title Flagging Numeric Names
#' @description Flags any participant with numbers in their name
#' @param data The data frame you wish to act on
#' @return A dataframe with a new column marking 1 for anybody who has numeric values in their name 
#' @export
flag_numeric_names <- function(data) {
  library(dplyr)
  data <- data %>%
    mutate(num_name_flag = ifelse(grepl("[0-9]", caregiver_name), 1, 0))
  return(data)
}

#' @title Screens screener export
#' @description Checks for: duplicate contact info, under 18, numeric names, NA names/emails, babies above age threshold, names all lowercase
#' @param data The data frame you wish to act on
#' @return A list with the filtered screener, and a new column flagging lowercase names (doesn't remove). List also contains dataset with removed responses with duplicate contact info and ineligible ages
#' @export
screen_fraudulence <- function(data) {
  library(dplyr)
  data <- data %>%
    flag_ineligible_age(threshold_date = Sys.Date() - 380) %>%
    flag_duplicate_contacts() %>%
    flag_lowercase_names() %>%
    flag_numeric_names()
  #creating new datasets
  ineligible_ages <- data %>%
    filter(age_ineligible == 1)
  duplicate_contacts <- data %>%
    filter(duplicate_email == 1 | duplicate_phone == 1)
  #removing NA names, numeric names, under 18 caregivers, duplicate contact info and age_ineligible babies
  data <- data %>%
    filter(!is.na(caregiver_name) & !is.na(email) & over_18 == "Yes" & num_name_flag == 0 & age_ineligible == 0)
  data <- data %>%
    filter(duplicate_email == 0 & duplicate_phone == 0)
  #removing excess columns other than flagged lowercase names 
  data <- data %>%
    select(-age_ineligible, -duplicate_email, -duplicate_phone, -num_name_flag)
  return(list(data = data, ineligible_ages = ineligible_ages, duplicate_contacts = duplicate_contacts))
}

#' @title Pulls US zipcode database
#' @description Loads a data base of US zipcodes and information (latitude, longitude, population)
#' @return A data frame with every us zipcode and information
#' @export
zip_data <- function() {
  data_path <- system.file("data", "zip_code_database.csv", package = "OrcaData")
  data <- read.csv(data_path, na.strings = "")
  data$zipcode <- as.character(data$zipcode)
  data$zipcode <- sprintf("%05s", data$zipcode)
  data <- dplyr::mutate(data,
                        town_or_city = ifelse(zipcode >= 10001 & zipcode <= 10282, "New York",
                                          ifelse(zipcode >= 10301 & zipcode <= 10314, "New York",
                                                 ifelse(zipcode >= 10451 & zipcode <= 10475, "New York",
                                                        ifelse(zipcode >= 11004 & zipcode <= 11109, "New York",
                                                               ifelse(zipcode >= 11351 & zipcode <= 11697, "New York",
                                                                      ifelse(zipcode >= 11201 & zipcode <= 11256, "New York", primary_city)))))))
  data <- dplyr::select(data,
                        zipcode, town_or_city, state, county, timezone, latitude, longitude, irs_estimated_population)
  return(data)
}


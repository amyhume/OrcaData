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
  df <- dplyr::filter(df, !stringr::str_detect(record_id, "IRB"))
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
#' @param timepoint Redcap event name for timepoint
#' @param timestamp Boolean - whether to include survey timestamps
#' @return A data frame for the completed surveys
#' @export
get_orca_chaos <- function(token, timepoint = 'orca_visit_1_arm_1', timestamp=T) {
  chaos = get_orca_data(token, "chaos", form_complete = T)
  chaos = dplyr::filter(chaos, redcap_event_name == timepoint)
  pos_items <- c("chaos_commotion","chaos_findthings","chaos_ontop","chaos_interrupt","chaos_relax", "chaos_calm")
  chaos <- dplyr::mutate_at(chaos, dplyr::vars(all_of(pos_items)), ~ abs(5 - .))
  chaos$total_score =  rowMeans(chaos[,c("chaos_commotion", "chaos_findthings", "chaos_rushed", "chaos_ontop",
                                         "chaos_late", "chaos_zoo", "chaos_interrupt", "chaos_fuss", "chaos_plans",
                                         "chaos_think", "chaos_arguement", "chaos_relax", "chaos_telephone","chaos_calm")], na.rm=T)
  
  
  if (timestamp) {
    chaos = chaos[,c("record_id", "chaos_timestamp", "total_score")]
  } else if (!timestamp) {
    chaos = chaos[,c("record_id", "total_score")]
  }
  return (chaos)
}

#' @title Process EPDS postpartum depression data
#'
#' @description This function will download and return the totalsummed scores for the EPDS. Only total scores, and not cutoff values, are returned.
#'
#' @param token Unique REDCap token ID
#' @param timepoint Redcap event name for timepoint
#' @param timestamp Boolean - whether to include survey timestamps
#' @return A data frame for the completed surveys
#' @export
get_orca_epds <- function(token, timepoint = 'orca_visit_1_arm_1', timestamp=T) {
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
  
  if (timestamp) {
    epds = epds[,c("record_id", "epds_timestamp", "epds_all", "epds_dep", "epds_anx")]
  } else if (!timestamp) {
    epds = epds[,c("record_id", "epds_all", "epds_dep", "epds_anx")]
  }
  
  return (epds)
}


#' @title Process Material Deprivation data
#'
#' @description  This function will download and return the total summed scores 
#'
#' @param token Unique REDCap token ID
#' @param timestamp Boolean - whether to include survey timestamps
#' @return A data frame with the calculated md score for each ID
#' @export
get_orca_md <- function(token, timestamp = T) {
  md <- get_orca_data(token, "material_deprivation", form_complete = T)
  md_columns <- dplyr::select(md, md_snap:md_overtime)
  md$md_score <- rowSums(md_columns)
  
  if (timestamp) {
    md <- dplyr::select(md, record_id, material_deprivation_timestamp, md_score)
  } else if (!timestamp) {
    md <- dplyr::select(md, record_id, md_score)
  }
  
  return (md)
}

#' @title Process Social Support data
#'
#' @description This function will download and return the total summed scores 
#'
#' @param token Unique REDCap token ID
#' @param timepoint Redcap event name for timepoint
#' @param timestamp Boolean - whether to include survey timestamps
#' @return A data frame with the calculated social support score for each ID
#' @export
get_orca_social_support <- function(token, timepoint = 'orca_visit_1_arm_1', timestamp=T) {
  ss <- get_orca_data(token, "social_support", form_complete = T)
  ss <- dplyr::filter(ss, redcap_event_name == timepoint)
  ss_columns <- dplyr::select(ss, support3:support10)
  ss$social_support <- rowSums(ss_columns, na.rm=T)
  ss <- dplyr::select(ss, record_id, social_support)
  
  if (timestamp) {
    ss <- dplyr::select(ss, record_id, social_support_timestamp, social_support)
  } else if (!timestamp) {
    ss <- dplyr::select(ss, record_id, social_support)
  }
  
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

#' @title Process IBQ Data for Visit 2
#' @description This function will download and return the mean scores for the IBQ subscales (surgency, negative affect, effortful control).
#' @param token Unique REDCap token ID
#' @param timestamp Boolean for whether to include survey timestamp
#' @return A data frame for the completed surveys
#' @export
get_orca_ibq <- function(token, timestamp=T) {
  ibq <- get_orca_data(token, form = 'infant_behavior_questionnaire_vsf')
  #reversing item 11
  ibq$ibq11r <- (8-ibq$ibq11)
  ibq$ibq_sur <- rowMeans(ibq[, c("ibq1", "ibq2", "ibq7", "ibq8", "ibq13", "ibq14", "ibq15", 
                                  "ibq20", "ibq21", "ibq26", "ibq27", "ibq36", "ibq37")], na.rm = TRUE)
  ibq$ibq_neg <- rowMeans(ibq[, c("ibq3", "ibq4", "ibq9", "ibq10", "ibq16", "ibq17", "ibq22", 
                                  "ibq23", "ibq28", "ibq29", "ibq32", "ibq33")], na.rm = TRUE)
  ibq$ibq_ec <- rowMeans(ibq[, c("ibq5", "ibq6", "ibq11r", "ibq12", "ibq18", "ibq19", "ibq24", 
                                 "ibq25", "ibq30", "ibq31", "ibq34", "ibq35")], na.rm = TRUE)
  
  if (timestamp) {
    ibq <- ibq[, c("record_id", "infant_behavior_questionnaire_vsf_timestamp", "ibq_sur", "ibq_neg", "ibq_ec")]
  } else if (!timestamp) {
    ibq <- ibq[, c("record_id", "ibq_sur", "ibq_neg", "ibq_ec")]
  }
  
  return(ibq)
}

#' @title Process Early Executive Functions Questionnaire Data for Visit 2
#' @description This function will download and return the mean scores for the EEFQ subscales (ic, fx, wm, rg).
#' @param token Unique REDCap token ID
#' @param timestamp Boolean for whether to include survey timestamp
#' @return A data frame for the completed surveys
#' @export
get_orca_eefq <- function(token, timestamp=T) {
  eefq <- get_orca_data(token, form = "early_executive_functions_questionnaire")
  
  reverse <- c("eefq3", "eefq9", "eefq11", "eefq16", "eefq22", "eefq23", "eefq24", "eefq25", "eefq26", "eefq27", "eefq28")
  eefq[reverse] <- lapply(eefq[reverse], function(x) 8 - x)
  
  ic <- c("eefq1", "eefq2", "eefq3", "eefq4", "eefq5", "eefq6", "eefq7")
  fx <- c("eefq8", "eefq9", "eefq10", "eefq11", "eefq12", "eefq13", "eefq14")
  wm <- c("eefq15", "eefq16", "eefq17", "eefq18", "eefq19", "eefq20")
  rg <- c("eefq21", "eefq22", "eefq23", "eefq24", "eefq25", "eefq26", "eefq27", "eefq28")
  
  eefq$eefq_ic <- rowMeans(eefq[, ic], na.rm=T)
  eefq$eefq_fx <- rowMeans(eefq[, fx], na.rm=T)
  eefq$eefq_wm <- rowMeans(eefq[, wm], na.rm=T)
  eefq$eefq_rg <- rowMeans(eefq[, rg], na.rm=T)
  
  if (timestamp) {
    eefq <- eefq[, c("record_id", "early_executive_functions_questionnaire_timestamp", "eefq_ic", "eefq_fx", "eefq_wm", "eefq_rg")]
  } else if (!timestamp) {
    eefq <- eefq[, c("record_id", "eefq_ic", "eefq_fx", "eefq_wm", "eefq_rg")]
  }
  
  return(eefq)
  
}

#' @title Calculates ITN
#' @description calculates ITN based on poverty threshold for number of people in household and annual income
#' @param data the data frame to act on
#' @return A data frame with ITN
#' @export
calculate_itn <- function(data) {
  #creating poverty guideline data base (2024)
  poverty_guidelines <- data.frame(
    household_n = c(1,2,3,4,5,6,7,8),
    income_threshold = c(15060,20440,25820,31200,36580,41960,47340,52720)
  )
  data$annual_income <- as.numeric(data$annual_income)
  data <- dplyr::mutate(data, itn = NA)
  for (row in 1:nrow(data)) {
    total_household = (data$children_home[row] + data$adults_home[row])
    if (!is.na(total_household) & total_household <= 8) {
      index <- which(poverty_guidelines$household_n == total_household)
    } else if (is.na(total_household)) {
      index = NA
    } else {
      index = 8
    }
    
    if (!is.na(index)) {
      threshold = poverty_guidelines$income_threshold[index]
      data$itn[row] <- data$annual_income[row] / threshold
    } 
  }
  return(data)
}


#' @title Process 24m SDQ Survey Data
#' @description This function will download and return the sum scores for the SDQ subscales.
#' @param token Unique REDCap token ID
#' @param timestamp Boolean for whether to include survey timestamp
#' @return A data frame for the completed surveys
#' @export
get_orca_sdq <- function(token, timestamp=T) {
  sdq = get_orca_data(token, "strengths_and_difficulties_questionnaire")
  sdq$record_id = str_remove(sdq$record_id, "^0+")
  
  reverse <- c("sdq7", "sdq25", "sdq21", "sdq11", "sdq14")
  sdq[reverse] <- lapply(sdq[reverse], function(x) 3 - x)
  
  emo <- c("sdq3", 'sdq8', 'sdq13', 'sdq16', 'sdq24')
  cp <-  c("sdq5", 'sdq7', 'sdq12', 'sdq18', 'sdq22')
  hyp <-  c("sdq2", 'sdq10', 'sdq15', 'sdq21', 'sdq25')
  pp <-  c("sdq6", 'sdq11', 'sdq14', 'sdq19', 'sdq23')
  pro <-  c("sdq1", 'sdq4', 'sdq9', 'sdq17', 'sdq20')
  ext <- c(cp, hyp)
  int <- c(emo, pp)
  total = c(ext, int)
  sdq$sdq_emo <- rowSums(sdq[, emo], na.rm=T)
  sdq$sdq_cp <- rowSums(sdq[, cp], na.rm=T)
  sdq$sdq_hyp <- rowSums(sdq[, hyp], na.rm=T)
  sdq$sdq_pp <- rowSums(sdq[, pp], na.rm=T)
  sdq$sdq_pro <- rowSums(sdq[, pro], na.rm=T)
  sdq$sdq_ext <- rowSums(sdq[, ext], na.rm=T)
  sdq$sdq_int <- rowSums(sdq[, int], na.rm=T)
  sdq$sdq_total <- rowSums(sdq[, total], na.rm=T)
  
  if (timestamp) {
    sdq <- dplyr::select(sdq, record_id, strengths_and_difficulties_questionnaire_timestamp, sdq_emo:sdq_total)
  } else if (!timestamp) {
    sdq <- dplyr::select(sdq, record_id, sdq_emo:sdq_total)
  }
  
  return(sdq)
  
}

#' @title Process 24m MCHAT Survey Data
#' @description This function will download and return the sum score for the MCHAT.
#' @param token Unique REDCap token ID
#' @param timestamp Boolean for whether to include survey timestamp
#' @return A data frame for the completed surveys
#' @export
get_orca_mchat <- function(token, timestamp = T) {
  mchat = get_orca_data(token, "mchat")
  mchat$record_id = str_remove(mchat$record_id, "^0+")
  reverse <- c("mchat2", "mchat5", "mchat12")
  mchat[reverse] <- lapply(mchat[reverse], function(x) 1 - x)
  
  mchat[,5:24] <- lapply(mchat[,5:24], function(x) 1 - x)
  
  mchat$mchat_score = rowSums(mchat[,5:24])
  
  if (timestamp) {
    mchat <- dplyr::select(mchat, record_id, mchat_timestamp, mchat_score)
  } else if (!timestamp) {
    mchat <- dplyr::select(mchat, record_id, mchat_score)
  }
  
  return(mchat)
}

#' @title Process 24m BITSEA Survey Data
#' @description This function will download and return the sum scores for the MCHAT subscales and threshold information.
#' @param token Unique REDCap token ID
#' @param timestamp Boolean for whether to include survey timestamp
#' @return A data frame for the completed surveys
#' @export
get_orca_bitsea <- function(token, timestamp=T) {
  library(stringr)
  bitsea = get_orca_data(token, "bitsea")
  bitsea$record_id = str_remove(bitsea$record_id, "^0+")
  
  bitsea$autism_competence  = rowSums(bitsea[,c("bitsea_1", "bitsea_10", "bitsea_13", "bitsea_15",
                                                "bitsea_22", "bitsea_25", "bitsea_29", "bitsea_31")], na.rm=T)
  bitsea$autism_problems = rowSums(bitsea[,c("bitsea_9", "bitsea_14", "bitsea_21", "bitsea_35",
                                             "bitsea_36", "bitsea_37", "bitsea_38", "bitsea_39",
                                             "bitsea_40")], na.rm=T)
  bitsea$autism_total = bitsea$autism_problems - bitsea$autism_competence
  bitsea$problems = rowSums(bitsea[,c("bitsea_2", "bitsea_3", "bitsea_4", "bitsea_6", "bitsea_7", "bitsea_8",
                                      "bitsea_9", "bitsea_11", "bitsea_12", "bitsea_14", "bitsea_16", "bitsea_17",
                                      "bitsea_18", "bitsea_21", "bitsea_23", "bitsea_24", "bitsea_26", "bitsea_27",
                                      "bitsea_28", "bitsea_30", "bitsea_32", "bitsea_33", "bitsea_34", "bitsea_35",
                                      "bitsea_36", "bitsea_37", "bitsea_38", "bitsea_39", "bitsea_40",
                                      "bitsea_41", "bitsea_42")], na.rm=T)
  bitsea$competence = rowSums(bitsea[,c("bitsea_1", "bitsea_5", "bitsea_10", "bitsea_13", "bitsea_15",
                                        "bitsea_19", "bitsea_20", "bitsea_22", "bitsea_25", "bitsea_29", "bitsea_31")], na.rm=T)
  
  if (timestamp) {
    bitsea = bitsea[,c("record_id", "bitsea_timestamp","autism_competence", "autism_problems", "autism_total", "competence", "problems")]
  } else if (!timestamp) {
    bitsea = bitsea[,c("record_id","autism_competence", "autism_problems", "autism_total", "competence", "problems")]
  }
  bitsea$problem_thres = 0
  bitsea[bitsea$problems > 11,"problem_thres"] = 1
  bitsea$competence_thres = 0
  bitsea[bitsea$competence < 13,"competence_thres"] = 1
  bitsea$autism_total_thresh = 0
  bitsea[bitsea$autism_total > 7,"autism_total_thresh"] = 1
  bitsea$autism_comp_thresh = 0
  bitsea[bitsea$competence < 12,"autism_comp_thresh"] = 1
  bitsea$autism_prob_thresh = 0
  bitsea[bitsea$autism_problems > 4,"autism_prob_thresh"] = 1
  
  
  
  return(bitsea)
  
}

#' @title Process Stress Ratings Data
#' @description This function will download and return relevant fields from stress ratings, filtered by timepoint
#' @param token Unique REDCap token ID
#' @param timepoint redcap event name for timepoint
#' @param timestamp Boolean for whether to include survey timestamp
#' @return A data frame for the completed surveys
#' @export
get_orca_stress_ratings <- function(token, timepoint = 'orca_visit_1_arm_1', timestamp=T) {
  library(dplyr)
  sr <- get_orca_data(token, form='stress_ratings')
  
  sr <- sr %>%
    filter(redcap_event_name == timepoint)
  
  if (timestamp) {
    sr <- select(sr, record_id, stress_ratings_timestamp, ratings_stress_3, stress_rating)
  } else if (!timestamp) {
    sr <- select(sr, record_id, ratings_stress_3, stress_rating)
  }
  
  return(sr)
  
}

#' @title Process Family Routines Data
#' @description This function will download and return relevant fields from family routines, filtered by timepoint
#' @param token Unique REDCap token ID
#' @param timepoint redcap event name for timepoint
#' @param timestamp Boolean for whether to include survey timestamp
#' @return A data frame for the completed surveys
#' @export
get_orca_family_routines <- function(token, timepoint = 'orca_visit_1_arm_1', timestamp = T) {
  library(dplyr)
  
  fr <- get_orca_data(token, form='family_routines')
  
  fr <- fr %>%
    filter(redcap_event_name == timepoint) 
  
  
  if (timestamp) {
    fr <- select(fr, record_id, family_routines_timestamp, routines_bedtime:routines_examples)
  } else if (!timestamp) {
    fr <- select(fr, record_id, routines_bedtime:routines_examples)
  }
  
  return(fr)
  
}

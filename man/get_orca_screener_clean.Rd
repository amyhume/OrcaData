\name{get_orca_screener_clean}
\alias{get_orca_screener_clean}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Pulling cleaned screener responses 
}
\description{
this function pulls and cleans screener data for import to excel tracker 
}
\usage{
get_orca_screener_clean(token, min_date_time = "2022-01-01 00:00:00")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{token}{
Unique REDCap API token
}
  \item{min_date_time}{
The minimum timestamp to pull responses after
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (token, min_date_time = "2022-01-01 00:00:00") 
{
    library(tidyverse)
    screener = get_orca_data(token, "orca_screener_survey")
    screener = select(screener, -redcap_survey_identifier, -rec_unique_record_id, 
        -orca_screener_survey_complete, -record_id)
    screener = rename(screener, timestamp = orca_screener_survey_timestamp, 
        language = rec_language_preference, caregiver_name = rec_caregiver_name, 
        phone = rec_phone_number, texting_okay = rec_phone_number_text, 
        email = rec_caregiver_email, over_18 = rec_over_18, zipcode = rec_address_zipcode, 
        race_other = rec_race_other, children_amount = rec_child_number, 
        child_dob_1 = rec_babydob_one, child_dob_2 = rec_babydob_two, 
        child_dob_3 = rec_babydob_three, child_dob_4 = rec_babydob_four, 
        child_dob_5 = rec_babydob_five, multiple_child_dobs = rec_babydob_sixplus)
    screener$timestamp = strptime(screener$timestamp, format = "\%Y-\%m-\%d \%H:\%M:\%S")
    min_date_time = strptime(min_date_time, format = "\%Y-\%m-\%d \%H:\%M:\%S")
    screener = filter(screener, timestamp > min_date_time)
    screener = clean_race(screener)
    screener = clean_education(screener)
    screener$zipcode <- as.integer(screener$zipcode)
    zip_info <- zips()
    cities <- city_info()
    screener <- left_join(screener, zip_info, by = "zipcode")
    screener <- left_join(screener, cities, by = "location")
    screener <- mutate(screener, rural = ifelse(is.na(population) & 
        !is.na(location), "Y", ifelse(population >= 50000, "N", 
        ifelse(population < 50000, "Y", "CHECK"))))
    screener <- mutate(screener, non_white = ifelse(race == "White", 
        "N", "Y"))
    screener <- mutate(screener, low_ses = ifelse(education == 
        "Graduate degree", "N", ifelse(education == "College degree", 
        "N", "Y")))
    screener$texting_okay <- gsub(1, "Yes", screener$texting_okay)
    screener$texting_okay <- gsub(0, "No", screener$texting_okay)
    screener$over_18 <- gsub(1, "Yes", screener$over_18)
    screener$over_18 <- gsub(0, "No", screener$over_18)
    screener$child_name <- ""
    screener$number_valid <- ""
    col_order <- c("caregiver_name", "child_name", "language", 
        "phone", "number_valid", "texting_okay", "email", "over_18", 
        "zipcode", "race", "race_other", "education", "children_amount", 
        "child_dob_1", "child_dob_2", "child_dob_3", "child_dob_4", 
        "child_dob_5", "multiple_child_dobs", "timestamp", "timezone", 
        "location", "rural", "non_white", "low_ses")
    screener <- screener[, col_order]
    screener <- dplyr::arrange(screener, timestamp)
    return(screener)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.

\name{get_orca_data}
\alias{get_orca_data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Pulling ORCA Data
}
\description{
This function retrieves data from a REDCap project using the API.
}
\usage{
get_orca_data(token = token, form = form, raw_v_label = "raw", form_complete = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{token}{
The API token for the project
}
  \item{form}{
The name of the REDCap form to retrieve data from
}
  \item{raw_v_label}{
The label for raw data fields
}
  \item{form_complete}{
Indicating whether you want to return all responses or just ones marked as complete (default is all)
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
function (token = token, form = form, raw_v_label = "raw", form_complete = F) 
{
    if (form_complete) {
        record_filter = paste("[", form, "_complete]=2", sep = "")
    }
    else {
        record_filter = ""
    }
    formData <- list(uri = url, token = token, content = "record", 
        format = "csv", type = "flat", csvDelimiter = "", `fields[0]` = "record_id", 
        `forms[0]` = form, rawOrLabel = raw_v_label, rawOrLabelHeaders = raw_v_label, 
        exportCheckboxLabel = "false", exportSurveyFields = "true", 
        exportDataAccessGroups = "false", returnFormat = "csv", 
        filterLogic = record_filter)
    response <- httr::POST(url, body = formData, encode = "form")
    df <- httr::content(response)
    df[df == -888] = NA
    df[df == 888] = NA
    df[df == 8888] = NA
    df[df == -999] = NA
    df[df == 999] = NA
    df[df == 9999] = NA
    return(df)
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

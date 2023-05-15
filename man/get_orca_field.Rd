\name{get_orca_field}
\alias{get_orca_field}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Pulling ORCA Field
}
\description{
PULLS RECORD ID AND INDIVIDUAL FIELD
}
\usage{
get_orca_field(token = token, field = field, raw_v_label = "raw")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{token}{
Unique REDCap token ID
}
  \item{field}{
Name of the specific field to be downloaded
}
  \item{raw_v_label}{
Whether raw data or labels are requested
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
function (token = token, field = field, raw_v_label = "raw") 
{
    formData <- list(uri = url, token = token, content = "record", 
        format = "csv", type = "flat", csvDelimiter = "", `fields[0]` = "record_id", 
        `fields[1]` = field, rawOrLabel = raw_v_label, rawOrLabelHeaders = raw_v_label, 
        exportCheckboxLabel = "false", exportSurveyFields = "false", 
        exportDataAccessGroups = "false", returnFormat = "csv")
    response <- httr::POST(url, body = formData, encode = "form")
    df <- httr::content(response)
    df[df == -888] = NA
    df[df == 888] = NA
    df[df == 8888] = NA
    df[df == -999] = NA
    df[df == 999] = NA
    df[df == 9999] = NA
    df <- dplyr::select(df, record_id, field)
    df <- df[!is.na(df[[field]]), ]
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

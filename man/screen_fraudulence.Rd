\name{screen_fraudulence}
\alias{screen_fraudulence}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Screens screener export
}
\description{
Checks for: duplicate contact info, under 18, numeric names, NA names/emails, babies above age threshold, names all lowercase
}
\usage{
screen_fraudulence(data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
The data frame you wish to act on
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
function (data) 
{
    library(dplyr)
    data <- data \%>\% flag_ineligible_age(threshold_date = Sys.Date() - 
        380) \%>\% flag_duplicate_contacts() \%>\% flag_lowercase_names() \%>\% 
        flag_numeric_names()
    ineligible_ages <- data \%>\% filter(age_ineligible == 1)
    duplicate_contacts <- data \%>\% filter(duplicate_email == 
        1 | duplicate_phone == 1)
    data <- data \%>\% filter(!is.na(caregiver_name) & !is.na(email) & 
        over_18 == "Yes" & num_name_flag == 0 & age_ineligible == 
        0)
    data <- data \%>\% filter(duplicate_email == 0 & duplicate_phone == 
        0)
    data <- data \%>\% select(-age_ineligible, -duplicate_email, 
        -duplicate_phone, -num_name_flag)
    return(list(data = data, ineligible_ages = ineligible_ages, 
        duplicate_contacts = duplicate_contacts))
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

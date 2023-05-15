\name{flag_ineligible_age}
\alias{flag_ineligible_age}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
INELIGIBLE AGES
}
\description{
Flags any child ages older than 380 days from today
}
\usage{
flag_ineligible_age(data, threshold_date = Sys.Date() - 380)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
The data frame you wish to act on
}
  \item{threshold_date}{
The date threshold you wish to compare - default is 380 days prior to today
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
function (data, threshold_date = Sys.Date() - 380) 
{
    library(dplyr)
    cols <- c("child_dob_1", "child_dob_2", "child_dob_3", "child_dob_4", 
        "child_dob_5")
    for (p in 1:nrow(data)) {
        date_vec <- na.omit(as.numeric(data[p, cols]))
        date_vec <- as.Date(date_vec, origin = "1970-01-01")
        data[p, "age_ineligible"] <- ifelse(all(date_vec <= threshold_date), 
            1, 0)
    }
    return(data)
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

\name{get_orca_dq}
\alias{get_orca_dq}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Pulling ORCA Data quality
}
\description{
RETURNS DATA QUALITY SCORES FOR EACH TASK SPECIFIED
}
\usage{
get_orca_dq(token, tasks = c("cecile", "maap", "memory", "vpc"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{token}{
The API token for the project
}
  \item{tasks}{
The tasks you wish to export data quality for - if multiple, place in vector
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
function (token, tasks = c("cecile", "maap", "memory", "vpc")) 
{
    dq <- get_orca_data(token, form = "data_quality")
    notes <- dplyr::select(dq, record_id, cecile_dq_notes, memory_dq_notes, 
        vpc_dq_notes, maap_dq_notes)
    notes <- dplyr::rename(notes, cecile = cecile_dq_notes, memory = memory_dq_notes, 
        vpc = vpc_dq_notes, maap = maap_dq_notes)
    dq <- dplyr::rename(dq, cecile = cecile_dq, maap = maap_dq, 
        memory = memory_dq, vpc = vpc_dq)
    dq <- dplyr::select(dq, record_id, tasks)
    notes <- dplyr::select(notes, record_id, tasks)
    dq <- dplyr::left_join(dq, notes, by = "record_id")
    return(dq)
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

\name{flag_duplicate_contacts}
\alias{flag_duplicate_contacts}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Duplicate contacts
}
\description{
Flags any participant with a duplicate phone or email
}
\usage{
flag_duplicate_contacts(data)
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
    data <- data \%>\% group_by(email) \%>\% mutate(duplicate_email = ifelse(!is.na(email) & 
        n() > 1, 1, 0)) \%>\% ungroup() \%>\% group_by(phone) \%>\% 
        mutate(duplicate_phone = ifelse(!is.na(phone) & n() > 
            1, 1, 0)) \%>\% ungroup()
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

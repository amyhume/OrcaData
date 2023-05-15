\name{get_orca_social_support}
\alias{get_orca_social_support}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Process Social Support data
}
\description{
This function will download and return the total summed scores 
}
\usage{
get_orca_social_support(token)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{token}{
Unique REDCap token ID
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
function (token) 
{
    ss <- get_orca_data(token, "social_support", form_complete = T)
    ss_columns <- dplyr::select(ss, support3:support10)
    ss$social_support <- rowSums(ss_columns)
    ss <- dplyr::select(ss, record_id, social_support)
    return(ss)
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

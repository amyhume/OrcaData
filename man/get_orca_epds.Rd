\name{get_orca_epds}
\alias{get_orca_epds}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Process EPDS postpartum depression data
}
\description{
This function will download and return the totalsummed scores for the EPDS. Only total scores, and not cutoff values, are returned.
}
\usage{
get_orca_epds(token)
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
    epds = get_orca_data(token, "epds", form_complete = T)
    epds$epds_1_v2 = 3 - epds$epds_1_v2
    epds$epds_2_v2 = 3 - epds$epds_2_v2
    epds$epds_all = epds$epds_1_v2 + epds$epds_2_v2 + epds$epds_3_v2 + 
        epds$epds_4_v2 + epds$epds_5_v2 + epds$epds_6_v2 + epds$epds_7_v2 + 
        epds$epds_8_v2 + epds$epds_9_v2 + epds$epds_10_v2
    epds$epds_dep = epds$epds_1_v2 + epds$epds_2_v2 + epds$epds_3_v2 + 
        epds$epds_7_v2 + epds$epds_8_v2 + epds$epds_9_v2 + epds$epds_10_v2
    epds$epds_anx = epds$epds_4_v2 + epds$epds_5_v2 + epds$epds_6_v2
    epds = epds[, c("record_id", "epds_all", "epds_dep", "epds_anx")]
    return(epds)
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

\name{get_orca_md}
\alias{get_orca_md}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Process Material Deprivation data
}
\description{
This function will download and return the totalsummed scores 
}
\usage{
get_orca_md(token)
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
    md <- get_orca_data(token, "material_deprivation", form_complete = T)
    md_columns <- dplyr::select(md, md_snap:md_overtime)
    md$material_deprivation <- rowSums(md_columns)
    md <- dplyr::select(md, record_id, md_score)
    return(md)
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

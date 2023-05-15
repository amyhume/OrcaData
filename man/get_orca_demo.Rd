\name{get_orca_demo}
\alias{get_orca_demo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Pulls sociodemographic cleaned responses 
}
\description{
This cleans and pulls demographic information
}
\usage{
get_orca_demo(token)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{token}{
The API token for the project
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
    demo <- get_orca_data(token, form = "sociodemographic", form_complete = T)
    demo <- dplyr::select(demo, record_id, sociodemographic_timestamp, 
        child_dob:socio_comments)
    demo <- replace_multiple(demo, "caregiver_ed", old_values = c(1, 
        2, 3, 4, 5), new_values = c("less than hs", "high school degree/ged", 
        "partial college", "bachelor's degree", "graduate degree"))
    demo <- replace_multiple(demo, "marital_status", old_values = c(1, 
        2, 3, 4, 5, 6, 7), new_values = c("married", "single living together", 
        "single never married", "separated", "divorced", "widowed", 
        "other"))
    return(demo)
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

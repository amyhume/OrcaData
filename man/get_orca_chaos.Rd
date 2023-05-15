\name{get_orca_chaos}
\alias{get_orca_chaos}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Process CHAOS data
}
\description{
This function will download and compute total scores for the CHAOS scale
}
\usage{
get_orca_chaos(token)
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
    chaos = get_orca_data(token, "chaos", form_complete = T)
    pos_items <- c("chaos_commotion", "chaos_findthings", "chaos_ontop", 
        "chaos_interrupt", "chaos_relax", "chaos_calm")
    chaos <- dplyr::mutate_at(chaos, dplyr::vars(all_of(pos_items)), 
        ~abs(5 - .))
    chaos$total_score = rowMeans(chaos[, c("chaos_commotion", 
        "chaos_findthings", "chaos_rushed", "chaos_ontop", "chaos_late", 
        "chaos_zoo", "chaos_interrupt", "chaos_fuss", "chaos_plans", 
        "chaos_think", "chaos_arguement", "chaos_relax", "chaos_telephone", 
        "chaos_calm")], na.rm = T)
    chaos = chaos[, c("record_id", "chaos_timestamp", "total_score")]
    return(chaos)
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

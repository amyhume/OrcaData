\name{replace_multiple}
\alias{replace_multiple}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Replacing Multiple Values Function
}
\description{
ALLOWS DATA TO BE REPLACED IN A COLUMN
}
\usage{
replace_multiple(data, column_name, old_values, new_values)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
The dataframe
}
  \item{column_name}{
The column you wish to work on
}
  \item{old_values}{
Vector of string variables you wish to replace
}
  \item{new_values}{
Vector of string variables you wish to replace the old ones with 
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
function (data, column_name, old_values, new_values) 
{
    data[[column_name]] <- new_values[match(data[[column_name]], 
        old_values)]
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

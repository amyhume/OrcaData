\name{clean_education}
\alias{clean_education}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Cleaning screener education responses
}
\description{
This function cleans education responses 
}
\usage{
clean_education(data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
data frame to act upon
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
    data$rec_education_level___1 <- gsub("1", "High School or Less", 
        data$rec_education_level___1)
    data$rec_education_level___2 <- gsub("1", "Less than College", 
        data$rec_education_level___2)
    data$rec_education_level___3 <- gsub("1", "College Degree", 
        data$rec_education_level___3)
    data$rec_education_level___4 <- gsub("1", "Graduate Degree", 
        data$rec_education_level___4)
    data <- unite(data, "education", rec_education_level___1:rec_education_level___4)
    data$education <- gsub("0", "", data$education)
    data$education <- gsub("_", "", data$education)
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

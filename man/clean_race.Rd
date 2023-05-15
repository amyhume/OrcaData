\name{clean_race}
\alias{clean_race}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Cleaning screener race responses
}
\description{
This function cleans race responses 
}
\usage{
clean_race(data)
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
    data$rec___1 <- gsub("1", "American Indian/Alaska Native", 
        data$rec___1)
    data$rec___2 <- gsub("1", "Asian", data$rec___2)
    data$rec___3 <- gsub("1", "Black/African American/African", 
        data$rec___3)
    data$rec___4 <- gsub("1", "Hispanic/Latino", data$rec___4)
    data$rec___5 <- gsub("1", "Middle Eastern/North African", 
        data$rec___5)
    data$rec___6 <- gsub("1", "Native Hawaiian", data$rec___6)
    data$rec___7 <- gsub("1", "White", data$rec___7)
    data$rec___8 <- gsub("1", "None fully describe", data$rec___8)
    data$rec___9 <- gsub("1", "Prefer not to answer", data$rec___9)
    data <- unite(data, "race", rec___1:rec___9)
    data$race <- gsub("0", "", data$race)
    data$race <- gsub("_", "", data$race)
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

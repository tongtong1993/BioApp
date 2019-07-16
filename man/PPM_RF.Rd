\name{PPM_RF}
\alias{PPM_RF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{The Prediction Precicsion Model (PPM) evaluate biomarker's applicability.
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PPM_RF(selected_features)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{selected_features}{The column index of features input to the PPM.
%%     ~~Describe \code{selected_features} here~~
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
## Set output Rdata file.
output_path = "./output.rda"
## The function will relocate the label to the first column.
preprocess("classification", iris, c(5))
[1] "1 :  Species"
[1] "2 :  Sepal.Length"
[1] "3 :  Sepal.Width"
[1] "4 :  Petal.Length"
[1] "5 :  Petal.Width"

CPM_RF(c(2,3))
PPM_RF(c(4,5))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

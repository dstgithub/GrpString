\name{GrpString-package}
\alias{GrpString-package}
\alias{GrpString}
\docType{package}
\title{
Patterns and Statistical Differences between Two Groups of Strings
}
\description{
Methods include converting series of event names to strings, discovering commen patterns
in a group of strings, discovering "unique" patterns when comparing two groups of strings as well
as the number and starting position of each "unique" pattern in each string, finding the transition
information, and statistically comparing the difference between two groups of strings.

Event names can be any action names such as events in log files or areas of insterst (AOIs) in eye tracking research.
}
\details{
\tabular{ll}{
Package: \tab GrpString\cr
Type: \tab Package\cr
Version: \tab 0.1.0\cr
Date: \tab 2016-12-28\cr
License: \tab GPL-2\cr
}
    Some functions have two types, one returning a data frame or a vector and the other exporting
one or more than one .txt file to the current directory. The former is a simple version of the functions, 
while the latter can be considered as a generalized or complex version of the former one. This is 
because some data sets are large (e.g., many rows or columns), or it helps the users to view and manage 
results when more than one data set is exported. These function pairs include EveStr - EveString,
CommonPatt - CommonPattern, and PatternInfo - UniPatterns.

    In addition, to save the users' effort, the function EveString uses an input file (which can be a .txt or .csv file) instead of a data frame. This is because the input data is more convenient to store in a .txt or .csv file than in a data frame. We suggest the users to copy the relevent input files (including eve1d.txt and eve1d.csv) to a different directory, because the function exports files to the same directory where the input files locate. 
}
\author{
Hui (Tom) Tang,
        Norbert J. Pienta

Maintainer: Hui (Tom) Tang <htang2013@gmail.com>
}
\examples{
# Discover common patterns in a group of strings
strs.vec <- c("ABCDdefABCDa", "def123DC", "123aABCD", "ACD13", "AC1ABC", "3123fe")
CommonPatt(strs.vec, low = 30)
}

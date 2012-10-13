\name{trade.depth}
\alias{trade.depth}
\title{Trades when depth is thin in one side of the market.}
\usage{trade.depth(num=100)}
\description{Trades when depth is thin in one side of the market.
Generates quote events according to poisson, and then analyses the book
to determine when to trade. Trades are taken when the depth on one side of the book
is larger than the other, and hence more likely to move. This strategy can be elaborated on to look at futures prices vs spot
or by using a bayesian updating probability of when the market will move considering multiple quote levels}
\arguments{\item{num}{The number of iterations to model}}

\name{trade.mm}
\alias{trade.mm}
\title{Simple market making strategy.}
\usage{trade.mm(num=100)}
\description{Simple market making strategy.
Whenever the spread is wider than a target spread this strategy provides liquidity.
This strategy attempts to keep risk (the position size) close to zero.}
\arguments{\item{num}{The number of iterations to model}}

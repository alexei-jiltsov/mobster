\name{init.book}
\alias{init.book}
\title{reset the book...}
\usage{init.book(n=10000, ask=100, bid=99.99)}
\description{reset the book
clears all the values in the historical order book, 
clears the execution history, and resets the current orderbook. 
This should be called everytime before starting a new session. The book is started at 100.00/99.99 by
default.}
\arguments{\item{n}{int. The number of rows to initialise in the book}
\item{ask}{The starting ask price}
\item{bid}{The starting bid price}}

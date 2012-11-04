\name{cancel}
\alias{cancel}
\title{Cancel an order/quote...}
\usage{cancel(id, qty=0, time=0, externalId=0)}
\description{Cancel an order/quote
cancels an order in the orderbook. The order is removed from the book.}
\arguments{\item{id}{the orderid to cancel.}
\item{qty}{the qty of the order to cancel.}
\item{time}{the time the order was cancelled (when replaying historical data).}
\item{externalId}{the id of the original order (use when replaying historical data).}}

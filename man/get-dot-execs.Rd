\name{get.execs}
\alias{get.execs}
\title{Get the execution history.}
\usage{get.execs(n)}
\description{Get the execution history.
Both sides of a trade are recorded in the execution history for analysis. The data.table returned includes the
orderid of the order, the matching order id, the qty filled, the price and the current mid price in the book (bid+ask)/2}
\value{data.table with columns: execid, orderid, matchorderid, side, trader, fillqty, price, midp (midprice at time of execution)}
\arguments{\item{n}{the number of rows to retrieve}}

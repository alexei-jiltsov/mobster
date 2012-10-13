\name{limit}
\alias{limit}
\title{Add limit order...}
\usage{limit(rsym, rtrader, rside, rprice, rqty, rtradetype)}
\description{Add limit order
adds a limit order to the book. The order will match if it crosses. Types include FAK (fill and kill) or 
GTC (good till cancel). GTC types will remain in the orderbook until filled or cancelled.}
\arguments{\item{rsym}{string. The symbol of the instrument being traded}
\item{rtrader}{string. The trader (or strategy name) making the trade}
\item{rside}{int. The side of the trade. buy=0, sell=1}
\item{rqty}{int. The qty to be traded}
\item{rprice}{double. The limit price}
\item{rtradetype}{string. GTC or FAK}}

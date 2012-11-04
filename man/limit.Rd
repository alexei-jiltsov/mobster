\name{limit}
\alias{limit}
\title{Add limit order...}
\usage{limit(sym, trader, side, price, qty, tradetype, time=0, externalId=0)}
\description{Add limit order
adds a limit order to the book. The order will match if it crosses. Types include FAK (fill and kill) or 
GTC (good till cancel). GTC types will remain in the orderbook until filled or cancelled.}
\arguments{\item{sym}{string. The symbol of the instrument being traded}
\item{trader}{string. The trader (or strategy name) making the trade}
\item{side}{int. The side of the trade. buy=0, sell=1}
\item{qty}{int. The qty to be traded}
\item{price}{double. The limit price}
\item{tradetype}{string. GTC or FAK}
\item{time}{int. The time a trade was done (ms)}
\item{externalId}{int. The trade id. Defaulted to zero, but passed so that historical replay from total itch data can be used to cancel orders correctly.}}

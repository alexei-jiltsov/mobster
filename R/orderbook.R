
#' onload
# onload function
#' @param libname the library
#' @param pkgname the pkgname
#' @export
.onLoad <- function(libname, pkgname) {
	library.dynam("mobster", pkgname, libname)
	orders <<- data.table(id=1:1000000, sym=rep("sym",1000000), trader=rep("t",1000000), price=rep(0,1000000), side=rep(0,1000000), qty=rep(0,1000000), tradeype=rep("tt",1000000), time=rep(0,1000000), externalId=rep(0,1000000), key="id")
	# default initialisation
	.C("init", as.integer(100000), as.integer(100*100),as.integer(99.99*100), PACKAGE='mobster')
}


#' Add limit order
#' adds a limit order to the book. The order will match if it crosses. Types include FAK (fill and kill) or 
#' GTC (good till cancel). GTC types will remain in the orderbook until filled or cancelled.
#' @param sym string. The symbol of the instrument being traded
#' @param trader string. The trader (or strategy name) making the trade
#' @param side int. The side of the trade. buy=0, sell=1
#' @param qty int. The qty to be traded
#' @param price double. The limit price
#' @param tradetype string. GTC or FAK
#' @param time int. The time a trade was done (ms)
#' @param externalId int. The trade id. Defaulted to zero, but passed so that historical replay from total itch data can be used to cancel orders correctly.
#' @export
limit <- function(sym, trader, side, price, qty, tradetype, time=0, externalId = 0) {
	if(qty<=0) {
		return(0)
	}
	result <<-.C("limit", as.character(sym), as.character(trader), as.integer(side), as.double(price), as.integer(qty), as.integer(0), as.character(tradetype), as.integer(time), as.integer(externalId), PACKAGE='mobster')
	lastOrderId <- result[[6]]
	
	set(orders, as.integer(lastOrderId), as.integer(2), result[[1]]) #sym 
	set(orders, as.integer(lastOrderId), as.integer(3), result[[2]]) #trader
	set(orders, as.integer(lastOrderId), as.integer(4), result[[4]]) #price
	set(orders, as.integer(lastOrderId), as.integer(5), result[[3]]) #side
	set(orders, as.integer(lastOrderId), as.integer(6), result[[5]]) #qty
	set(orders, as.integer(lastOrderId), as.integer(7), result[[7]]) #type
	set(orders, as.integer(lastOrderId), as.integer(8), result[[8]]) #time
	set(orders, as.integer(lastOrderId), as.integer(9), result[[9]]) #externalId
	
	return(lastOrderId)
}


#' Place a market order.
#' A market order will scan the orderbook until the amount required has been matched, or there is no
#' more depth in the orderbook. 
#' @param sym string. The symbol of the instrument being traded
#' @param trader string. The trader (or strategy name) making the trade
#' @param side int. The side of the trade. buy=0, sell=1
#' @param price double. not used
#' @param qty int. The qty to be traded
#' @export
market <- function(sym, trader, side, price, qty, time=0, externalId = 0) {

	if(qty<=0) {
		return(0)
	}

	result <- .C("market",  as.character(sym), as.character(trader), as.integer(side), as.double(price), as.integer(qty), as.integer(0), as.numeric(time), as.integer(externalId), PACKAGE='mobster')

	lastOrderId <- result[[6]]
	if(result[[6]] >0) {
		set(orders, as.integer(lastOrderId), as.integer(2), result[[1]]) #sym 
		set(orders, as.integer(lastOrderId), as.integer(3), result[[2]]) #trader
		set(orders, as.integer(lastOrderId), as.integer(4), result[[4]]) #price
		set(orders, as.integer(lastOrderId), as.integer(5), result[[3]]) #side
		set(orders, as.integer(lastOrderId), as.integer(6), result[[5]]) #qty
		set(orders, as.integer(lastOrderId), as.integer(7), "FAK") #type
		set(orders, as.integer(lastOrderId), as.integer(8), result[[7]]) #time
	}
	
	return(lastOrderId)
}


#' Get the historical book.
#' Each quote/order is recorded in a historical book for analysis.
#' The historical book records 10 levels of bid/ask prices, qty,
#' the total traded volume and total number of trades
#' @param n the number of orders to retrieve 
#' @export
get.hob <- function(n){

	totalTraded <- rep(1:n)
	totalTrades <- rep(1:n)
	time <- rep(1:n)
	ask0 <- rep(1:n)
	askqty0 <- rep(1:n)
	ask1 <- rep(1:n)
	askqty1 <- rep(1:n)
	ask2 <- rep(1:n)
	askqty2 <- rep(1:n)
	ask3 <- rep(1:n)
	askqty3 <- rep(1:n)
	ask4 <- rep(1:n)
	askqty4 <- rep(1:n)
	ask5 <- rep(1:n)
	askqty5 <- rep(1:n)
	ask6 <- rep(1:n)
	askqty6 <- rep(1:n)
	ask7 <- rep(1:n)
	askqty7 <- rep(1:n)
	ask8 <- rep(1:n)
	askqty8 <- rep(1:n)
	ask9 <- rep(1:n)
	askqty9 <- rep(1:n)
	
	bid0 <- rep(1:n)
	bidqty0 <- rep(1:n)
	bid1 <- rep(1:n)
	bidqty1 <- rep(1:n)
	bid2 <- rep(1:n)
	bidqty2 <- rep(1:n)
	bid3 <- rep(1:n)
	bidqty3 <- rep(1:n)
	bid4 <- rep(1:n)
	bidqty4 <- rep(1:n)
	bid5 <- rep(1:n)
	bidqty5 <- rep(1:n)
	bid6 <- rep(1:n)
	bidqty6 <- rep(1:n)
	bid7 <- rep(1:n)
	bidqty7 <- rep(1:n)
	bid8 <- rep(1:n)
	bidqty8 <- rep(1:n)
	bid9 <- rep(1:n)
	bidqty9 <- rep(1:n)
	
	
	obRes <<- .C("get_hob", as.integer(n), as.integer(totalTraded), as.integer(totalTrades), as.integer(ask0), as.integer(askqty0), as.integer(ask1), as.integer(askqty1), as.integer(ask2), as.integer(askqty2), as.integer(ask3), as.integer(askqty3), as.integer(ask4), as.integer(askqty4), as.integer(ask5), as.integer(askqty5), as.integer(ask6), as.integer(askqty6), as.integer(ask7), as.integer(askqty7), as.integer(ask8), as.integer(askqty8), as.integer(ask9), as.integer(askqty9), 
	as.integer(bid0), as.integer(bidqty0), as.integer(bid1), as.integer(bidqty1), as.integer(bid2), as.integer(bidqty2), as.integer(bid3), as.integer(bidqty3), as.integer(bid4), as.integer(bidqty4), as.integer(bid5), as.integer(bidqty5), as.integer(bid6), as.integer(bidqty6), as.integer(bid7), as.integer(bidqty7), as.integer(bid8), as.integer(bidqty8), as.integer(bid9), as.integer(bidqty9), as.integer(time) 
	, PACKAGE='mobster')

	ob <- data.table(time=obRes[[44]], ask0=obRes[[4]], askqty0=obRes[[5]], ask1=obRes[[6]], askqty1=obRes[[7]],ask2=obRes[[8]], askqty2=obRes[[9]],ask3=obRes[[10]], askqty3=obRes[[11]],ask4=obRes[[12]], askqty4=obRes[[13]],ask5=obRes[[14]], askqty5=obRes[[15]],ask6=obRes[[16]], askqty6=obRes[[17]],ask7=obRes[[18]], askqty7=obRes[[19]],ask8=obRes[[20]], askqty8=obRes[[21]],ask9=obRes[[22]], askqty9=obRes[[23]],
		             bid0=obRes[[24]], bidqty0=obRes[[25]], bid1=obRes[[26]], bidqty1=obRes[[27]],bid2=obRes[[28]], bidqty2=obRes[[29]],bid3=obRes[[30]], bidqty3=obRes[[31]],bid4=obRes[[32]], bidqty4=obRes[[33]],bid5=obRes[[34]], bidqty5=obRes[[35]],bid6=obRes[[36]], bidqty6=obRes[[37]],bid7=obRes[[38]], bidqty7=obRes[[39]],bid8=obRes[[40]], bidqty8=obRes[[41]],bid9=obRes[[42]], bidqty9=obRes[[43]],
		             totalTraded=obRes[[2]],totalTrades=obRes[[3]]
		            )
		            
	return(ob)
}


#' Get the orderbook.
#' Gets a snapshot of the current orderbook. The order book has 10 ask and 10 bid levels.
#' By adding limit orders/quotes and market orders the dynamics of an exhange can be modelled.
#' By using a file of real exchange data trading strategies can be plugged in to model market microstructure.  
#' @export
get.ob <- function(){
	n=20
	p <- rep(0,n)
	q <- rep(0,n)
	s <- c(rep(1,n/2),rep(0,n/2))
	
	obRes <- .C("get_ob", as.double(p), as.integer(q), as.integer(s), PACKAGE='mobster')
	
	ob <- data.frame(price=obRes[[1]], qty=obRes[[2]], side=obRes[[3]])
	ob$price <- ob$price/100	
	return(ob)
}

#' Get the total filled qty for an order.
#' @param id the orderid to get the filled qty for
#' @export
get.filled.qty <- function(id, externalId=0) {
	qty <- .C("getFilledQty", as.integer(id), as.integer(externalId), PACKAGE='mobster')[[1]]
	return(qty)
}


#' Cancel an order/quote
#' cancels an order in the orderbook. The order is removed from the book. 
#' @param id the orderid to cancel. 
#' @export
cancel <- function(id, qty=0, time=0, externalId=0) {
	id <- .C("cancel", as.integer(id), as.integer(qty), as.integer(time), as.integer(externalId), PACKAGE='mobster')[[1]]
	return(id)
}


#replace <- function(ordid, sym, trader, side, price, qty) {
#	id <- .C("replace",  as.integer(ordid), as.character(sym), as.character(trader), as.integer(side), as.double(price), as.integer(qty), as.integer(0), PACKAGE='mobster')[[7]]
#	return(id)
#}


#' reset the book
#' clears all the values in the historical order book, 
#' clears the execution history, and resets the current orderbook. 
#' This should be called everytime before starting a new session. The book is started at 100.00/99.99 by
#' default.
#' @param n int. The number of rows to initialise in the book
#' @param ask The starting ask price
#' @param bid The starting bid price
#' @export
init.book <- function(n=10000, ask=100.00, bid=99.99) {
	orders <<- data.table(id=1:1000000, sym=rep("sym",1000000), trader=rep("t",1000000), price=rep(0,1000000), side=rep(0,1000000), qty=rep(0,1000000), tradeype=rep("tt",1000000), key="id")
	# prices are required to be shorts for arrayindexing in c code
	.C("init", as.integer(n), as.integer(ask*100),as.integer(bid*100), PACKAGE='mobster')
}



#' Get the execution history.
#' Both sides of a trade are recorded in the execution history for analysis. The data.table returned includes the
#' orderid of the order, the matching order id, the qty filled, the price and the current mid price in the book (bid+ask)/2
#' @param n the number of rows to retrieve
#' @return data.table with columns: execid, orderid, matchorderid, side, trader, fillqty, price, midp (midprice at time of execution)
#' @export
get.execs <- function(n) {
	
	exec_id <- rep(1:n)
	exec_time <- rep(1:n)
	exec_oid <- rep(1:n)
	exec_mid <- rep(1:n)
	exec_eoid <- rep(1:n)
	exec_emid <- rep(1:n)
	exec_side <- rep(1:n)
	exec_trader <- rep(1:n)
	exec_fillqty <- rep(1:n)
	exec_price <- rep(1:n)
	exec_midp <- rep(1:n)
	
	execRes <- .C("get_execs", as.integer(n), as.integer(exec_id), as.integer(exec_oid),as.integer(exec_mid),as.integer(exec_side),as.character(exec_trader),as.integer(exec_fillqty),as.integer(exec_price), as.double(exec_midp), as.integer(exec_time), as.integer(exec_eoid),as.integer(exec_emid), PACKAGE='mobster') 
	
	execRes[[2]] <- execRes[[2]][2:n]
	execRes[[3]] <- execRes[[3]][2:n]
	execRes[[4]] <- execRes[[4]][2:n]
	execRes[[5]] <- execRes[[5]][2:n]
	execRes[[6]] <- execRes[[6]][2:n]
	execRes[[7]] <- execRes[[7]][2:n]
	execRes[[8]] <- execRes[[8]][2:n]
	execRes[[9]] <- execRes[[9]][2:n]
	execRes[[10]] <- execRes[[10]][2:n]
	execRes[[11]] <- execRes[[11]][2:n]
	execRes[[12]] <- execRes[[12]][2:n]
	
	exec <- data.table(id=execRes[[2]], time=execRes[[10]], oid=execRes[[3]], mid=execRes[[4]], side=execRes[[5]], trader=execRes[[6]], fillqty=execRes[[7]],price=execRes[[8]],midp=execRes[[9]], eoid=execRes[[11]], emid=execRes[[12]])
	
	return(exec)
}

#.adjustWidth <- function(...){
#       options(width=Sys.getenv("COLUMNS"))
#       TRUE
#}
 
#.adjustWidthCallBack <- addTaskCallback(.adjustWidth)


#' Simple market making strategy.
#' Whenever the spread is wider than a target spread this strategy provides liquidity.
#' This strategy attempts to keep risk (the position size) close to zero. 
#' @param num The number of iterations to model  
#' @export
trade.historical.spy <- function(file='/Users/tomappleton/Downloads/20120928_SPY-1.csv') {
	
	
	orders <<- data.table(id=1:1000000, sym=rep("sym",1000000), trader=rep("t",1000000), price=rep(0,1000000), side=rep(0,1000000), qty=rep(0,1000000), tradeype=rep("tt",1000000), time=rep(0,1000000), externalId=rep(0,1000000), key="id")

	# init the book
	

	# strategies current position after filling orders
	position <<- 0
	
	# current sell quote id	
	sell.quote.id <<- NULL
	
	# current buy quote id
	buy.quote.id  <<- NULL

	# start main loop
	file='/Users/tomappleton/Downloads/20120928_SPY.csv'
	df <- read.csv(file)
	df <- df[1:80000,]
	
	min <- min(df$Price)
	max <- max(df$Price)
	df <- df[which(df$Price!=max),]
	max <- max(df$Price)
	df <- df[which(df$Price!=max),]
	max <- max(df$Price)
	df <- df[which(df$Price!=max),]
	max <- max(df$Price)
	df <- df[which(df$Price!=max),]
	max <- max(df$Price)
	df <- df[which(df$Price!=max),]
	max <- max(df$Price)
	df <- df[which(df$Price!=max),]
	
	result <- .C("init", as.integer(1:1000000), as.integer(max), as.integer(min), PACKAGE='mobster')
	count <<- 1
	apply(dfs, 1, trade)
	
	
}

# num = 390000
# h <- get.hob(num)
# ylim <- c(min(h[2:num]$ask0), max(h[2:num]$ask0))
# plot(h[2:num]$ask0, type='s', col='red', ylim=ylim)
# par(new=TRUE)
# plot(h[2:num]$bid0, type='s', col='green', ylim=ylim)

 # plot(format(as.POSIXlt(df$Time/1000, origin="2012-09-28"), format="%H:%M:%OS3"), get.hob(num)$ask0, type='s', col='red', ylim=ylim)
 # plot(format(as.POSIXlt(execs$Time/1000, origin="2012-09-28"), format="%H:%M:%OS3"),execs$sum, type='s', col='red', ylim=ylim)
 
# 
trade <- function(row) {

		price 		<- as.numeric(row[[6]])/10000
		size 		<- as.numeric(row[[5]])
		tradetype 	<- as.character(row[[4]])
		externalid	<- as.integer(row[[3]])
		sym 		<- as.character(row[[2]])
		time 		<- as.integer(row[[1]])
	
		count <<- 1 + count	

		if(tradetype=='D') {
			cancel(externalid)
		} else if(tradetype=='B') {
			limit(sym, "mkt", 0, price, size, "GTC", time, externalid)
		} else if(tradetype=='S') {
			limit(sym, "mkt", 1, price, size, "GTC", time, externalid)
		} else if(tradetype=='C') {
			cancel(id, qty=size, time, externalid)
		} 


		ob <- get.ob()
		
		bestAsk <- ob[10,]
		bestBid <- ob[11,]	
		 
		spread <- ob[10,]$price - ob[11,]$price
		  		 
		# this is our trade opportunity. If the spread is wide we can provide liquidity  		 
		if(spread >0.011) {
			# choose whether to offer at the bid or the ask based on current position
			if(position > 0) {
				# we are positive - offer to sell
				if(is.null(sell.quote.id)) {
					print('sell')
					id <<- limit("sym", "sell", 1, bestAsk$price-0.01, 1, "GTC")
					sell.quote.id <<- id
				}
			} else if(position <0) {
				# we are negative - offer to buy
				if(is.null(buy.quote.id)) {
					print('buy')
					id <<- limit("sym", "buy", 0, bestBid$price+0.01, 1, "GTC")
					buy.quote.id <<- id
				}
			} else if(position ==0 && spread >0.021) {
				# we are flat - offer both
				if(is.null(sell.quote.id)) {
					print('both sell')
					id <<- limit("sym", "bos", 1, bestAsk$price-0.01, 1, "GTC")
					sell.quote.id <<- id
				}
				if(is.null(buy.quote.id)) {
					print('both buy')
					id <<- limit("sym", "bob", 0, bestBid$price+0.01, 1, "GTC")
					buy.quote.id <<- id
				}
			}
		} 	
		
		# check for fills and adjust the position
		if(!is.null(sell.quote.id)) {
			fillqty <<- get.filled.qty(sell.quote.id)
			if(fillqty >0) {
				currentOrder <<- orders[sell.quote.id]
				if(currentOrder$side==0) {
					position <<- position + 1
				} else {
					position <<- position - 1
				}
				sell.quote.id <<- NULL
			}
		}
		if(!is.null(buy.quote.id)) {	
			fillqty <<- get.filled.qty(buy.quote.id)
			if(fillqty >0) {
				currentOrder <<- orders[buy.quote.id]
				if(currentOrder$side==0) {
					position <<- position + 1
				} else {
					position <<- position - 1
				}
				buy.quote.id <<- NULL
			} 			
		}
		
		# stop loss on any open order
		if(!is.null(sell.quote.id)) {
			currentOrder <<- orders[sell.quote.id]
			print(paste('stop sell id:', sell.quote.id, 'orderPrice:', currentOrder$price, 'bestAsk:', bestAsk$price))
			if(currentOrder$price > bestAsk + 0.10) {
				cancel(sell.quote.id)
				print('stopped sell')
				sell.quote.id <<- NULL
			}
		}
		if(!is.null(buy.quote.id)) {
			currentOrder <<- orders[buy.quote.id]
			print(paste('stop buy id:', buy.quote.id, 'orderPrice:', currentOrder$price, 'bestBid:', bestBid$price))
			if(currentOrder$price < bestBid - 0.10) {
				cancel(buy.quote.id)
				print('stopped buy')
				buy.quote.id <<- NULL
			}
		}
		

}

# hsub$bidqty2 <- ifelse(hsub$bidqty3==0,1,hsub$bidqty3)
# hsub$bidqty1 <- ifelse(hsub$bidqty1==0,1,hsub$bidqty1)

# hsub$askqty0f <- cut(log(hsub$askqty0),8)
# hsub$askqty1f <- cut(log(hsub$askqty1),8)
# hsub$askqty2f <- cut(log(hsub$askqty2),8)
# hsub$bidqty2f <- cut(log(hsub$bidqty2),8)
# hsub$bidqty1f <- cut(log(hsub$bidqty1),8)
# hsub$bidqty0f <- cut(log(hsub$bidqty0),8)


# some stats on performance
# perf <- data.frame(n=1:100000, t=rep(0,100000))
# for(i in 1:100000) { init(i); x<- system.time(generate()); perf[i,]$t=x[[3]]}



init <- function(num=1000) {


	library(testthat)
	library(data.table)
	library(mobster)

	.C("init", as.integer(num), as.integer(10000), as.integer(9999))
	
}

test_that("init initialises the price", {
	
	init(100)
	ob<-get.ob()
	expect_that(ob$qty[[10]], equals(0))
	expect_that(ob$price[[10]], equals(100))
	expect_that(ob$price[[11]], equals(99.99))
	
	limit("sym", "trader", 0, 100, 10, "GTC")
	ob<-get.ob()
	expect_that(ob$qty[[11]], equals(10))
	
	hob <- get.hob(10)
	expect_that(ob$qty[[11]], equals(10))
})

test_that("limit Buy Order Quote", {
	
	init(100)
	
	limit("sym", "trader", 0, 99.99, 5, "GTC")
	ob<-get.ob()
	expect_that(ob$price[[11]], equals(99.99))
	expect_that(ob$qty[[11]], equals(5))
	expect_that(ob$qty[[10]], equals(0))
	expect_that(ob$price[[10]], equals(100.00))
	
	limit("sym", "trader", 0, 99.99, 5, "GTC")
	ob<-get.ob()
	expect_that(ob$qty[[11]], equals(10))
	expect_that(ob$qty[[10]], equals(0))
	expect_that(ob$price[[10]], equals(100.00))
	
	limit("sym", "trader", 0, 99.98, 15, "GTC")
	ob<-get.ob()
	expect_that(ob$qty[[11]], equals(10))
	expect_that(ob$qty[[12]], equals(15))
	expect_that(ob$qty[[10]], equals(0))
	expect_that(ob$price[[10]], equals(100.00))

	limit("sym", "trader", 0, 99.9, 15, "GTC")
	ob<-get.ob()
	expect_that(ob$qty[[11]], equals(10))
	expect_that(ob$qty[[12]], equals(15))
	expect_that(ob$qty[[20]], equals(15))
	expect_that(ob$price[[20]], equals(99.90))
	expect_that(ob$qty[[10]], equals(0))
	expect_that(ob$price[[10]], equals(100.00))
	
	# not added
	limit("sym", "trader", 0, 99.8, 15, "GTC")
	ob<-get.ob()
	expect_that(ob$qty[[11]], equals(10))
	expect_that(ob$qty[[12]], equals(15))
	expect_that(ob$qty[[20]], equals(15))
	expect_that(ob$price[[20]], equals(99.90))
	expect_that(ob$qty[[10]], equals(0))
	expect_that(ob$price[[10]], equals(100.00))

	
	hob <- get.hob(10)
	expect_that(hob$askqty0[[1]], equals(0))
	expect_that(hob$ask0[[1]], equals(10000))

	expect_that(hob$bidqty0[[1]], equals(5))
	expect_that(hob$bid0[[1]], equals(9999))
	expect_that(hob$bidqty0[[2]], equals(10))
	expect_that(hob$bid0[[2]], equals(9999))
	expect_that(hob$bidqty0[[3]], equals(10))
	expect_that(hob$bid0[[3]], equals(9999))
	expect_that(hob$bidqty1[[4]], equals(15))
	expect_that(hob$bid1[[4]], equals(9998))
			
})

test_that("test limit Sell Order Quote", {
	
	init(100)
	
	limit("sym", "trader", 1, 100.00, 5, "GTC")
	ob<-get.ob()
	expect_that(ob$price[[10]], equals(100.00))
	expect_that(ob$qty[[10]], equals(5))
	expect_that(ob$qty[[9]], equals(0))
	expect_that(ob$price[[9]], equals(100.01))
	
	limit("sym", "trader", 1, 100.00, 5, "GTC")
	ob<-get.ob()
	expect_that(ob$qty[[10]], equals(10))
	expect_that(ob$qty[[9]], equals(0))
	expect_that(ob$price[[9]], equals(100.01))
	
	limit("sym", "trader", 1, 100.01, 15, "GTC")
	ob<-get.ob()
	expect_that(ob$qty[[10]], equals(10))
	expect_that(ob$qty[[9]], equals(15))
	expect_that(ob$qty[[8]], equals(0))
	expect_that(ob$price[[8]], equals(100.02))

	limit("sym", "trader", 1, 100.09, 15, "GTC")
	ob<-get.ob()
	expect_that(ob$qty[[10]], equals(10))
	expect_that(ob$qty[[9]], equals(15))
	expect_that(ob$qty[[1]], equals(15))
	expect_that(ob$price[[1]], equals(100.09))
	expect_that(ob$qty[[11]], equals(0))
	expect_that(ob$price[[11]], equals(99.99))
	
	
	hob <- get.hob(10)

	expect_that(hob$askqty0[[1]], equals(5))
	expect_that(hob$ask0[[1]], equals(10000))
	expect_that(hob$askqty0[[2]], equals(10))
	expect_that(hob$ask0[[2]], equals(10000))
	expect_that(hob$askqty0[[3]], equals(10))
	expect_that(hob$ask0[[3]], equals(10000))
	expect_that(hob$askqty1[[4]], equals(15))
	expect_that(hob$ask1[[4]], equals(10001))	
})

test_that("test limit Sell Order Quote far from top of book", {
	
	init(100)
	
	limit("sym", "trader", 1, 110.00, 5, "GTC")
	ob<-get.ob()
	expect_that(ob$price[[10]], equals(110.00))
	expect_that(ob$qty[[10]], equals(5))
	expect_that(ob$qty[[9]], equals(0))
	expect_that(ob$price[[9]], equals(110.01))
})



test_that("test limit Sell Order Trade", {
	
	init(100)

	# this should match	and give a fill
	limit("sym", "trader", 1, 100.00, 10, "GTC")
	limit("sym", "trader", 0, 100.00, 5, "GTC")
	ob<-get.ob()
	expect_that(ob$qty[[10]], equals(5))
	execs <- get.execs(10) 
	expect_that(execs$id[1], equals(1))
	expect_that(execs$oid[1], equals(2))
	expect_that(execs$mid[1], equals(1))
	expect_that(execs$side[1], equals(0))
	expect_that(execs$price[1], equals(10000))	
	expect_that(execs$fillqty[1], equals(5))	
	expect_that(execs$id[2], equals(2))
	expect_that(execs$oid[2], equals(1))
	expect_that(execs$mid[2], equals(2))
	expect_that(execs$side[2], equals(1))
	expect_that(execs$price[2], equals(10000))	
	expect_that(execs$fillqty[2], equals(5))	

	# this should increase the qty	
	limit("sym", "trader", 1, 100.00, 5, "GTC")
	ob<-get.ob()
	expect_that(ob$qty[[10]], equals(10))
	
	# this should match	and give 2 fills, of 5 each and  1 miss that is left in the book
	limit("sym", "trader", 0, 100.00, 15, "GTC")
	ob<-get.ob()
	expect_that(ob$qty[[11]], equals(5))
	expect_that(ob$side[[11]], equals(0))
	expect_that(ob$price[[11]], equals(100.00))
	execs <- get.execs(10) 

	expect_that(execs$id[3], equals(3))
	expect_that(execs$oid[3], equals(4))
	expect_that(execs$mid[3], equals(1))
	expect_that(execs$side[3], equals(0))
	expect_that(execs$price[3], equals(10000))	
	expect_that(execs$fillqty[3], equals(5))	
	expect_that(execs$id[4], equals(4))
	expect_that(execs$oid[4], equals(1))
	expect_that(execs$mid[4], equals(4))
	expect_that(execs$side[4], equals(1))
	expect_that(execs$price[4], equals(10000))	
	expect_that(execs$fillqty[4], equals(5))
	
	expect_that(execs$id[5], equals(5))
	expect_that(execs$oid[5], equals(4))
	expect_that(execs$mid[5], equals(3))
	expect_that(execs$side[5], equals(0))
	expect_that(execs$price[5], equals(10000))	
	expect_that(execs$fillqty[5], equals(5))	
	expect_that(execs$id[6], equals(6))
	expect_that(execs$oid[6], equals(3))
	expect_that(execs$mid[6], equals(4))
	expect_that(execs$side[6], equals(1))
	expect_that(execs$price[6], equals(10000))	
	expect_that(execs$fillqty[6], equals(5))		
})


test_that("test limit Buy Order Trade", {
	
	init(100)

	# this should match	and give a fill
	limit("sym", "trader", 0, 100.00, 10, "GTC")
	limit("sym", "trader", 1, 100.00, 5, "GTC")
	ob<-get.ob()
	expect_that(ob$qty[[11]], equals(5))
	execs <- get.execs(10) 
	expect_that(execs$id[1], equals(1))
	expect_that(execs$oid[1], equals(1))
	expect_that(execs$mid[1], equals(2))
	expect_that(execs$side[1], equals(0))
	expect_that(execs$price[1], equals(10000))	
	expect_that(execs$fillqty[1], equals(5))	
	expect_that(execs$id[2], equals(2))
	expect_that(execs$oid[2], equals(2))
	expect_that(execs$mid[2], equals(1))
	expect_that(execs$price[2], equals(10000))
	expect_that(execs$side[2], equals(1))	
	expect_that(execs$fillqty[2], equals(5))	

	# this should increase the qty	
	limit("sym", "trader", 0, 100.00, 5, "GTC")
	ob<-get.ob()
	expect_that(ob$qty[[11]], equals(10))
	
	# this should match	and give 2 fills, of 5 each and  1 miss that is left in the book
	limit("sym", "trader", 1, 100.00, 15, "GTC")
	ob<-get.ob()
	expect_that(ob$qty[[10]], equals(5))
	expect_that(ob$side[[10]], equals(1))
	expect_that(ob$price[[10]], equals(100.00))
	execs <- get.execs(10) 

	expect_that(execs$id[3], equals(3))
	expect_that(execs$oid[3], equals(1))
	expect_that(execs$mid[3], equals(4))
	expect_that(execs$side[3], equals(0))
	expect_that(execs$price[3], equals(10000))	
	expect_that(execs$fillqty[3], equals(5))	
	expect_that(execs$id[4], equals(4))
	expect_that(execs$oid[4], equals(4))
	expect_that(execs$mid[4], equals(1))
	expect_that(execs$side[4], equals(1))
	expect_that(execs$price[4], equals(10000))	
	expect_that(execs$fillqty[4], equals(5))	
	
	expect_that(execs$id[5], equals(5))
	expect_that(execs$oid[5], equals(3))
	expect_that(execs$mid[5], equals(4))
	expect_that(execs$side[5], equals(0))
	expect_that(execs$price[5], equals(10000))	
	expect_that(execs$fillqty[5], equals(5))	
	expect_that(execs$id[6], equals(6))
	expect_that(execs$oid[6], equals(4))
	expect_that(execs$mid[6], equals(3))
	expect_that(execs$side[6], equals(1))
	expect_that(execs$price[6], equals(10000))	
	expect_that(execs$fillqty[6], equals(5))	
})


test_that("buy order sweeps the ask market",{
	
	init(100)

	# fill multiple levels
	limit("sym", "trader", 1, 100.00, 1, "GTC")
	limit("sym", "trader", 1, 100.01, 1, "GTC")
	limit("sym", "trader", 1, 100.02, 1, "GTC")
	limit("sym", "trader", 1, 100.03, 1, "GTC")
	limit("sym", "trader", 1, 100.04, 1, "GTC")

	# verify which level fills - should be at level 0 and price level 0 in the execs
	limit("sym", "trader", 0, 100.04, 1, "GTC")
	
	ob<-get.ob()
	expect_that(ob$qty[[10]], equals(1))
	expect_that(ob$price[[10]], equals(100.01))	
	execs <- get.execs(10)
	
	expect_that(execs$id[1], equals(1))
	expect_that(execs$oid[1], equals(6))
	expect_that(execs$mid[1], equals(1))
	expect_that(execs$side[1], equals(0))
	expect_that(execs$price[1], equals(10000))	
	expect_that(execs$fillqty[1], equals(1))	
	expect_that(execs$id[2], equals(2))
	expect_that(execs$oid[2], equals(1))
	expect_that(execs$mid[2], equals(6))
	expect_that(execs$side[2], equals(1))
	expect_that(execs$price[2], equals(10000))	
	expect_that(execs$fillqty[2], equals(1))
	
	# cross more than one level - should report all the data
	limit("sym", "trader", 0, 100.04, 2, "GTC")
	ob<-get.ob()
	expect_that(ob$qty[[10]], equals(1))
	expect_that(ob$price[[10]], equals(100.03))	
	expect_that(ob$qty[[9]], equals(1))
	expect_that(ob$price[[9]], equals(100.04))	
	execs <- get.execs(10)
	
	expect_that(execs$id[3], equals(3))
	expect_that(execs$oid[3], equals(7))
	expect_that(execs$mid[3], equals(2))
	expect_that(execs$side[3], equals(0))
	expect_that(execs$price[3], equals(10001))	
	expect_that(execs$fillqty[3], equals(1))	
	expect_that(execs$id[4], equals(4))
	expect_that(execs$oid[4], equals(2))
	expect_that(execs$mid[4], equals(7))
	expect_that(execs$side[4], equals(1))
	expect_that(execs$price[4], equals(10001))	
	expect_that(execs$fillqty[4], equals(1))	
		
	expect_that(execs$id[5], equals(5))
	expect_that(execs$oid[5], equals(7))
	expect_that(execs$mid[5], equals(3))
	expect_that(execs$side[5], equals(0))
	expect_that(execs$price[5], equals(10002))	
	expect_that(execs$fillqty[5], equals(1))	
	expect_that(execs$id[6], equals(6))
	expect_that(execs$oid[6], equals(3))
	expect_that(execs$mid[6], equals(7))
	expect_that(execs$side[6], equals(1))
	expect_that(execs$price[6], equals(10002))	
	expect_that(execs$fillqty[6], equals(1))	
		
})

test_that("sell order sweeps the bid market",{
	init(100)

	# fill multiple levels
	limit("sym", "trader", 0, 100.00, 1, "GTC")
	limit("sym", "trader", 0, 100.01, 1, "GTC")
	limit("sym", "trader", 0, 100.02, 1, "GTC")
	limit("sym", "trader", 0, 100.03, 1, "GTC")
	limit("sym", "trader", 0, 100.04, 1, "GTC")

	# verify which level fills - should be at level 0and price level 0 in the execs
	limit("sym", "trader", 1, 100.00, 1, "GTC")
	
	ob<-get.ob()
	expect_that(ob$qty[[11]], equals(1))
	expect_that(ob$price[[11]], equals(100.03))	
	execs <- get.execs(10)
	
	expect_that(execs$id[1], equals(1))
	expect_that(execs$oid[1], equals(5))
	expect_that(execs$mid[1], equals(6))
	expect_that(execs$side[1], equals(0))
	expect_that(execs$price[1], equals(10004))	
	expect_that(execs$fillqty[1], equals(1))	
	expect_that(execs$id[2], equals(2))
	expect_that(execs$oid[2], equals(6))
	expect_that(execs$mid[2], equals(5))
	expect_that(execs$side[2], equals(1))
	expect_that(execs$price[2], equals(10004))	
	expect_that(execs$fillqty[2], equals(1))
	
	# cross more than one level - should report all the data
	limit("sym", "trader", 1, 100.00, 2, "GTC")
	ob<-get.ob()
	expect_that(ob$qty[[11]], equals(1))
	expect_that(ob$price[[11]], equals(100.01))	
	expect_that(ob$qty[[12]], equals(1))
	expect_that(ob$price[[12]], equals(100.00))	
	execs <- get.execs(10)
	
	expect_that(execs$id[3], equals(3))
	expect_that(execs$oid[3], equals(4))
	expect_that(execs$mid[3], equals(7))
	expect_that(execs$side[3], equals(0))
	expect_that(execs$price[3], equals(10003))	
	expect_that(execs$fillqty[3], equals(1))	
	expect_that(execs$id[4], equals(4))
	expect_that(execs$oid[4], equals(7))
	expect_that(execs$mid[4], equals(4))
	expect_that(execs$side[4], equals(1))
	expect_that(execs$price[4], equals(10003))	
	expect_that(execs$fillqty[4], equals(1))	
		
	expect_that(execs$id[5], equals(5))
	expect_that(execs$oid[5], equals(3))
	expect_that(execs$mid[5], equals(7))
	expect_that(execs$side[5], equals(0))
	expect_that(execs$price[5], equals(10002))	
	expect_that(execs$fillqty[5], equals(1))	
	expect_that(execs$id[6], equals(6))
	expect_that(execs$oid[6], equals(7))
	expect_that(execs$mid[6], equals(3))
	expect_that(execs$side[6], equals(1))
	expect_that(execs$price[6], equals(10002))	
	expect_that(execs$fillqty[6], equals(1))	
		
})

# test_that("market order", {
# 
# 	init(100)
# 
# 	# fill multiple levels
# 	limit("sym", "trader", 0, 100.00, 3, "GTC")
# 	limit("sym", "trader", 0, 100.01, 3, "GTC")
# 	limit("sym", "trader", 0, 100.02, 3, "GTC")
# 	limit("sym", "trader", 0, 100.03, 3, "GTC")
# 	limit("sym", "trader", 0, 100.04, 3, "GTC")
# 
# 	# verify which level fills - should be at level 0and price level 0 in the execs
# 	#sym, trader, side, price, qty
# 	market("sym", "trader", 1, 92.00, 6)
# 	ob <- get.ob()
# 	expect_that(ob$qty[[11]], equals(3))
# 	expect_that(ob$price[[11]], equals(100.02))	
# 	expect_that(ob$qty[[12]], equals(3))
# 	expect_that(ob$price[[12]], equals(100.01))	
# 	
# 	init(100)
# 
# 	# fill multiple levels
# 	limit("sym", "trader", 1, 100.00, 3, "GTC")
# 	limit("sym", "trader", 1, 100.01, 3, "GTC")
# 	limit("sym", "trader", 1, 100.02, 3, "GTC")
# 	limit("sym", "trader", 1, 100.03, 3, "GTC")
# 	limit("sym", "trader", 1, 100.04, 3, "GTC")
# 
# 	# verify which level fills - should be at level 0and price level 0 in the execs
# 	#sym, trader, side, price, qty
# 	market("sym", "trader", 0, 92.00, 6)
# 	ob <- get.ob()
# 	expect_that(ob$qty[[10]], equals(3))
# 	expect_that(ob$price[[10]], equals(100.02))	
# 	expect_that(ob$qty[[9]], equals(3))
# 	expect_that(ob$price[[9]], equals(100.03))	
# })
# 
# 
# test_that("market order 2", {
# 	init(100)
# 
# 	# fill multiple levels
# 	limit("sym", "trader", 0, 100.00, 3, "GTC")
# 	limit("sym", "trader", 0, 100.01, 3, "GTC")
# 	limit("sym", "trader", 0, 100.02, 3, "GTC")
# 	limit("sym", "trader", 0, 100.03, 3, "GTC")
# 	limit("sym", "trader", 0, 100.04, 3, "GTC")
# 
# 	# verify which level fills - should be at level 0 and price level 0 in the execs
# 	#sym, trader, side, price, qty
# 	id <- market("sym", "trader", 1, 92.00, 6)
# 	ob<-get.ob()
# 	expect_that(ob$qty[[11]], equals(3))
# 	expect_that(ob$price[[11]], equals(100.02))	
# 	expect_that(ob$price[[12]], equals(100.01))	
# 	#order <- orders[id]
# 
# 	init(100)
# 
# 	# fill multiple levels
# 	limit("sym", "trader", 1, 100.00, 3, "GTC")
# 	limit("sym", "trader", 1, 100.01, 3, "GTC")
# 	limit("sym", "trader", 1, 100.02, 3, "GTC")
# 	limit("sym", "trader", 1, 100.03, 3, "GTC")
# 	limit("sym", "trader", 1, 100.04, 3, "GTC")
# 
# 	# verify which level fills - should be at level 0 and price level 0 in the execs
# 	#sym, trader, side, price, qty
# 	market("sym", "trader", 0, 92.00, 6)
# 	ob<-get.ob()
# 	expect_that(ob$qty[[10]], equals(3))
# 	expect_that(ob$price[[10]], equals(100.02))	
# 	expect_that(ob$qty[[9]], equals(3))
# 	expect_that(ob$price[[9]], equals(100.03))
# 	
# 	# empty orderbook market orders
# 
# 	init(100)
# 
# 	# verify which level fills - should be at level 0 and price level 0 in the execs
# 	#sym, trader, side, price, qty
# 	market("sym", "trader", 0, 92.00, 6)
# 	ob<-get.ob()
# 	expect_that(ob$qty[[10]], equals(0))
# 	expect_that(ob$price[[10]], equals(655.36))	
# 	expect_that(ob$qty[[9]], equals(0))
# 	expect_that(ob$price[[9]], equals(655.37))
# 	expect_that(ob$qty[[11]], equals(0))
# 	expect_that(ob$price[[11]], equals(99.99))	
# 	expect_that(ob$qty[[12]], equals(0))
# 	expect_that(ob$price[[12]], equals(99.98))
# 		
# 	market("sym", "trader",1, 92.00, 6)
# 	ob<-get.ob()
# 	expect_that(ob$qty[[10]], equals(0))
# 	expect_that(ob$price[[10]], equals(655.36))	
# 	expect_that(ob$qty[[9]], equals(0))
# 	expect_that(ob$price[[9]], equals(655.37))
# 	expect_that(ob$qty[[11]], equals(0))
# 	expect_that(ob$price[[11]], equals(0.99))	
# 	expect_that(ob$qty[[12]], equals(0))
# 	expect_that(ob$price[[12]], equals(0.98))
# 
# })

test_that("trader is passed correctly", {

	init(100)

	# fill multiple levels
	limit("sym", "t1", 0, 100.00, 3, "GTC")
	limit("sym", "t2", 1, 100.00, 3, "GTC")
	ob<-get.ob()
	execs <- get.execs(10);
	expect_that(execs[1,]$trader, equals("t1"))
	expect_that(execs[2,]$trader, equals("t2"))	
})

test_that("initialise historical order book more than once correctly", {

	init(100)
	
	limit("sym", "trader", 0, 99.99, 5, "GTC")
	limit("sym", "trader", 0, 99.99, 5, "GTC")
	limit("sym", "trader", 0, 99.98, 15, "GTC")
	limit("sym", "trader", 0, 99.9, 15, "GTC")
	# not added
	limit("sym", "trader", 0, 99.8, 15, "GTC")

	
	hob <- get.hob(10)
	expect_that(hob$askqty0[[1]], equals(0))
	expect_that(hob$ask0[[1]], equals(10000))

	expect_that(hob$bidqty0[[1]], equals(5))
	expect_that(hob$bid0[[1]], equals(9999))
	expect_that(hob$bidqty0[[2]], equals(10))
	expect_that(hob$bid0[[2]], equals(9999))
	expect_that(hob$bidqty0[[3]], equals(10))
	expect_that(hob$bid0[[3]], equals(9999))
	expect_that(hob$bidqty1[[4]], equals(15))
	expect_that(hob$bid1[[4]], equals(9998))
	
	init(100)
	
	limit("sym", "trader", 0, 99.99, 5, "GTC")
	limit("sym", "trader", 0, 99.99, 5, "GTC")
	limit("sym", "trader", 0, 99.98, 15, "GTC")
	limit("sym", "trader", 0, 99.9, 15, "GTC")
	limit("sym", "trader", 0, 99.8, 15, "GTC")

	
	hob <- get.hob(10)
	expect_that(hob$askqty0[[1]], equals(0))
	expect_that(hob$ask0[[1]], equals(10000))

	expect_that(hob$totalTraded[[1]], equals(0))
	expect_that(hob$totalTrades[[1]], equals(0))
	expect_that(hob$bidqty0[[1]], equals(5))
	expect_that(hob$bid0[[1]], equals(9999))
	expect_that(hob$bidqty0[[2]], equals(10))
	expect_that(hob$bid0[[2]], equals(9999))
	expect_that(hob$bidqty0[[3]], equals(10))
	expect_that(hob$bid0[[3]], equals(9999))
	expect_that(hob$bidqty1[[4]], equals(15))
	expect_that(hob$bid1[[4]], equals(9998))
	
	expect_that(hob$bidqty0[[5]], equals(10))
	expect_that(hob$bid0[[5]], equals(9999))

	expect_that(hob$totalTraded[[1]], equals(0))
	expect_that(hob$totalTrades[[1]], equals(0))
	
	expect_that(hob$totalTraded[[5]], equals(0))
	expect_that(hob$totalTrades[[5]], equals(0))
	
})


test_that("filled qty for an order", {

	init(100)

	id <- limit("sym", "trader", 0, 99.99, 5, "GTC")
	id2 <- limit("sym", "trader", 0, 99.99, 5, "GTC")
	limit("sym", "trader", 0, 99.98, 15, "GTC")
	limit("sym", "trader", 0, 99.9, 15, "GTC")

	expect_that(get.filled.qty(id), equals(0))
	
	mid <- limit("sym", "trader", 1, 99.99, 1, "GTC")
	
	expect_that(get.filled.qty(id), equals(1))
	expect_that(get.filled.qty(mid), equals(1))
	
	mid <- limit("sym", "trader", 1, 99.99, 1, "GTC")

	expect_that(get.filled.qty(id), equals(2))
	expect_that(get.filled.qty(mid), equals(1))
	
	mid <- limit("sym", "trader", 1, 99.99, 1, "GTC")

	expect_that(get.filled.qty(id), equals(3))
	expect_that(get.filled.qty(mid), equals(1))

	mid <- limit("sym", "trader", 1, 99.99, 1, "GTC")

	expect_that(get.filled.qty(id), equals(4))
	expect_that(get.filled.qty(mid), equals(1))
	
	mid <- limit("sym", "trader", 1, 99.99, 1, "GTC")

	expect_that(get.filled.qty(id), equals(5))
	expect_that(get.filled.qty(mid), equals(1))
	
	mid <- limit("sym", "trader", 1, 99.99, 1, "GTC")

	expect_that(get.filled.qty(id), equals(5))
	expect_that(get.filled.qty(mid), equals(1))
	expect_that(get.filled.qty(id2), equals(1))
})


test_that("FAK orders", {

	init(100)

	# fill multiple levels
	limit("sym", "t2", 1, 100.00, 3, "GTC")
	limit("sym", "t2", 1, 100.01, 3, "GTC")
	limit("sym", "t2", 1, 100.02, 3, "GTC")
	
	# FAK should fill and then there should be nothing on the other side in the book
	id <- limit("sym", "t2", 0, 100.00, 20, "FAK")
	ob <- get.ob()
	
	expect_that(ob$price[[10]], equals(100.01))
	expect_that(ob$price[[11]], equals(99.99))
	expect_that(ob$qty[[10]], equals(3))
	expect_that(ob$qty[[11]], equals(0))
	filled <- get.filled.qty(id)
	expect_that(filled, equals(3))
	
})

test_that("cancel orders", {

	init(100)

	# fill multiple levels
	id1 <- limit("sym", "t2", 1, 100.00, 3, "GTC")
	id2 <- limit("sym", "t2", 1, 100.01, 3, "GTC")
	id3 <- limit("sym", "t2", 1, 100.02, 3, "GTC")
	
	# cancel part should leave some in the book
	cancel(id1, 1)
	ob <- get.ob()
	
	expect_that(ob$price[[10]], equals(100.00))
	expect_that(ob$price[[11]], equals(99.99))
	expect_that(ob$qty[[10]], equals(2))
	expect_that(ob$qty[[11]], equals(0))
	filled <- get.filled.qty(id1)
	expect_that(filled, equals(0))

	# cancel the remaining
	cancel(id1)
	ob <- get.ob()
	
	expect_that(ob$price[[10]], equals(100.00))
	expect_that(ob$price[[11]], equals(99.99))
	expect_that(ob$qty[[10]], equals(0))
	expect_that(ob$qty[[11]], equals(0))
	filled <- get.filled.qty(id1)
	expect_that(filled, equals(0))

	cancel(id1, 1)
	ob <- get.ob()
	expect_that(ob$price[[10]], equals(100.00))
	expect_that(ob$price[[11]], equals(99.99))
	expect_that(ob$qty[[10]], equals(0))
	expect_that(ob$qty[[11]], equals(0))
	filled <- get.filled.qty(id1)
	expect_that(filled, equals(0))
	
})

test_that("externalid is populated on execution reports", {

	init(100)

	id1 <- limit("sym", "t2", 1, 100.00, 3, "GTC", externalId=20000)
	id2 <- limit("sym", "t2", 1, 100.01, 3, "GTC", externalId=20001)
	id3 <- limit("sym", "t1", 0, 100.01, 1, "GTC", externalId=30002)
	id4 <- limit("sym", "t1", 0, 100.01, 7, "GTC", externalId=30003)
	
	ob <- get.ob()
	
	expect_that(ob$price[[10]], equals(100.02))
	expect_that(ob$price[[11]], equals(100.01))
	expect_that(ob$qty[[11]], equals(2))
	expect_that(ob$qty[[10]], equals(0))
	
	filled <- get.filled.qty(id1)
	expect_that(filled, equals(3))
	
	e <- get.execs(10)
	
	expect_that(e[1]$eoid, equals(30002))
	expect_that(e[1]$emid, equals(20000))
	expect_that(e[2]$eoid, equals(20000))
	expect_that(e[2]$emid, equals(30002))
	expect_that(e[3]$eoid, equals(30003))
	expect_that(e[3]$emid, equals(20000))
	expect_that(e[4]$eoid, equals(20000))
	expect_that(e[4]$emid, equals(30003))

})

/****************************************************************
* DISCLAIMER
*
* The core of the orderbook engine is shamelessly 
* lifted from the winning submission by voyager from the 
* Quant Cup. It is customised here to allow integration with R.  
* 
*****************************************************************/

#include <stdio.h>
#include <strings.h>
#include <stdlib.h>
#include "engine.h"
#include "history.h"
#include <R.h>


//#define DEBUG_PRINT_ENABLED 1  // uncomment to enable DEBUG statements
#if DEBUG_PRINT_ENABLED
#define DEBUG printf
#else
#define DEBUG(format, args...) ((void)0)
#endif

t_orderid orderid;
int curFillId = 0;
int num_txn = 0;

/* Enable/disable optimizations */
#define UNROLL_STRCPY

#define MAX_NUM_ORDERS 1010000

#ifdef UNROLL_STRCPY
#define COPY_STRING(dst, src) do {		         \
dst[0] = src[0]; dst[1] = src[1]; dst[2] = src[2];     \
dst[3] = src[3]; /* dst[4] = src[4]; */		 \
} while(0)
#else
#include <string.h>
#define COPY_STRING(dst, src) strcpy(dst, src)
#endif


/* record executions - both sides */
int e_exec_id[1000000];
int e_exec_time[1000000];
int e_exec_oid[1000000];
int e_exec_mid[1000000];
int e_exec_side[1000000];
char e_exec_trader[1000000][8];
int e_exec_fillqty[1000000];
int e_exec_price[1000000];
double e_exec_midp[1000000];

/* track how much has been filled in an order */
int orderFills[1000000];
/* when replaying itch data we may want to cancel a different id to the internally created id */
int externalIds[100000000];


/* Monotonically-increasing orderID */
static t_orderid curOrderID;          


/* Statically-allocated memory arena for order book entries. This data 
 structure allows us to avoid the overhead of heap-based memory allocation. */
static orderBookEntry_t arenaBookEntries[MAX_NUM_ORDERS] = {0};

static orderBookEntry_t *arenaPtr;

void init(int * size, int* RaskMin, int* RbidMax) {
	orderid = 0;
	num_txn = 0;
	curFillId = 0;
	
	/* Initialize the price point array */
	bzero(pricePoints, (MAX_PRICE + 1) * sizeof(pricePoint_t));  
	
	/* Initialize the memory arena */
	bzero(arenaBookEntries, MAX_NUM_ORDERS * sizeof(orderBookEntry_t));
	arenaPtr = arenaBookEntries;   // Bring the arena pointer into the cache
	
	curOrderID = 0;
	askMin = RaskMin[0];
	bidMax = RbidMax[0];
	
	totalTraded = 0;
    totalTrades = 0;
    
    curFillId = 0;
	
	int max = size[0] * 1000;
	
	/* reset the values to zero TODO - implement as memset */
	for(int i=0; i < size[0]; i++) 
	{
	
		e_exec_id[i] = 0;
		e_exec_time[i] = 0;
		e_exec_oid[i] = 0;
		e_exec_mid[i] = 0;
		e_exec_side[i] = 0;
		e_exec_trader[i][0] = '\0';
		e_exec_fillqty[i] = 0;
		e_exec_price[i] = 0;
		e_exec_midp[i] = 0;
	
		ob_id[i] = 0;
		ob_time[i] = 0;
		ob_traded_qty[i] = 0;
		ob_trade_count[i] = 0;
		ob_ask0[i] = 0;
		ob_askqty0[i] = 0;
		ob_ask1[i] = 0;
		ob_askqty1[i] = 0;
		ob_ask2[i] = 0;
		ob_askqty2[i] = 0;
		ob_ask3[i] = 0;
		ob_askqty3[i] = 0;
		ob_ask4[i] = 0;
		ob_askqty4[i] = 0;
		ob_ask5[i] = 0;
		ob_askqty5[i] = 0;
		ob_ask6[i] = 0;
		ob_askqty6[i] = 0;
		ob_ask7[i] = 0;
		ob_askqty7[i] = 0;
		ob_ask8[i] = 0;
		ob_askqty8[i] = 0;
		ob_ask9[i] = 0;
		ob_askqty9[i] = 0;
		ob_ask10[i] = 0;
		ob_askqty10[i] = 0;
	
		ob_bid0[i] = 0;
		ob_bidqty0[i] = 0;
		ob_bid1[i] = 0;
		ob_bidqty1[i] = 0;
		ob_bid2[i] = 0;
		ob_bidqty2[i] = 0;
		ob_bid3[i] = 0;
		ob_bidqty3[i] = 0;
		ob_bid4[i] = 0;
		ob_bidqty4[i] = 0;
		ob_bid5[i] = 0;
		ob_bidqty5[i] = 0;
		ob_bid6[i] = 0;
		ob_bidqty6[i] = 0;
		ob_bid7[i] = 0;
		ob_bidqty7[i] = 0;
		ob_bid8[i] = 0;
		ob_bidqty8[i] = 0;
		ob_bid9[i] = 0;
		ob_bidqty9[i] = 0;
		ob_bid10[i] = 0;
		ob_bidqty10[i] = 0;
		
		orderFills[i] = 0;
		externalIds[i] = 0;
	}	
}


void destroy() { }


/* get the given number of executions from the execution history */
void get_execs(int* rows, int* Rid, int*Roid, int* Rmid, int* Rside, char* Rtrader[8], int* Rqty,int* Rprice, double* Rmidp, int* Rtime) {
	for(int i=0; i<rows[0]; i++) {
		Rid[i] = e_exec_id[i];
		Rtime[i] = e_exec_time[i];
		Roid[i] = e_exec_oid[i];
		Rmid[i] = e_exec_mid[i];
		Rside[i] = e_exec_side[i];
		Rtrader[i] = &e_exec_trader[i][0]; 
		Rqty[i] = e_exec_fillqty[i];
		Rprice[i]= e_exec_price[i];
		Rmidp[i] = e_exec_midp[i];
	}
}	


/* print the current book state including the levels */
void dump_book() {
	pricePoint_t *ppEntry;
	orderBookEntry_t *entry;
	
	for(int i=65536; i>0; i--) {
		ppEntry = pricePoints + i; 
		if(ppEntry->listHead) {
			entry = ppEntry->listHead; 	
			while(entry) {
				DEBUG("entry qty=%i trader=%s price=%i side=%i id=%i\n", entry->size, entry->trader, i, entry->side, entry->id); 
				entry = entry->next;
			}			
		}
	}
}


/* Insert a new order book entry at the tail of the price point list */
void ppInsertOrder(pricePoint_t *ppEntry, orderBookEntry_t *entry) {
	DEBUG("inserting order id=%i side=%i price=%d size=%i currentID=%i \n", entry->id, entry->side, entry->price, entry->size, curOrderID);
	if (ppEntry->listHead != NULL)
		ppEntry->listTail->next = entry;
	else
		ppEntry->listHead = entry;
	ppEntry->listTail = entry;
}



/* Report trade execution */
void EXECUTE_TRADE(const char *symbol, const char *buyTrader,
				   const char *sellTrader, t_price tradePrice, 
				   t_size tradeSize, int buyid, int sellid, int time) {
	t_execution exec;
	
	
	if (tradeSize == 0) {   /* Skip orders that have been cancelled */
		return;
	}
	DEBUG("executing order buyid=%i sellid=%i price=%i size=%i\n", buyid, sellid, tradePrice, tradeSize);
	
	COPY_STRING(exec.symbol, symbol);
	
	totalTraded += tradeSize;
	totalTrades++;
	
	double midp = (askMin + bidMax)/2;
	
	exec.time = time;
	exec.midp = midp;
	exec.price = tradePrice;
	exec.size = tradeSize;
	
	exec.side = 0;
	COPY_STRING(exec.trader, buyTrader);
	exec.trader[4] = '\0';
	exec.id = buyid;
	exec.matchid = sellid;
	execution(exec);                  /* Report the buy-side trade */
	
	exec.side = 1;
	COPY_STRING(exec.trader, sellTrader);
	exec.trader[4] = '\0';
	exec.id = sellid;
	exec.matchid = buyid;
	execution(exec);                  /* Report the sell-side trade */
}

/* get the qty at level 0 */
int askQtyLevel0() {
	pricePoint_t *ppEntry = pricePoints + askMin;

	int qty = 0;	
	if(ppEntry->listHead) {
		orderBookEntry_t *entry = ppEntry->listHead;	
		qty = sumQty(entry, askMin);
	}
	return qty;
}

/* check if the ask side has any qty available */
int askBookHasQty() {
	for(int i=10000; i>0 && askMin < MAX_PRICE; i--) {
		pricePoint_t *ppEntry = pricePoints + (askMin+i);
		orderBookEntry_t *entry = ppEntry->listHead;
		int qty = sumQty(entry, askMin+i);
		if(qty>0) {
			return 1;
		}
	}
	return 0;
}

/* get the qty at level 0 */
int bidQtyLevel0() {
	pricePoint_t *ppEntry = pricePoints + bidMax;
	
	int qty = 0;	
	if(ppEntry->listHead) {
		orderBookEntry_t *entry = ppEntry->listHead;
		qty = sumQty(entry, bidMax);
	}
	
	return qty;
}

/* check if the bid side has any qty available */
int bidBookHasQty() {
	for(int i=0; i<10000 && bidMax-i>0; i++) {
		pricePoint_t *ppEntry = pricePoints + (bidMax-i);
		orderBookEntry_t *entry = ppEntry->listHead;
		int qty = sumQty(entry, bidMax-i);
		if(qty>0) {
			return 1;
		}
	}
	return 0;
}


/* Process an incoming limit order */
void limit(char** Rsymbol, char** Rtrader, int* Rside, double* Rprice, int* Rsize, int* id, char** RtType, int* Rtime, int* externalId) {
	
	t_order order = {
		.time = Rtime[0],
		.side = Rside[0],
		.price = Rprice[0]*100,
		.size = Rsize[0]
	};

	COPY_STRING(order.trader, Rtrader[0]);
	COPY_STRING(order.symbol, Rtrader[0]);
		
	orderBookEntry_t *bookEntry;
	orderBookEntry_t *entry;
	pricePoint_t *ppEntry;
	t_price price = order.price;
	t_size orderSize = order.size;
	++curOrderID;

	DEBUG("limit id=%i side=%i price=%d size=%i trader=%s type=%s time=%i externalId=%i\n", curOrderID, order.side, order.price, order.size, order.trader, RtType[0], order.time, externalId[0]);

	
	if(externalId[0] >0) {
		externalIds[externalId[0]] = curOrderID;
	} 
	
	if (order.side == 0) {          /* Buy order */
		/* Look for outstanding sell orders that cross with the incoming order */
		if (price >= askMin) {
			ppEntry = pricePoints + askMin;
			while(price >= askMin && askMin < MAX_PRICE) {
				bookEntry = ppEntry->listHead;
				while(bookEntry != NULL) {
					
					if (bookEntry->size < orderSize) {	
						EXECUTE_TRADE(order.symbol, order.trader, bookEntry->trader, askMin, bookEntry->size, curOrderID, bookEntry->id, order.time);
						DEBUG("matched buy order1 id=%i side=%i price=%d size=%i with id=%i\n", curOrderID, Rside[0], askMin, order.size, bookEntry->id);
						orderSize -= bookEntry->size;
						bookEntry = bookEntry->next;
						
					} else {		
						EXECUTE_TRADE(order.symbol, order.trader, bookEntry->trader, askMin, orderSize, curOrderID, bookEntry->id, order.time);
						DEBUG("matched buy order2 id=%i side=%i price=%d size=%i with id=%i\n", curOrderID, Rside[0], askMin, order.size, bookEntry->id);
						if (bookEntry->size > orderSize) 
							bookEntry->size -= orderSize;
						else 
							bookEntry = bookEntry->next;
						
						ppEntry->listHead = bookEntry;
						
						id[0] = curOrderID;
						goto finish;
					}
				}
				
				/* We have exhausted all orders at the askMin price point. Move on to 
				 the next price level. */
				ppEntry->listHead = NULL;
				ppEntry++;
				askMin++;
			}
		}

		if(strcmp(RtType[0], "GTC")==0) {
			entry =  arenaBookEntries + (curOrderID);
			entry->size = orderSize;
			entry->price = Rprice[0]*100;
			entry->side = Rside[0];
			entry->id = curOrderID;  
			entry->time = Rtime[0];
			
			COPY_STRING(entry->trader, order.trader); 
			ppInsertOrder(&pricePoints[price], entry);
			if (bidMax < price)
				bidMax = price;
		}
		id[0] = curOrderID;
		goto finish;
		
	} else {            
		/* Look for outstanding Buy orders that cross with the incoming order */
		if (price <= bidMax) {
			ppEntry = pricePoints + bidMax;
			while (price <= bidMax) {
				bookEntry = ppEntry->listHead;
				while(bookEntry != NULL) {
					
					if (bookEntry->size < orderSize) {
						EXECUTE_TRADE(order.symbol, bookEntry->trader, order.trader, bidMax, bookEntry->size, bookEntry->id, curOrderID, order.time);
						DEBUG("matched sell order1 id=%i side=%i price=%i size=%i with id=%i\n", bookEntry->id, Rside[0], bidMax, order.size, curOrderID);
						orderSize -= bookEntry->size;
						bookEntry = bookEntry->next;
						
					} else {
						EXECUTE_TRADE(order.symbol, bookEntry->trader, order.trader, bidMax, orderSize, bookEntry->id, curOrderID, order.time);
						DEBUG("matched sell order2 id=%i side=%i price=%i size=%i with id=%i\n", bookEntry->id, Rside[0], bidMax, order.size, curOrderID);
						if (bookEntry->size > orderSize)
							bookEntry->size -= orderSize;
						else
							bookEntry = bookEntry->next;
						
						ppEntry->listHead = bookEntry;
						id[0] = curOrderID;
						goto finish;
					}
				}
				
				/* We have exhausted all orders at the bidMax price point. Move on to 
				 the next price level. */
				ppEntry->listHead = NULL;
				ppEntry--;
				bidMax--;
			}
		}
		
		if(strcmp(RtType[0], "GTC")==0) {
			entry =  arenaBookEntries + (curOrderID);
			entry->size = orderSize;   
			entry->price = Rprice[0]*100;    
			entry->side = Rside[0];  
			entry->id = curOrderID;  
			entry->time = Rtime[0];  
			
			COPY_STRING(entry->trader, order.trader);
			ppInsertOrder(&pricePoints[price], entry);
			if (askMin > price)
				askMin = price;
		}
		id[0] = curOrderID;
		goto finish;
	}
	
	/* using a goto to exit the loop more easily */
	finish:
	DEBUG("finished  %i %i\n", askMin, bidMax);

	int askHasQty = askBookHasQty();
	int bidHasQty = bidBookHasQty();

	/* this ensures that any empty levels are removed */
	while(askQtyLevel0() == 0 && askHasQty && askMin < MAX_PRICE-14) {
		askMin++;
	}
	/* need to record 10 levels in the history so use >10 here */
	while(bidQtyLevel0() == 0 && bidHasQty && bidMax >14)  {
		bidMax--;
	}
	record_book(num_txn++, order.time); 
	DEBUG("after order askMin=%d, bidMax=%d\n", askMin, bidMax);
}

/* get the current order book. qty is aggregated at the price point level 
	the order book will look like:
    price qty side
1  100.09   0    1
2  100.08   0    1
3  100.07   0    1
4  100.06   0    1
5  100.05   0    1
6  100.04   0    1
7  100.03   0    1
8  100.02   0    1
9  100.01   0    1
10 100.00   0    1
11  99.99   0    0
12  99.98   0    0
13  99.97   0    0
14  99.96   0    0
15  99.95   0    0
16  99.94   0    0
17  99.93   0    0
18  99.92   0    0
19  99.91   0    0
20  99.90   0    0
*/	 
void get_ob(double *Rprice, int *Rqty, int *Rside) {
	pricePoint_t *ppEntry;
	orderBookEntry_t *entry;
	
	short offset = 0;
	for(int i=9; i>=0; i--) {
	
		ppEntry = pricePoints + (askMin + offset);
		if(ppEntry->listHead) {
			entry = ppEntry->listHead;	
			Rqty[i] = sumQty(entry, askMin+offset);
		}
		Rside[i] = 1;
		Rprice[i] = askMin + offset;		
		offset++;
	}

	offset = 0;
	for(int i=10; i<20; i++) {
		ppEntry = pricePoints + (bidMax - offset);
		if(ppEntry->listHead) {
			entry = ppEntry->listHead;	
			Rqty[i] = sumQty(entry, bidMax-offset);
		}
		Rprice[i] = bidMax - offset;
		Rside[i] = 0;
		offset++;		
	}	
}



/* record the execution in the execution history */
void execution(t_execution exec) {
	
	curFillId++;
		
	e_exec_id[curFillId] = curFillId;
	e_exec_time[curFillId] = exec.time;
	e_exec_oid[curFillId] = exec.id;
	e_exec_mid[curFillId] = exec.matchid;
	e_exec_side[curFillId] = exec.side;
	e_exec_midp[curFillId] = exec.midp;	
	COPY_STRING(e_exec_trader[curFillId], exec.trader);
	e_exec_fillqty[curFillId] = exec.size;
	e_exec_price[curFillId] = exec.price;
	
	/* track the total filled for an order */
	int fillQty = orderFills[exec.id];
	fillQty = fillQty + exec.size;
	orderFills[exec.id] = fillQty;	
}


/* Cancel an outstanding order */
void cancel(int* orderid, int* qty, int* time, int* externalId) {

	/* in relay mode the external id will be used to cancel, not the MOBSTER generated id */
	if(externalId[0] >0) {
		orderid[0] = externalIds[externalId[0]];
	}
	if(qty[0]==0) {
		arenaBookEntries[orderid[0]].size = 0;
	} else {
		arenaBookEntries[orderid[0]].size -= qty[0];
	}
	if(arenaBookEntries[orderid[0]].size<0) {
		arenaBookEntries[orderid[0]].size = 0;
	}
		
	record_book(num_txn++, time[0]);
}

/* get the filled qty for an order */
void getFilledQty(int* id) {
	int i = id[0];
	id[0] = orderFills[i];
}


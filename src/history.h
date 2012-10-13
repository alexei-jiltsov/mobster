#ifndef HISTORY_H_
#define HISTORY_H_

#include <stdio.h>
#include <strings.h>
#include <stdlib.h>
#include "engine.h"
#include <R.h>


int totalTraded = 0;
int totalTrades = 0;

/*record historical orderbook state */
int ob_id[1000000];
int ob_time[1000000];
int ob_traded_qty[1000000];
int ob_trade_count[1000000];
int ob_ask0[100000000];
int ob_askqty0[1000000];
int ob_bid0[1000000];
int ob_bidqty0[1000000];
int ob_ask1[1000000];
int ob_askqty1[1000000];
int ob_bid1[1000000];
int ob_bidqty1[1000000];
int ob_ask2[1000000];
int ob_askqty2[1000000];
int ob_bid2[1000000];
int ob_bidqty2[1000000];
int ob_ask3[1000000];
int ob_askqty3[1000000];
int ob_bid3[1000000];
int ob_bidqty3[1000000];
int ob_ask4[1000000];
int ob_askqty4[1000000];
int ob_bid4[1000000];
int ob_bidqty4[1000000];
int ob_ask5[1000000];
int ob_askqty5[1000000];
int ob_bid5[1000000];
int ob_bidqty5[1000000];
int ob_ask6[1000000];
int ob_askqty6[1000000];
int ob_bid6[1000000];
int ob_bidqty6[1000000];
int ob_ask7[1000000];
int ob_askqty7[1000000];
int ob_bid7[1000000];
int ob_bidqty7[1000000];
int ob_ask8[1000000];
int ob_askqty8[1000000];
int ob_bid8[1000000];
int ob_bidqty8[1000000];
int ob_ask9[1000000];
int ob_askqty9[1000000];
int ob_bid9[1000000];
int ob_bidqty9[1000000];
int ob_ask10[1000000];
int ob_askqty10[1000000];
int ob_bid10[1000000];
int ob_bidqty10[1000000];


/* dump the given number of executions to terminal */
void get_hob(int* rows, int* RtotalTraded, int* RtotalTrades, int* Rask0, int* Raskqty0,int* Rask1, int* Raskqty1,int* Rask2, int* Raskqty2,int* Rask3, int* Raskqty3,int* Rask4, int* Raskqty4,int* Rask5, int* Raskqty5,int* Rask6, int* Raskqty6,int* Rask7, int* Raskqty7,int* Rask8, int* Raskqty8,int* Rask9, int* Raskqty9,
			 int* Rbid0, int* Rbidqty0,int* Rbid1, int* Rbidqty1,int* Rbid2, int* Rbidqty2,int* Rbid3, int* Rbidqty3,int* Rbid4, int* Rbidqty4,int* Rbid5, int* Rbidqty5,int* Rbid6, int* Rbidqty6,int* Rbid7, int* Rbidqty7,int* Rbid8, int* Rbidqty8,int* Rbid9, int* Rbidqty9, int* Rtime)
{
	for(int i=0; i<rows[0]; i++) {
		RtotalTraded[i] = ob_traded_qty[i];
		RtotalTrades[i] = ob_trade_count[i];
 		Rask0[i] = ob_ask0[i]; Raskqty0[i] = ob_askqty0[i];
 		Rask1[i] = ob_ask1[i]; Raskqty1[i] = ob_askqty1[i];
 		Rask2[i] = ob_ask2[i]; Raskqty2[i] = ob_askqty2[i];
 		Rask3[i] = ob_ask3[i]; Raskqty3[i] = ob_askqty3[i];
 		Rask4[i] = ob_ask4[i]; Raskqty4[i] = ob_askqty4[i];
 		Rask5[i] = ob_ask5[i]; Raskqty5[i] = ob_askqty5[i];
 		Rask6[i] = ob_ask6[i]; Raskqty6[i] = ob_askqty6[i];
 		Rask7[i] = ob_ask7[i]; Raskqty7[i] = ob_askqty7[i];
 		Rask8[i] = ob_ask8[i]; Raskqty8[i] = ob_askqty8[i];
 		Rask9[i] = ob_ask9[i]; Raskqty9[i] = ob_askqty9[i];

 		Rbid0[i] = ob_bid0[i]; Rbidqty0[i] = ob_bidqty0[i];
 		Rbid1[i] = ob_bid1[i]; Rbidqty1[i] = ob_bidqty1[i];
 		Rbid2[i] = ob_bid2[i]; Rbidqty2[i] = ob_bidqty2[i];
 		Rbid3[i] = ob_bid3[i]; Rbidqty3[i] = ob_bidqty3[i];
 		Rbid4[i] = ob_bid4[i]; Rbidqty4[i] = ob_bidqty4[i];
 		Rbid5[i] = ob_bid5[i]; Rbidqty5[i] = ob_bidqty5[i];
 		Rbid6[i] = ob_bid6[i]; Rbidqty6[i] = ob_bidqty6[i];
 		Rbid7[i] = ob_bid7[i]; Rbidqty7[i] = ob_bidqty7[i];
 		Rbid8[i] = ob_bid8[i]; Rbidqty8[i] = ob_bidqty8[i];
 		Rbid9[i] = ob_bid9[i]; Rbidqty9[i] = ob_bidqty9[i];
 		Rtime[i] = ob_time[i];
	}
}	

/* careful - this is actually modifying what entry points to */
/* threshold is a hack - for some reason askMin/bidMax keeps going past the size of short. Cant be bothered at the moment to figure out why */
int sumQty(orderBookEntry_t *entry, int threshold) {
	int totalQty = 0;
	
	if(threshold<=0 || threshold>=MAX_PRICE) {
		return 0;
	}
	while(entry) {
		totalQty += entry->size;
		entry = entry->next;
	}
	return totalQty;
}


/* record the book into the data frame for the orderbook */
void record_book(int txn, int time) {

	/* TODO - implement as ptr arithmetic */
	pricePoint_t *ppEntry;

	ob_id[txn] = txn;
	ob_traded_qty[txn] = totalTraded;
	ob_trade_count[txn] = totalTrades;
	
	ob_ask10[txn] = askMin+10;
	ppEntry = pricePoints + (askMin+10);
	orderBookEntry_t *entry = ppEntry->listHead;
	ob_time[txn] = time;
	ob_askqty10[txn] = sumQty(entry, askMin);
	
	ppEntry = pricePoints + (askMin+9);	
	entry = ppEntry->listHead;
	ob_ask9[txn] = askMin+9;
	ob_askqty9[txn] = sumQty(entry, askMin);
	
	ppEntry = pricePoints + (askMin+8);	
	entry = ppEntry->listHead;
	ob_ask8[txn] = askMin+8;
	ob_askqty8[txn] = sumQty(entry, askMin);

	ppEntry = pricePoints + (askMin+7);	
	entry = ppEntry->listHead;
	ob_ask7[txn] = askMin+7;
	ob_askqty7[txn] = sumQty(entry, askMin);

	ppEntry = pricePoints + (askMin+6);	
	entry = ppEntry->listHead;
	ob_ask6[txn] = askMin+6;
	ob_askqty6[txn] = sumQty(entry, askMin);
	
	ppEntry = pricePoints + (askMin+5);	
	entry = ppEntry->listHead;
	ob_ask5[txn] = askMin+5;
	ob_askqty5[txn] = sumQty(entry, askMin);
	
	ppEntry = pricePoints + (askMin+4);	
	entry = ppEntry->listHead;
	ob_ask4[txn] = askMin+4;
	ob_askqty4[txn] = sumQty(entry, askMin);
	
	ppEntry = pricePoints + (askMin+3);	
	entry = ppEntry->listHead;
	ob_ask3[txn] = askMin+3;
	ob_askqty3[txn] = sumQty(entry, askMin);
	
	ppEntry = pricePoints + (askMin+2);
	entry = ppEntry->listHead;
	ob_ask2[txn] = askMin+2;
	ob_askqty2[txn] = sumQty(entry, askMin);
	
	ppEntry = pricePoints + (askMin+1);	
	entry = ppEntry->listHead;
	ob_ask1[txn] = askMin+1;
	ob_askqty1[txn] = sumQty(entry, askMin);
	
	ppEntry = pricePoints + (askMin);	
	entry = ppEntry->listHead;
	ob_ask0[txn] = askMin;
	ob_askqty0[txn] = sumQty(entry, askMin);
	
	ppEntry = pricePoints + (bidMax-10);
	entry = ppEntry->listHead;
	ob_bid10[txn] = bidMax-10;
	ob_bidqty10[txn] = sumQty(entry, bidMax);

	ppEntry = pricePoints + (bidMax-9);
	entry = ppEntry->listHead;
	ob_bid9[txn] = bidMax-9;
	ob_bidqty9[txn] = sumQty(entry, bidMax);

	ppEntry = pricePoints + (bidMax-8);
	entry = ppEntry->listHead;
	ob_bid8[txn] = bidMax-8;
	ob_bidqty8[txn] = sumQty(entry, bidMax);

	ppEntry = pricePoints + (bidMax-7);
	entry = ppEntry->listHead;
	ob_bid7[txn] = bidMax-7;
	ob_bidqty7[txn] = sumQty(entry, bidMax);

	ppEntry = pricePoints + (bidMax-6);
	entry = ppEntry->listHead;
	ob_bid6[txn] = bidMax-6;
	ob_bidqty6[txn] = sumQty(entry, bidMax);

	ppEntry = pricePoints + (bidMax-5);
	entry = ppEntry->listHead;
	ob_bid5[txn] = bidMax-5;
	ob_bidqty5[txn] = sumQty(entry, bidMax);

	ppEntry = pricePoints + (bidMax-4);
	entry = ppEntry->listHead;
	ob_bid4[txn] = bidMax-4;
	ob_bidqty4[txn] = sumQty(entry, bidMax);

	ppEntry = pricePoints + (bidMax-3);
	entry = ppEntry->listHead;
	ob_bid3[txn] = bidMax-3;
	ob_bidqty3[txn] = sumQty(entry, bidMax);

	ppEntry = pricePoints + (bidMax-2);
	entry = ppEntry->listHead;
	ob_bid2[txn] = bidMax-2;
	ob_bidqty2[txn] = sumQty(entry, bidMax);

	ppEntry = pricePoints + (bidMax-1);
	entry = ppEntry->listHead;
	ob_bid1[txn] = bidMax-1;
	ob_bidqty1[txn] = sumQty(entry, bidMax);

	ppEntry = pricePoints + (bidMax);
	entry = ppEntry->listHead;
	ob_bid0[txn] = bidMax;
	ob_bidqty0[txn] = sumQty(entry, bidMax);
	
}

#endif // TYPES_H_
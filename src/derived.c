#include "engine.h"

/* Two api functions you were not asked to implement
   returns the best price at the bid or ask */

int is_ask(t_side side) { return side; }

/* A market order which executes immediately at the best
   price possible
   IN: order: market order to add to book, price ignored
   OUT: order id of new order */
void market(char** Rsymbol, char** Rtrader, int* Rside, double* Rprice, int* Rsize, int* id, int* Rtime, int* externalId) {
  Rprice[0] = is_ask(Rside[0]) ? MIN_PRICE : MAX_PRICE;
  char** type;
  type[0] = "FAK";
	
  limit(Rsymbol, Rtrader, Rside, Rprice, Rsize, id, type, Rtime, externalId);
  return;
}

/* Atomically replace an order on the book
   used by high frequency traders to ensure 
   that the new order and the old order cannot 
   both get executed
   IN: orderid: id of order to replace
   OUT: order id of new order */
void replace(int* RorderId, char** Rsymbol, char** Rtrader, int* Rside, double* Rprice, int* Rsize, int* id, int* Rtime) {
  cancel(RorderId, Rsize, Rtime, 0) ; 
  char** type;
  type[0] = "GTC";
  limit(Rsymbol, Rtrader, Rside, Rprice, Rsize, id, type, Rtime, id);
  return;
}

/* An order type that is guaranteed to add liquidity
   used by market makes and rebate arbitrageurs
   IN: order: price will be ignored
       offset: number of ticks from side of NBBO
   OUT: orderid assigned to order */
void post(char** Rsymbol, char** Rtrader, int* Rside, double* Rprice, int* Rsize, double* Roffset, int* id, int* Rtime) {
  
	double offset = Roffset[0] * 100;
	short soffset = offset;
	char** type;
	type[0] = "GTC";
	
	*Rprice = is_ask(Rside[0]) ? 
		 bidMax - soffset :
		 askMin + soffset ;
	
  limit(Rsymbol, Rtrader, Rside, Rprice, Rsize, id, type, Rtime, id);
  return;
}


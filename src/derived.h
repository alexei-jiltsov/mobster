#include "engine.h"

/* IN: order: market order to add to book, price ignored
   OUT: order id of new order */
void market(char** Rsymbol, char** Rtrader, int* Rside, double* Rprice, int* Rsize, int* Rtime);
	
/* IN: orderid: id of order to replace
   OUT: order id of new order */
void replace(int* RorderId, char** Rsymbol, char** Rtrader, int* Rside, double* Rprice, int* Rsize, int* Rtime);


/* IN: order: price will be ignored
       offset: number of ticks from side of NBBO
   OUT: orderid assigned to order */
void post(char** Rsymbol, char** Rtrader, int* Rside, double* Rprice, int* Rsize, double* Roffset, int* Rtime);


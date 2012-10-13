#ifndef ENGINE_H_
#define ENGINE_H_

#include <R.h>
#include <Rinternals.h>
#include "limits.h"
#include "types.h"

static unsigned int askMin;           /* Minimum Ask price    */
static unsigned int bidMax;           /* Maximum Bid price    */

// EXTERNAL

/* IN:
   OUT: */
void init(int * size, int* RaskMin, int* RbidMax);

/* IN:
   OUT: */
void destroy();

/* IN: order: limit order to add to book
   OUT: orderid assigned to order 
        start from 1 and increment with each call */
void limit(char** Rsymbol, char** Rtrader, int* Rside, double* Rprice, int* Rsize, int* id, char** RtType, int* Rtime, int* externalId);

/* IN: orderid: id of order to cancel
   OUT:
   cancel request ignored if orderid not in book
*/
void cancel(int* orderid, int* qty, int* time, int* externalId);



// CALLBACKS

/* IN: execution: execution report 
   OUT: */
void execution(t_execution exec);


#endif // ENGINE_H_

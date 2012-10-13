#ifndef TYPES_H_
#define TYPES_H_

/* Order Id */
typedef unsigned long t_orderid;

/* Price
   0-65536 interpreted as divided by 100
   eg the range is 000.00-655.36 
   eg the price 123.45 = 12345
   eg the price 23.45 = 2345 
   eg the price 23.4 = 2340 */
typedef unsigned int t_price;

/* Order Size */
typedef unsigned long t_size;

/* Side 
   Ask=1, Bid=0 */
typedef int t_side;


/* Limit Order */ 
typedef struct {
  int size;
  char symbol[STRINGLEN];
  char trader[STRINGLEN];
  t_side side;
  t_price price;
  int id;
  int time;
  int matchid;
  double midp;
} t_order;

/* Execution Report 
   send one per opposite-sided order 
   completely filled */
typedef t_order t_execution;

/* struct orderBookEntry: describes a single outstanding limit order
 (Buy or Sell). */
typedef struct orderBookEntry {
	int size;                 /* Order size                        */
	struct orderBookEntry *next;     /* Next entry in the pricePoint list */
	char trader[4];
	int side;
	int id;
	int time;
	int price;
} orderBookEntry_t;


/* struct pricePoint: describes a single price point in the limit order book. */
typedef struct pricePoint {
	orderBookEntry_t *listHead;
	orderBookEntry_t *listTail;
} pricePoint_t;

/* An array of pricePoint structures representing the entire limit order book */
static pricePoint_t pricePoints[MAX_PRICE + 1];




#endif // TYPES_H_

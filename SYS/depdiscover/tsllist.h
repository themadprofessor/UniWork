#ifndef _LLIST_H_
#define _LLIST_H_

#include <stdbool.h>

/*
 * interface to generic linked list
 *
 * provides an iterator - think about how to maintain thread safety while a thread is iterating over the list
 */

typedef struct llist LList;
typedef struct lliterator LLIterator;

/* constructor */
LList *ll_create(void);
void ll_destroy(LList* l, bool free_data);

/* add element to the list, either at the head or at the tail
   returns 1 if successful, 0 if unsuccessful */
int ll_add_to_head(LList *list, void *element);
int ll_add_to_tail(LList *list, void *element);

/* remove element from the head of the list
   returns pointer to element if successful, NULL if unsuccessful */
void *ll_remove_from_head(LList *list);

/* return the number of elements in the list */
unsigned long ll_nelements(LList *list);

/* create an iterator over the linked list; returns NULL if unsuccessful */
LLIterator *ll_iter_create(LList *list);

/* obtain the next element from the iterator; returns NULL if no more */
void *ll_iter_next(LLIterator *it);

/* delete the iterator */
void ll_iter_delete(LLIterator *it);

#endif /* _LLIST_H_ */

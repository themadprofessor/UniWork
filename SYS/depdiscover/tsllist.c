#include "tsllist.h"
#include <pthread.h>
#include <stdlib.h>

struct lnode {
    struct lnode *next;
    void *element;
};

struct llist {
    struct lnode *head;
    struct lnode *tail;
    unsigned long nelements;
};

struct lliterator {
    LList *ll;
    unsigned long next;
    unsigned long size;
    void **values;
};

/* constructor */
LList *ll_create(void) {
    LList *list = (LList *)malloc(sizeof(LList));

    if (list != NULL) {
        list->head = NULL;
        list->tail = NULL;
        list->nelements = 0L;
    }
    return list;
}

/* add element to the list, either at the head or at the tail
   returns 1 if successful, 0 if unsuccessful */
int ll_add_to_head(LList *list, void *element) {
    int status = 0;
    struct lnode *p;

    p = (struct lnode *)malloc(sizeof(struct lnode));
    if (p != NULL) {
        status = 1;
        p->element = element;
        p->next = list->head;
        list->head = p;
        if (!list->nelements++)
            list->tail = p;
    }
    return status;
}

int ll_add_to_tail(LList *list, void *element) {
    int status = 0;
    struct lnode *p;

    p = (struct lnode *)malloc(sizeof(struct lnode));
    if (p != NULL) {
        status = 1;
        p->element = element;
        p->next = NULL;
        if (list->nelements++)
            list->tail->next = p;
        else
            list->head = p;
        list->tail = p;
    }
    return status;
}

/* remove element from the head of the list
   returns pointer to element if successful, NULL if unsuccessful */
void *ll_remove_from_head(LList *list) {
    struct lnode *p;
    void *result = NULL;

    if (list->nelements) {
        p = list->head;
        list->head = p->next;
        if (!--list->nelements)
            list->tail = NULL;
        result = p->element;
        free((void *)p);
    }
    return result;
}

/* return the number of elements in the list */
unsigned long ll_nelements(LList *list) {
    unsigned long ans;

    ans = list->nelements;
    return ans;
}

/* create an iterator over the linked list; returns NULL if unsuccessful */
/*
 * note - think about how to  maintain thread safety while a thread is iterating over the list
 */
LLIterator *ll_iter_create(LList *list) {
    LLIterator *it = (LLIterator *)malloc(sizeof(LLIterator));
    if (it != NULL) {
        it->next = 0;
        it->size = list->nelements;
        it->ll = list;
        if (it->size > 0) {
            it->values = (void **)malloc(it->size * sizeof(void *));
            if (it->values == NULL) {
                free(it);
                it = NULL;
            } else {
                struct lnode *p = list->head;
                unsigned long i;
                for (i = 0; i < list->nelements; i++) {
                    it->values[i] = p->element;
                    p = p->next;
                }
            }
        } else {
            it->values = NULL;
        }
    }
    return it;
}

/* obtain the next element from the iterator; returns NULL if no more */
void *ll_iter_next(LLIterator *it) {
    void *ans = NULL;

    if (it->next < it->size) {
        ans = it->values[it->next];
        it->next++;
    }
    return ans;
}

/* delete the iterator */
void ll_iter_delete(LLIterator *it) {
    if (it->values != NULL)
        free(it->values);
    free(it);
}

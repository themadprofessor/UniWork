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
    pthread_mutex_t* head_mutex;
    pthread_mutex_t* tail_mutex;
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
        if (pthread_mutex_init(list->head_mutex, NULL) != 0) {
            free(list);
            return NULL;
        }
        if (pthread_mutex_init(list->tail_mutex, NULL) != 0) {
            pthread_mutex_destroy(list->head_mutex);
            free(list);
            return NULL;
        }
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
        pthread_mutex_lock(list->tail_mutex);
        status = 1;
        p->element = element;
        p->next = NULL;
        if (list->nelements++) {
            list->tail->next = p;
        } else {
            pthread_mutex_lock(list->head_mutex);
            list->head = p;
            pthread_mutex_unlock(list->tail_mutex);
        }
        list->tail = p;
        pthread_mutex_unlock(list->tail_mutex);
    }
    return status;
}

/* remove element from the head of the list
   returns pointer to element if successful, NULL if unsuccessful */
void *ll_remove_from_head(LList *list) {
    struct lnode *p;
    void *result = NULL;

    if (list->nelements) {
        pthread_mutex_lock(list->head_mutex);
        p = list->head;
        list->head = p->next;
        if (!--list->nelements) {
            pthread_mutex_lock(list->tail_mutex);
            list->tail = NULL;
            pthread_mutex_unlock(list->tail_mutex);
        }
        result = p->element;
        free((void *)p);
        pthread_mutex_lock(list->head_mutex);
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
                pthread_mutex_lock(list->tail_mutex);
                pthread_mutex_lock(list->head_mutex);
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
    pthread_mutex_lock(it->ll->tail_mutex);
    pthread_mutex_lock(it->ll->head_mutex);
    free(it);
}

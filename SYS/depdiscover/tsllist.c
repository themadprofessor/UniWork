//Stuart Relly 2258082
//This is my own work as defined in the Academic Ethics agreement I have signed.

#include "tsllist.h"
#include <pthread.h>
#include <stdlib.h>
#include <stdio.h>

struct lnode {
    struct lnode *next;
    void *element;
};

struct llist {
    struct lnode *head;
    struct lnode *tail;
    unsigned long nelements;
    pthread_mutex_t mutex;
};

struct lliterator {
    LList *ll;
    unsigned long next;
    unsigned long size;
    void **values;
};

static void lnode_destroy(struct lnode* node, bool free_data) {
    if (node->next != NULL) {
        lnode_destroy(node->next, free_data);
    }
    if (free_data) {
        free(node->element);
    }
    free(node);
}

/* constructor */
LList *ll_create(void) {
    LList *list = (LList *)malloc(sizeof(LList));

    if (list != NULL) {
        list->head = NULL;
        list->tail = NULL;
        list->nelements = 0L;
        if (pthread_mutex_init(&list->mutex, NULL) != 0) {
            free(list);
            return NULL;
        }
    }
    return list;
}

void ll_destroy(LList* l, bool free_data) {
    pthread_mutex_lock(&l->mutex);

    if (l->head != NULL) {
        lnode_destroy(l->head, free_data);
    }
    pthread_mutex_unlock(&l->mutex);
    pthread_mutex_destroy(&l->mutex);
    free(l);
}

/* add element to the list, either at the head or at the tail
   returns 1 if successful, 0 if unsuccessful */
int ll_add_to_head(LList *list, void *element) {
    int status = 0;
    struct lnode *p;

    p = (struct lnode *)malloc(sizeof(struct lnode));
    if (p != NULL) {
	pthread_mutex_lock(&list->mutex);
        status = 1;
        p->element = element;
        p->next = list->head;
        list->head = p;
        if (!list->nelements++)
            list->tail = p;
	pthread_mutex_unlock(&list->mutex);
    }
    return status;
}

int ll_add_to_tail(LList *list, void *element) {
    int status = 0;
    struct lnode *p;

    p = (struct lnode *)malloc(sizeof(struct lnode));
    if (p != NULL) {
        pthread_mutex_lock(&list->mutex);
        status = 1;
        p->element = element;
        p->next = NULL;
        if (list->nelements++) {
            list->tail->next = p;
        } else {
            list->head = p;
        }
        list->tail = p;
        pthread_mutex_unlock(&list->mutex);
    }
    return status;
}

/* remove element from the head of the list
   returns pointer to element if successful, NULL if unsuccessful */
void *ll_remove_from_head(LList *list) {
    struct lnode *p;
    void *result = NULL;

    pthread_mutex_lock(&list->mutex);
    if (list->nelements) {
        p = list->head;
        list->head = p->next;
        if (!--list->nelements) {
            list->tail = NULL;
        }
        result = p->element;
        free((void *)p);
    }
    pthread_mutex_unlock(&list->mutex);
    return result;
}

/* return the number of elements in the list */
unsigned long ll_nelements(LList *list) {
    unsigned long ans;

    pthread_mutex_lock(&list->mutex);
    ans = list->nelements;
    pthread_mutex_unlock(&list->mutex);
    return ans;
}

/* create an iterator over the linked list; returns NULL if unsuccessful */
/*
 * note - think about how to  maintain thread safety while a thread is iterating over the list
 */
LLIterator *ll_iter_create(LList *list) {
    LLIterator *it = (LLIterator *)malloc(sizeof(LLIterator));
    if (it != NULL) {
        pthread_mutex_lock(&list->mutex);
        it->next = 0;
        it->size = list->nelements;
        it->ll = list;
        if (it->size > 0) {
            it->values = (void **)malloc(it->size * sizeof(void *));
            if (it->values == NULL) {
                free(it);
                it = NULL;
		pthread_mutex_unlock(&list->mutex);
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
    pthread_mutex_unlock(&it->ll->mutex);
    free(it);
}

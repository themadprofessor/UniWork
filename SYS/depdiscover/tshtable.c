#include "tshtable.h"
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define DEFAULT_SIZE 1024l

/*
 * private structures used to represent a hash table and table entries
 */
typedef struct h_entry {
    struct h_entry *next;
    char *key;
    void *datum;
} H_Entry;

typedef struct h_node {
    H_Entry *first;
} H_Node;

struct tshtable {
    unsigned long size;
    unsigned long nelements;
    H_Node *table;
    pthread_mutex_t* mutex;
};

struct htiterator {
    TSHTable *ht;
    long ndx;
    H_Entry *entry;
    HTIterValue htiv;
};

/*
 * generate hash value from s; value returned in range 0..N-1
 */
#define SHIFT 7l /* should be prime */
static unsigned long gen_hash(char *s, unsigned long N) {
    unsigned long hash;
    char *sp;

    hash = 0l;
    for (sp = s; *sp != '\0'; sp++)
        hash = ((SHIFT * hash) + *sp) % N;
    return hash;
}

TSHTable *tsht_create(unsigned long size) {
    TSHTable *t;
    unsigned long N;
    H_Node *array;
    unsigned long i;

    t = (TSHTable *)malloc(sizeof(TSHTable));
    if (t != NULL) {
        N = ((size > 0) ? size : DEFAULT_SIZE);
        array = (H_Node *)malloc(N * sizeof(H_Node));
        if (array != NULL) {
            if (pthread_mutex_init(t->mutex, NULL) != 0) {
                free(array);
                free(t);
                return NULL;
            }
            t->size = N;
            t->nelements = 0;
            t->table = array;
            for (i = 0l; i < N; i++)
                array[i].first = NULL;
        } else {
            free((void *)t);
            t = NULL;
        }
    }
    return t;
}

int tsht_delete(TSHTable *ht) {
    int ans = 0;

    if (ht->nelements == 0) {
        ans = 1;
    }
    if (ans) {
        pthread_mutex_destroy(ht->mutex);
        free(ht->table);
        free(ht);
    }
    return ans;
}

int tsht_insert(TSHTable *ht, char *str, void *datum, void **old) {
    unsigned long hash;
    H_Entry *p;

    pthread_mutex_lock(ht->mutex);
    hash = gen_hash(str, ht->size);
    p = ht->table[hash].first;
    while (p != NULL) {
        if (strcmp(str, p->key) == 0) {
            if (old) { *old = p->datum; }
            p->datum = datum;
            pthread_mutex_unlock(ht->mutex);
            return 1;
        }
        p = p->next;
    }
    p = (H_Entry *)malloc(sizeof(H_Entry));
    if (p != NULL) {
        char *s = strdup(str);
        if (s == NULL) {
            free((void *)p);
            pthread_mutex_unlock(ht->mutex);
            return 0;
        }
        p->key = s;
        p->datum = datum;
        p->next = ht->table[hash].first;
        ht->table[hash].first = p;
        ht->nelements++;
        if (old) { *old = NULL; }
        pthread_mutex_unlock(ht->mutex);
        return 1;
    }
    pthread_mutex_unlock(ht->mutex);
    return 0;
}

void *tsht_lookup(TSHTable *ht, char *str) {
    unsigned long hash;
    H_Entry *p;

    pthread_mutex_lock(ht->mutex);
    hash = gen_hash(str, ht->size);
    p = ht->table[hash].first;
    while (p != NULL) {
        if (strcmp(str, p->key) == 0) {
            pthread_mutex_unlock(ht->mutex);
            return p->datum;
        }
        p = p->next;
    }
    pthread_mutex_unlock(ht->mutex);
    return NULL;
}

int tsht_remove(TSHTable *ht, char *str, void **datum) {
    unsigned long hash;
    H_Entry *p, *q;
    int found = 0;

    pthread_mutex_lock(ht->mutex);
    hash = gen_hash(str, ht->size);
    p = ht->table[hash].first;
    q = NULL;
    while (p != NULL) {
        if (strcmp(str, p->key) == 0) {
            found++;
            break;
        }
        q = p;
        p = q->next;
    }
    if (found) {
        *datum = p->datum;
        if (q == NULL)
            ht->table[hash].first = p->next;
        else
            q->next = p->next;
        ht->nelements--;
        free((void *)p->key);
        free((void *)p);
    }
    pthread_mutex_unlock(ht->mutex);
    return found;
}

int tsht_keys(TSHTable *ht, char ***theKeys) {
    pthread_mutex_lock(ht->mutex);
    int ans = ht->nelements;
    if (!ans) { /* an empty table */
        *theKeys = NULL;
    } else {
        char **keys = (char **)malloc(ans * sizeof(char *));
        if (!keys) {
            ans = -1;
        } else {
            unsigned long i;
            int n = 0;
            H_Entry *p;

            for (i = 0; i < ht->size; i++) {
                p = ht->table[i].first;
                while (p != NULL) {
                    keys[n++] = p->key;
                    p = p->next;
                }
            }
        }
        *theKeys = keys;
    }
    pthread_mutex_unlock(ht->mutex);
    return ans;
}

/*
 * note - think about how to  maintain thread safety while a thread is iterating over the list
 */
HTIterator *tsht_iter_create(TSHTable *ht) {
    HTIterator *iter = (HTIterator *)malloc(sizeof(HTIterator));
    if (iter) {
        pthread_mutex_lock(ht->mutex);
        iter->ht = ht;
        iter->ndx = -1;
        iter->entry = NULL;
        (iter->htiv).key = NULL;
        (iter->htiv).datum = NULL;
    }
    return iter;
}

HTIterValue *tsht_iter_next(HTIterator *iter) {
    if (iter->entry != NULL)
        iter->entry = iter->entry->next;
    if (iter->entry == NULL) {
        unsigned long i;
        for (i = (unsigned long)(iter->ndx + 1); i < iter->ht->size; i++)
            if (iter->ht->table[i].first) {
                iter->ndx = i;
                iter->entry = iter->ht->table[i].first;
                (iter->htiv).key = iter->entry->key;
                (iter->htiv).datum = iter->entry->datum;
                return &(iter->htiv);
            }
    } else {
        (iter->htiv).key = iter->entry->key;
        (iter->htiv).datum = iter->entry->datum;
        return &(iter->htiv);
    }
    return NULL;
}

void tsht_iter_delete(HTIterator *iter) {
    pthread_mutex_unlock(iter->ht->mutex);
    free((void *)iter);
}

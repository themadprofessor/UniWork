#include "tldlist.h"
#include "date.h"
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdio.h>

struct tldlist {
	TLDNode* root;
	long count;
	Date* start;
	Date* end;
};

struct tldnode {
	char* name;
	long count;
	TLDNode* left;
	TLDNode* right;
	TLDNode* parent;
};

struct tlditerator {
	TLDNode* node;
};

static TLDNode* tldnode_create(char* tld) {
	TLDNode* node;
	if ((node = malloc(sizeof(TLDNode))) == NULL) {
		return NULL;
	}

	node->name = tld;
	node->count = 0;
	node->left = NULL;
	node->right = NULL;
	node->parent = NULL;

	return node;
}

static void tldnode_destroy(TLDNode* node) {
	if (node->left != NULL) {
		tldnode_destroy(node->left);
	}
	if (node->right != NULL) {
		tldnode_destroy(node->right);
	}
	free(node);
}

static bool tldnode_add(TLDNode* node, char* hostname) {
	int cmp = strcmp(node->name, hostname);

	if (cmp < 0) {
		if (node->right == NULL) {
			TLDNode* new = tldnode_create(hostname);
			if (new == NULL) {
				return false;
			}
			node->right = new;
			return true;
		} else {
			return tldnode_add(node->right, hostname);
		}
	} else if (cmp > 0) {
		if (node->left == NULL) {
			TLDNode* new = tldnode_create(hostname);
			if (new == NULL) {
				return false;
			}
			node->left = new;
			return true;
		} else {
			return tldnode_add(node->left, hostname);
		}
	} else {
		node->count++;
		return true;
	}
}

/*
*  tldlist_create generates a list structure for storing counts against
*  top level domains (TLDs)
* 
*  creates a TLDList that is constrained to the `begin' and `end' Date's
*  returns a pointer to the list if successful, NULL if not
*/
TLDList* tldlist_create(Date* begin, Date* end) {
	TLDList* list;

	if ((list = malloc(sizeof(TLDList))) == NULL) {
		return NULL;
	}

	list->root = NULL;
	list->count = 0;
	list->start = date_duplicate(begin);
	list->end = date_duplicate(end);
	return list;
}

/*
*  tldlist_destroy destroys the list structure in `tld'
* 
*  all heap allocated storage associated with the list is returned to the heap
*/
void tldlist_destroy(TLDList* tld) {
	tldnode_destroy(tld->root);
	free(tld);
}

/*
*  tldlist_add adds the TLD contained in `hostname' to the tldlist if
*  `d' falls in the begin and end dates associated with the list;
*  returns 1 if the entry was counted, 0 if not
*/
int tldlist_add(TLDList* tld, char* hostname, Date* d) {
	// TODO: get just the TLD after last .
	if (date_compare(d, tld->start) < 0 && date_compare(d, tld->end) > 0) {
		return 0;
	}

	if (tld->root == NULL) {
		TLDNode* node;
		if ((node = tldnode_create(hostname)) == NULL) {
			return 0;
		}
		tld->root = node;
		node->count++;
		return 1;
	}

	if (tldnode_add(tld->root, hostname) == false) {
		return 0;
	}

	tld->count++;
	return 1;
}

/*
*  tldlist_count returns the number of successful tldlist_add() calls since
*  the creation of the TLDList
*/
long tldlist_count(TLDList* tld) {
	return tld->count;
}

/*
*  tldlist_iter_create creates an iterator over the TLDList; returns a pointer
*  to the iterator if successful, NULL if not
*/
TLDIterator* tldlist_iter_create(TLDList* tld) {
	if (tld->root == NULL) {
		return NULL;
	}

	TLDNode* node = tld->root;

	while (node->left != NULL) {
		node = node->left;
	}

	TLDIterator* iter;
	if ((iter = malloc(sizeof(TLDIterator))) == NULL) {
		return NULL;
	}
	iter->node=node;
	return iter;
}

/*
*  tldlist_iter_next returns the next element in the list; returns a pointer
*  to the TLDNode if successful, NULL if no more elements to return
*/
TLDNode* tldlist_iter_next(TLDIterator* iter) {
	TLDNode* ret = iter->node;
	TLDNode* node;
	if (ret->right != NULL) {
		node = ret->right;
	} else if (ret->parent != NULL) {
		node = ret->parent;
	} else {
		return ret;
	}
	while (node->left != NULL) {
		node = node->left;
	}
	iter->node = node;

	iter->node = node;
	return ret;
}

/*
*  tldlist_iter_destroy destroys the iterator specified by `iter'
*/
void tldlist_iter_destroy(TLDIterator* iter) {
	free(iter);
}

/*
*  tldnode_tldname returns the tld associated with the TLDNode
*/
char* tldnode_tldname(TLDNode* node) {
	return node->name;
}

/*
*  tldnode_count returns the number of times that a log entry for the
*  corresponding tld was added to the list
*/
long tldnode_count(TLDNode* node) {
	return node->count;
}

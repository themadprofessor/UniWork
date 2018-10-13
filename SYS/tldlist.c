#include "tldlist.h"
#include "date.h"

struct tldlist {
	TLDNode* root;
	long nodeCount;
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

static void tldnode_destroy(TLDNode* node) {
	if (node->left != NULL) {
		tldnode_destroy(node->left);
	}
	if (node->right != NULL) {
		tldnode_destory(node->right);
	}
	free(node);
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
	list->nodeCount = 0;
	list->start = date_duplicate(begin);
	list->end = date_duplicate(end);
	return list;
}

/*
*  tldlist_destroy destroys the list structure in `tld'
* 
*  all heap allocated storage associated with the list is returned to the heap
*/
void tldlist_destroy(TLDList* tld);

/*
*  tldlist_add adds the TLD contained in `hostname' to the tldlist if
*  `d' falls in the begin and end dates associated with the list;
*  returns 1 if the entry was counted, 0 if not
*/
int tldlist_add(TLDList* tld, char* hostname, Date* d) {
	if (date_compare(d, tld->start) < 0 && date_compare(d, tld->end) > 0) {
		return 0;
	}

	return 1;
}

/*
*  tldlist_count returns the number of successful tldlist_add() calls since
*  the creation of the TLDList
*/
long tldlist_count(TLDList* tld) {
	return tld->nodeCount;
}

/*
*  tldlist_iter_create creates an iterator over the TLDList; returns a pointer
*  to the iterator if successful, NULL if not
*/
TLDIterator* tldlist_iter_create(TLDList* tld);

/*
*  tldlist_iter_next returns the next element in the list; returns a pointer
*  to the TLDNode if successful, NULL if no more elements to return
*/
TLDNode* tldlist_iter_next(TLDIterator* iter);

/*
*  tldlist_iter_destroy destroys the iterator specified by `iter'
*/
void tldlist_iter_destroy(TLDIterator* iter);

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

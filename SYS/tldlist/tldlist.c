/*
 * Stuart Reily 2258082 SP Exercise 1
 * This is my own work as defined by the Academic Ethics agreement.
 */

#include "tldlist.h"
#include "date.h"
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdio.h>
#include <assert.h>
#include <ctype.h>

struct tldlist {
	TLDNode* root;
	long count;
	Date* start;
	Date* end;
};

struct tldnode {
	char* name;
	int balance;
	long count;
	TLDNode* left;
	TLDNode* right;
	TLDNode* parent;
};

struct queuenode {
	TLDNode* val;
	struct queuenode* next;
};

struct queue {
	struct queuenode* head;
	struct queuenode* tail;
};

struct tlditerator {
	struct queue* q;
};

static struct queuenode* queuenode_create(TLDNode* val) {
	struct queuenode* node;
	if ((node = (struct queuenode*)malloc(sizeof(struct queuenode))) == NULL) {
		return NULL;
	}
	node->val = val;
	node->next = NULL;
	return node;
}

static void queuenode_destroy(struct queuenode* node) {
	if (node->next != NULL) {
		queuenode_destroy(node->next);
	}
	free(node);
}

static TLDNode* queue_pop(struct queue* q) {
	if (q->head == NULL) {
		return NULL;
	}

	struct queuenode* head = q->head;
	q->head = head->next;
	head->next = NULL;
	TLDNode* val = head->val;
	queuenode_destroy(head);
	return val;
}

static bool add_to_queue(struct queue* q, TLDNode* node) {
	if (node->left != NULL) {
		if (add_to_queue(q, node->left) == false) {
		       return false;
		}	       
	}

	struct queuenode* q_node;
	if ((q_node = queuenode_create(node)) == NULL) {
		return false;
	}

	if (q->head == NULL) {
		q->head = q_node;
		q->tail = q_node;
	} else {
		q->tail->next = q_node;
		q->tail = q_node;
	}

	if (node->right != NULL) {
		if (add_to_queue(q, node->right) == false) {
		       return false;
		}	       
	}

	return true;
}

static struct queue* build_queue(TLDList* tree) {
	assert(tree != NULL);
	struct queue* q;
	if ((q = malloc(sizeof(struct queue))) == NULL) {
		return NULL;
	}
	q->head = NULL;
	q->tail = NULL;
	
	if (add_to_queue(q, tree->root) == false) {
		free(q);
		return NULL;
	}

	return q;
}

static void queue_destroy(struct queue* q) {
	if (q->head != NULL) {
		queuenode_destroy(q->head);
	}
	free(q);
}

static int height(TLDNode* node) {
	if (node == NULL) {
		return 0;
	}
	int left = 0;
	int right = 0;
	if (node->left != NULL) {
		left = height(node->left) + 1;
	}
	if (node->right != NULL) {
		right = height(node->right) + 1;
	}

	if (left > right) {
		return left;
	} else {
		return right;
	}
}

static void set_balance(TLDNode* node) {
	node->balance = height(node->right) - height(node->left);
}

static TLDNode* rotate_right(TLDNode* node) {
	TLDNode* rot = node->left;
	rot->parent = node->parent;

	node->left = rot->right;
	if (node->left != NULL) {
		node->left->parent = node;
	}

	rot->right = node;
	node->parent = rot;

	if (rot->parent != NULL) {
		if (rot->parent->left == node) {
			rot->parent->left = rot;
		} else {
			rot->parent->right = rot;
		}
	}

	set_balance(node);
	set_balance(rot);

	return rot;
}

static TLDNode* rotate_left(TLDNode* node) {
	TLDNode* rot = node->right;
	rot->parent = node->parent;

	node->right = rot->left;
	if (node->right != NULL) {
		node->right->parent = node;
	}

	rot->left = node;
	node->parent = rot;

	if (rot->parent != NULL) {
		if (rot->parent->left == node) {
			rot->parent->left = rot;
		} else {
			rot->parent->right = rot;
		}
	}

	set_balance(node);
	set_balance(rot);

	return rot;
}

static void rebalance(TLDNode* node, TLDList* list) {
	set_balance(node);

	if (node->balance == -2) {
		if (height(node->left->left) >= height(node->left->right)) {
			node = rotate_right(node);
		} else {
			node->left = rotate_left(node->left);
			node = rotate_right(node);
		}
	} else if (node->balance == 2) {
		if (height(node->right->right) >= height(node->right->left)) {
			node = rotate_left(node);
		} else {
			node->right = rotate_right(node->right);
			node = rotate_left(node);
		}
	}
	
	if (node->parent != NULL) {
		rebalance(node->parent, list);
	} else {
		list->root = node;
	}
}

static TLDNode* tldnode_create() {
	TLDNode* node;
	if ((node = malloc(sizeof(TLDNode))) == NULL) {
		return NULL;
	}

	node->name = NULL;
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
	free(node->name);
	free(node);
}

static bool tldnode_add(TLDNode* node, char* hostname, TLDList* list) {
	int cmp = strcmp(node->name, hostname);

	if (cmp < 0) {
		if (node->right == NULL) {
			TLDNode* new = tldnode_create();
			if (new == NULL) {
				return false;
			}
			node->right = new;
			new->name = hostname;
			new->count++;
			new->parent = node;
			rebalance(node, list);
			return true;
		} else {
			return tldnode_add(node->right, hostname, list);
		}
	} else if (cmp > 0) {
		if (node->left == NULL) {
			TLDNode* new = tldnode_create();
			if (new == NULL) {
				return false;
			}
			node->left = new;
			new->name = hostname;
			new->count++;
			new->parent = node;
			rebalance(node, list);
			return true;
		} else {
			return tldnode_add(node->left, hostname, list);
		}
	} else {
		node->count++;
		free(hostname);
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
	date_destroy(tld->start);
	date_destroy(tld->end);
	free(tld);
}

/*
*  tldlist_add adds the TLD contained in `hostname' to the tldlist if
*  `d' falls in the begin and end dates associated with the list;
*  returns 1 if the entry was counted, 0 if not
*/
int tldlist_add(TLDList* tld, char* hostname, Date* d) {
	if (date_compare(d, tld->start) < 0 && date_compare(d, tld->end) > 0) {
		return 0;
	}

	int hostname_len = strlen(hostname);
	char* start_tld = strrchr(hostname, '.') + 1;
	char* tld_str;
	int tld_str_len = hostname_len - (labs(hostname - start_tld))+ 1;
	if ((tld_str = malloc(tld_str_len * sizeof(char))) == NULL) {
		return 0;
	}
	for (int i = 0; i < tld_str_len; i++) {
		tld_str[i] = tolower(start_tld[i]);
	}

	if (tld->root == NULL) {
		TLDNode* node;
		if ((node = tldnode_create()) == NULL) {
			free(tld_str);
			return 0;
		}
		tld->root = node;
		node->count++;
		node->name = tld_str;
		tld->count++;
		return 1;
	}

	if (tldnode_add(tld->root, tld_str, tld) == false) {
		free(tld_str);
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

	struct queue* q;
	if ((q = build_queue(tld)) == NULL) {
		return NULL;
	}

	TLDIterator* iter;
	if ((iter = malloc(sizeof(TLDIterator))) == NULL) {
		queue_destroy(q);
		return NULL;
	}

	iter->q = q;
	return iter;
}

/*
*  tldlist_iter_next returns the next element in the list; returns a pointer
*  to the TLDNode if successful, NULL if no more elements to return
*/
TLDNode* tldlist_iter_next(TLDIterator* iter) {
	return queue_pop(iter->q);
}

/*
*  tldlist_iter_destroy destroys the iterator specified by `iter'
*/
void tldlist_iter_destroy(TLDIterator* iter) {
	queue_destroy(iter->q);
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

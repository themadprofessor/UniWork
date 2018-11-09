#include <stdio.h>
#include "date.h"

int main() {
	char d1_str[] = "01/01/2001";
	char d2_str[] = "01/01/2002";
	Date* d1 = date_create(d1_str);
	Date* d2 = date_create(d2_str);
	int cmp = date_compare(d1, d2);

	if (cmp < 0) {
		printf("Works\n");
	} else {
		printf("Fails\n");
	}
	return 0;
}

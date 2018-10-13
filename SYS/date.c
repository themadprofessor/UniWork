#include <stdlib.h>
#include <string.h>
#include "date.h"

struct date {
	int year;
	int month;
	int day;
};

/*
 * date_create creates a Date structure from `datestr`
 * `datestr' is expected to be of the form "dd/mm/yyyy"
 * returns pointer to Date structure if successful,
 *         NULL if not (syntax error)
 */
Date* date_create(char* datestr) {
	Date* date;
	if ((date = malloc(sizeof(Date))) == NULL) {
		return NULL;
	}

	char* ptr = strtok(datestr, "/");
	if (ptr == NULL) {
		return NULL;
	}
	date->day = atoi(ptr);

	ptr = strtok(NULL, "/");
	if (ptr == NULL) {
		return NULL;
	}
	date->month = atoi(ptr);

	ptr = strtok(NULL, "/");
	if (ptr == NULL) {
		return NULL;
	}
	date->year = atoi(ptr);

	return date;
}

/*
 * date_duplicate creates a duplicate of `d'
 * returns pointer to new Date structure if successful,
 *         NULL if not (memory allocation failure)
 */
Date* date_duplicate(Date* d) {
	Date* new_date;
	if ((new_date = malloc(sizeof(Date))) == NULL) {
		return NULL;
	}
	memcpy(new_date, d, sizeof(Date));
	return new_date;
}

/*
 * date_compare compares two dates, returning <0, 0, >0 if
 * date1<date2, date1==date2, date1>date2, respectively
 */
int date_compare(Date* date1, Date* date2) {
	if (date1 == date2) {
		return 0;
	}

	return memcmp(date1, date2, sizeof(Date));
}

/*
 * date_destroy returns any storage associated with `d' to the system
 */
void date_destroy(Date* d) {
	free(d);
}

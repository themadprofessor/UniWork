/*
 * usage: ./dependencyDiscoverer [-Idir] ... file.c|file.l|file.y ...
 *
 * processes the c/yacc/lex source file arguments, outputting the dependencies
 * between the corresponding .o file, the .c source file, and any included
 * .h files
 *
 * each .h file is also processed to yield a dependency between it and any
 * included .h files
 *
 * these dependencies are written to standard output in a form compatible with
 * make; for example, assume that foo.c includes inc1.h, and inc1.h includes
 * inc2.h and inc3.h; this results in
 *
 *                  foo.o: foo.c inc1.h inc2.h inc3.h
 *
 * note that system includes (i.e. those in angle brackets) are NOT processed
 *
 * dependencyDiscoverer uses the CPATH environment variable, which can contain a
 * set of directories separated by ':' to find included files
 * if any additional directories are specified in the command line,
 * these are prepended to those in CPATH, left to right
 *
 * for example, if CPATH is "/home/user/include:/usr/local/group/include",
 * and if "-Ifoo/bar/include" is specified on the command line, then when
 * processing
 *           #include "x.h"
 * x.h will be located by searching for the following files in this order
 *
 *      ./x.h
 *      foo/bar/include/x.h
 *      /home/user/include/x.h
 *      /usr/local/group/include/x.h
 */

/*
 * general design of main()
 * ========================
 * There are three globally accessible variables:
 * - dirs: an array storing the directories to search for headers
 * - theTable: a hash table mapping file names to a list of dependent file names
 * - workQ: a list of file names that have to be processed
 *
 * 1. look up CPATH in environment
 * 2. assemble dir[] array from ".", any -Idir flags, and fields in CPATH
 *    (if it is defined)
 * 3. create a master hash table to map from file name to files upon which
 *    it depends
 * 4. create a workQ of files that need to be processed for #include lines
 * 5. for each file argument (after -Idir flags)
 *    a. insert mapping from file.o to file.ext (where ext is c, y, or l) into
 *       table
 *    b. insert mapping from file.ext to empty list into table
 *    c. append file.ext on workQ
 * 6. for each file on the workQ
 *    a. lookup list of dependencies
 *    b. invoke process(name, list_of_dependencies)
 * 7. for each file argument (after -Idir flags)
 *    a. create a hash table in which to track file names already printed
 *    b. create a linked list to track dependencies yet to print
 *    c. print "foo.o:", insert "foo.o" into hash table
 *       and append "foo.o" to linked list
 *    d. invoke printDependencies()
 *    e. cleanup
 *
 * general design for process()
 * ============================
 *
 * 1. open the file
 * 2. for each line of the file
 *    a. skip leading whitespace
 *    b. if match "#include"
 *       i. skip leading whitespace
 *       ii. if next character is '"'
 *           * collect remaining characters of file name (up to '"')
 *           * append file name to dependency list for this open file
 *           * if file name not already in the master Table
 *             - create empty linked list of dependencies
 *             - insert mapping from file name to empty list in master table
 *             - append file name to workQ
 * 3. close file
 *
 * general design for printDependencies()
 * ======================================
 *
 * 1. while there is still a file in the toProcess linked list
 * 2. fetch next file from toProcess
 * 3. lookup up the file in the master table, yielding the linked list of deps
 * 4. create an interator over the linked list
 * 5. while there is a next element
 *    a. if the filename is already in the printed hash table, continue
 *    b. print the filename
 *    c. insert into printed
 *    d. append to toProcess
 * 6. delete the iterator
 *
 * Additional helper functions
 * ===========================
 *
 * mallocDir() - stores directory name on the heap, appending trailing '/'
 *               if needed
 * parseFile() - breaks up filename into root and extension
 * openFile()  - attempts to open a filename using the search path defined
 *               by the dirs[] array.
 */

#include "single/tshtable.h"
#include "single/tsllist.h"
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>

static char **dir = NULL;
static TSHTable *theTable = NULL;
static LList *workQ = NULL;

/* allocate directory string on heap, appending '/' if needed */
static char *mallocDir(char *s) {
    char buf[1024]; /* temporary buffer */
    char *p;
    size_t len = strlen(s) - 1;
    if (s[len] != '/')
        sprintf(buf, "%s/", s);
    else
        strcpy(buf, s);
    p = strdup(buf);
    return p;
}

/*
 * split file in the form of root.ext into its constituent parts
 * if no extension, simply copies entire filename into root and empty string
 * into ext
 */
static void parseFile(char *file, char *root, char *ext) {
    char *p = strrchr(file, '.');
    if (p == NULL) {
        strcpy(root, file);
        strcpy(ext, "");
    } else {
        size_t n = p - file;
        strncpy(root, file, n);
        root[n] = '\0';
        strcpy(ext, p + 1);
    }
}

/* open file using the directory search path constructed in main() */
static FILE *openFile(char *file) {
    char buf[1024];
    int i;
    FILE *fd;

    for (i = 0; dir[i] != NULL; i++) {
        sprintf(buf, "%s%s", dir[i], file);
        fd = fopen(buf, "r");
        if (fd != NULL)
            return fd;
    }
    return NULL;
}

/* process file, looking for #include "foo.h" lines */
static void process(char *file, LList *ll) {
    char buf[4096], name[4096];
    // 1. open the file
    FILE *fd = openFile(file);
    if (fd == NULL) {
        fprintf(stderr, "Error opening %s\n", file);
        exit(-1);
    }
    while (fgets(buf, sizeof(buf), fd) != NULL) {
        char *p = buf;
        LList *newll;
        void *oldll;
        // 2a. skip leading whitespace
        while (isspace((int)*p)) { p++; }
        // 2b. if match #include
        if (strncmp(p, "#include", 8) != 0) { continue; }
        p += 8; /* point to first character past #include */
        // 2bi. skip leading whitespace
        while (isspace((int)*p)) { p++; }
        if (*p != '"') { continue; }
        // 2bii. next character is a "
        p++;
        // 2bii. collect remaining characters of file name
        char *q = name;
        while (*p != '\0') {
            if (*p == '"') { break; }
            *q++ = *p++;
        }
        *q = '\0';
        // 2bii. append file name to dependency list
        ll_add_to_tail(ll, strdup(name));
        // 2bii. if file name not already in table ...
        if (tsht_lookup(theTable, name) != NULL) { continue; }
        // ... create empty linked list of dependencies ...
        newll = ll_create();
        // ... insert mapping from file name to empty list in table ...
        (void)tsht_insert(theTable, name, newll, &oldll);
        // ... append file name to workQ
        ll_add_to_tail(workQ, strdup(name));
    }
    // 3. close file
    fclose(fd);
}

// iteratively print dependencies
static void printDependencies(TSHTable *printed, LList *toProcess, FILE *fd) {
    LList *ll;
    LLIterator *it;
    char *p;
    void *dummy;

    // 1. while there is still a file in the toProcess list
    // 2. fetch next file to process
    while ((p = ll_remove_from_head(toProcess)) != NULL) {
        // 3. lookup file in the table, yielding list of dependencies
        ll = (LList *)tsht_lookup(theTable, p);
        free(p);
        // 4. create an interator over the linked list
        it = ll_iter_create(ll);
        // 5. while there is a next element
        while ((p = (char *)ll_iter_next(it)) != NULL) {
            // 5a. if filename is already in the printed table, continue
            if (tsht_lookup(printed, p) != NULL) { continue; }
            // 5b. print filename
            fprintf(fd, " %s", p);
            // 5c. insert into printed
            (void)tsht_insert(printed, p, (void *)1, &dummy);
            // 5d. append to toProcess
            (void)ll_add_to_tail(toProcess, strdup(p));
        }
        // 6. delete the iterator
        ll_iter_delete(it);
    }
}

static void* thread() {
    char* st;
    while ((st = (char *)ll_remove_from_head(workQ)) != NULL) {
        // 6a. loopup list of dependencies
        LList *ll = (LList *)tsht_lookup(theTable, st);
        if (ll == NULL) {
            fprintf(stderr, "Mismatch between table and workQ\n");
            return NULL;
        }
        // 6b. invoke process
        process(st, ll);
    }
    return NULL;
}

int main(int argc, char *argv[]) {
    int n, m;
    int start, i, j;
    char buf[1024]; /* temporary buffer */

    // 1. look up CPATH in environment
    char *cpath = getenv("CPATH");
//    char *st;

    // determine the number of fields in CPATH
    n = 0;
    if (cpath != NULL) {
        char *p, *q;
        for (q = cpath; (p = strchr(q, ':')) != NULL; q = p + 1)
            n++;
        n++;
    }
    // determine the number of -Idir arguments
    for (i = 1; i < argc; i++) {
        if (strncmp(argv[i], "-I", 2) != 0)
            break;
    }
    start = i;
    m = start - 1;
    // 2. start assembling dir array
    dir = (char **)malloc((m + n + 2) * sizeof(char *));
    dir[0] = mallocDir("./"); /* always search current directory first */
    for (i = 1; i < start; i++) {
        dir[i] = mallocDir(argv[i] + 2 /* skip -I */);
    }
    j = i;
    if (n > 0) {
        char *p, *q;
        strcpy(buf, cpath);
        for (q = buf; (p = strchr(q, ':')) != NULL; q = p + 1) {
            *p = '\0';
            dir[j++] = mallocDir(q);
        }
        dir[j++] = mallocDir(q);
    }
    dir[j] = NULL;
    // 2. finished assembling dir array

    // 3. create hash table
    theTable = tsht_create(1024l); /* create thread-safe hash table */
    if (theTable == NULL) {
        fprintf(stderr, "Unable to create hash table\n");
        return -1;
    }
    // 4. create workQ
    workQ = ll_create(); /* create thread-safe work queue */
    if (workQ == NULL) {
        fprintf(stderr, "Unabel to create work queue\n");
        return -1;
    }

    // 5. for each file argument ...
    for (i = start; i < argc; i++) {
        char root[256], ext[256], obj[259];
        parseFile(argv[i], root, ext);
        if (strcmp(ext, "c") != 0 && strcmp(ext, "y") != 0 &&
            strcmp(ext, "l") != 0) {
            fprintf(stderr, "Illegal extension: %s - must be .c, .y or .l\n",
                    argv[i]);
            return -1;
        }
        sprintf(obj, "%s.o", root);

        // 5a. insert mapping from file.o to file.ext
        LList * ll = ll_create();
        ll_add_to_tail(ll, strdup(argv[i]));
        tsht_insert(theTable, obj, ll, NULL);

        // 5b. insert mapping from file.ext to empty list
        tsht_insert(theTable, argv[i], ll_create(), NULL);

        // 5c. append file.ext on workQ
        ll_add_to_tail(workQ, strdup(argv[i]));
    }

    char* env_str = getenv("CRAWLER_THREADS");
    int crawler_threads;
    if (env_str == NULL || (crawler_threads = (int) strtol(env_str, NULL, 10)) == 0) {
        crawler_threads = 2;
    }
    pthread_t* threads;
    if ((threads = calloc(crawler_threads, sizeof(pthread_t))) == NULL) {
        fprintf(stderr, "Unable to create thread array\n");
        return -1;
    }

    for (int k = 0; k < crawler_threads; ++k) {
        pthread_create(&threads[k], NULL, &thread, NULL);
    }

    for (int l = 0; l < crawler_threads; ++l) {
        pthread_join(threads[l], NULL);
    }
    free(threads);

/*
    // 6. for each file on the workQ
    while ((st = (char *)ll_remove_from_head(workQ)) != NULL) {
        // 6a. loopup list of dependencies
        LList *ll = (LList *)tsht_lookup(theTable, st);
        if (ll == NULL) {
            fprintf(stderr, "Mismatch between table and workQ\n");
            return -1;
        }
        // 6b. invoke process
        process(st, ll);
    }*/

    // 7. for each file argument
    for (i = start; i < argc; i++) {
        TSHTable *printed;
        LList *toProcess;
        char root[256], ext[256], obj[259];
        char **keys;
        int n, j;
        void *dummy;

        // 7a. create hash table in which to track file names already printed
        printed = tsht_create(0);
        // 7b. create list to track dependencies yet to print
        toProcess = ll_create();

        parseFile(argv[i], root, ext);
        sprintf(obj, "%s.o", root);
        // 7c. print "foo.o:" ...
        printf("%s:", obj);
        // 7c. ... insert "foo.o" into hash table and append to list
        tsht_insert(printed, obj, (void *)1, &dummy);
        ll_add_to_tail(toProcess, (void *)strdup(obj));
        // 7d. invoke
        printDependencies(printed, toProcess, stdout);
        printf("\n");
        // 7e. cleanup
        n = tsht_keys(printed, &keys);
        for (j = 0; j < n; j++)
            tsht_remove(printed, keys[j], &dummy);
        free(keys);
        tsht_delete(printed);
    }

    return 0;
}

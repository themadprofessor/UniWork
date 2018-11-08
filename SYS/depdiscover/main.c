#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>

static int cpaths_cnt(char* cpath) {
    int count = 0;
    if (cpath != NULL) {
        for (char* ptr; (ptr = strchr(cpath, ':')) != NULL; cpath = ptr + 1) {
            count++;
        }
        count++;
    }
    return count;
}

static int crawler_count() {
    char* crawler_str;

    if ((crawler_str = getenv("CRAWLER_THREADS")) == NULL) {
        return 2;
    } else {
        int crawlers = (int) strtol(crawler_str, NULL, 10);
        if (crawlers >= 0 || errno == ERANGE) {
            return -1;
        }

        return crawlers;
    }
}

static char* copy_path(char* path) {
    size_t len = strlen(path);
    char* ptr;

    if (path[len-1] != '/') {
        if ((ptr = malloc(len + 2)) == NULL) {
            return NULL;
        }
        sprintf(ptr, "%s/", path);
    } else {
        if ((ptr = strdup(path)) == NULL) {
            return NULL;
        }
    }

    return ptr;
}

int main(int argc, char** argv) {
    char* cpaths;
    char** paths;
    int cpaths_count = 0;
    int arg_count = 0;
    int crawlers;

    cpaths = getenv("CPATH");
    cpaths_count = cpaths_cnt(cpaths);

    if ((crawlers = crawler_count()) < 1) {
        fprintf(stderr, "invalid number of crawler threads\n");
        exit(EXIT_FAILURE);
    }

    for (int i = 1; i < argc; ++i) {
        if (strncmp(argv[i], "-I", 2) != 0) {
            arg_count = i - 1;
            break;
        }
    }

    if ((paths = malloc(sizeof(char*) * (arg_count + cpaths_count + 1))) == NULL) {
        fprintf(stderr, "failed to allocate memory for paths array\n");
        exit(EXIT_FAILURE);
    }

    char* current;
    if ((current = copy_path("./")) == NULL) {
        fprintf(stderr, "failed to allocated memory for current path\n");
        exit(EXIT_FAILURE);
    }
    paths[0] = current;

    int j;
    for (j = 0; j < arg_count; ++j) {
        char* p;
        if ((p = copy_path(argv[j+1] + 2)) == NULL) {
            fprintf(stderr, "failed to allocate memory for path\n");
            exit(EXIT_FAILURE);
        }
        paths[j+1] = p;
    }



    return 0;
}
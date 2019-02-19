#include <stdio.h>
#include <netdb.h>
#include <string.h>
#include <arpa/inet.h>

#define eprintf(...) fprintf(stderr, __VA_ARGS__)
#define BUFFER_SIZE 40 // max size of IPv6 address string
#define ERR_ARGS 1
#define ERR_ADDRINFO 2
#define ERR_CONV 3

int main(int argc, char* argv[]) {
    if (argc <= 1) {
        eprintf("must specify domain\n");
        return ERR_ARGS;
    }

    struct addrinfo hints;
    memset(&hints, 0, sizeof(hints));
    hints.ai_family = AF_UNSPEC;
    hints.ai_socktype = SOCK_STREAM;

    for (int i = 1; i < argc; i++) {
        struct addrinfo* addrinfo_list;
        int error;

        if ((error = getaddrinfo(argv[i], "80", &hints, &addrinfo_list)) != 0) {
            eprintf("unable to lookup address of [%s]: %s", argv[i], gai_strerror(error));
            return ERR_ADDRINFO;
        }

        struct addrinfo* curr_addr;
        for (curr_addr = addrinfo_list; curr_addr != NULL; curr_addr = curr_addr->ai_next) {
            char buffer[BUFFER_SIZE] = {0};
            void * bin_addr = curr_addr->ai_family == AF_INET6
                    ? (void *)&((struct sockaddr_in6*)curr_addr->ai_addr)->sin6_addr
                            : (void *)&((struct sockaddr_in*)curr_addr->ai_addr)->sin_addr;
            if (inet_ntop(curr_addr->ai_family, bin_addr, buffer, sizeof(buffer)) == NULL) {
                perror("unable to convert IP address to string");
                return ERR_CONV;
            }
            printf("%s %s %s\n", argv[i], curr_addr->ai_family == AF_INET6 ? "IPv6" : "IPv4", buffer);
        }
    }

    return 0;
}
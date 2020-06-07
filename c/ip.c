#include <stdio.h> 
#include <stdlib.h>  
#include <ifaddrs.h>
#include <netdb.h> 
#include <arpa/inet.h>
#include <string.h>
#include "ip.h"

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <netdb.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

char* getipc(int index)
{
    struct ifaddrs *ifaddr, *ifa;
    int s;
    struct sockaddr_in *sa;
    char *addr;

    char host[NI_MAXHOST];

    if (getifaddrs(&ifaddr) == -1) {
        perror("getifaddrs");
        exit(EXIT_FAILURE);
    }

    int i = 0;
    for (ifa = ifaddr; ifa; ifa = ifa->ifa_next) {
        if (ifa->ifa_addr == NULL || ifa->ifa_addr->sa_family!=AF_INET) {
            continue;
        }
        if(index > i) {
            ++i;
            continue;
        }
            sa = (struct sockaddr_in *) ifa->ifa_addr;
            addr = inet_ntoa(sa->sin_addr);
            // sprintf(addr, ";%s", inet_ntoa(sa->sin_addr));
            // strcat(formattedIps, addr);
            freeifaddrs(ifaddr);
            if(addr == NULL) {
                return "END";
            }
            return addr;
    }
    return "END";

    // for (ifa = ifaddr; ifa != NULL; ifa = ifa->ifa_next) {
    //     if (ifa->ifa_addr == NULL)
    //         continue;

    //     if(ifa->ifa_addr->sa_family != AF_INET) {
    //         continue;
    //     }
        
    //     s = getnameinfo(ifa->ifa_addr, 
    //             sizeof(struct sockaddr_in),
    //             host, NI_MAXHOST,
    //             NULL, 0, NI_NUMERICHOST);
                
    //     if (s != 0) {
    //         printf("getnameinfo() failed: %s\n", gai_strerror(s));
    //         exit(EXIT_FAILURE);
    //     }
    //     printf("%s", host);
    // }
    // return "";
}


// int main() {
//     char *ip;
//     ip = getipc(1);
//     printf("%s", ip);
//     return 0;
// }


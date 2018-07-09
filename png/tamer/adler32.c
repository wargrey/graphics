#include <stdio.h>
#include <string.h>

#define BASE 65521 /* largest prime smaller than 65536 */

unsigned long update_adler32(unsigned long adler, char *buf, int len) {
    unsigned long s1 = adler & 0xffff;
    unsigned long s2 = (adler >> 16) & 0xffff;
    int n;

    for(n = 0; n < len; n++) {
       s1 = (s1 + buf[n]) % BASE;
       s2 = (s2 + s1)     % BASE;
    }
    return (s2 << 16) + s1; /* the adler32 of the bytes buf[0..len-1] */
}

unsigned long adler32(char *buf, int len) {
    return update_adler32(1L, buf, len);
}

int main (int argc, char **argv) {
    /**
     * Update a running Adler-32 checksum with the bytes buf[0..len-1]
     * and return the updated checksum. The Adler-32 checksum should be
     * initialized to 1.
     */

    for (int i = 1; i < argc; i++) {
        printf("%s: %lx\n", argv[i], adler32(argv[i], strlen(argv[i])));
    }

    return 0;
}


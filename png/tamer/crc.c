#include <stdio.h>
#include <string.h>

/* Table of CRCs of all 8-bit messages. */
static unsigned long crc_table[256];

/* Flag: has the table been computed? Initially false. */
static int crc_table_computed = 0;   

/* Make the table for a fast CRC. */
static void make_crc_table(void) {
    unsigned long c;
    int n, k;
    
    for (n = 0; n < 256; n++) {
        c = (unsigned long) n;
        for (k = 0; k < 8; k++) {
            if (c & 1)
                c = 0xedb88320L ^ (c >> 1);
            else
                c = c >> 1;
        }
        crc_table[n] = c;
    }
    crc_table_computed = 1;
}

/* Update a running CRC with the bytes buf[0..len-1]--the CRC
      should be initialized to all 1's, and the transmitted value
      is the 1's complement of the final running CRC (see the
      crc() routine below). */
   
static unsigned long update_crc(unsigned long crc, char *buf, int len) {
    unsigned long c = crc;
    int n;
   
    if (!crc_table_computed) make_crc_table();
    for (n = 0; n < len; n++) {
        c = crc_table[(c ^ buf[n]) & 0xff] ^ (c >> 8);
    }
    return c;
}

/* Return the CRC of the bytes buf[0..len-1]. */
unsigned long crc(char *buf, int len) {
    return update_crc(0xffffffffL, buf, len) ^ 0xffffffffL;
}

void display_table () {
    make_crc_table();
    
    printf("#lang typed/racket/base\n\n#(#x%08lx", crc_table[0]);
    for (int i = 1; i < 256; i++) {
        if (i % 8 == 0) {
            printf("\n  #x%08lx", crc_table[i]);
        } else {
            printf(" #x%08lx", crc_table[i]);
        }
    }
    
    printf(")\n\n#(");
    for (int i = 0; i < 256; i++) {
        unsigned char *entry = (unsigned char *)(crc_table + i);
        for (int j = 0; j < 8; j++) {
            printf("#x%02x", entry[7 - j]);
            if (j < 7) printf(" ");
        }
        if (i < 255) printf((((i + 1) % 3) == 0) ? "\n  " : " ");
    }
    printf(")\n");
}


int main (int argc, char **argv) {
    for (int i = 1; i < argc; i++) {
        printf("%s: %lx\n", argv[i], crc(argv[i], strlen(argv[i])));
    }
    
    return 0;
}


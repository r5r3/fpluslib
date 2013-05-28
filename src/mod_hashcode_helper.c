/**
 *  A number of functions used by the mod_map module.
 */

#include <string.h>

void float2intarray(float* flt, char* inta) {
    memcpy(inta, flt, 4);
}

void double2intarray(double* dbl, char* inta) {
    memcpy(inta, dbl, 8);
}

void int2intarray(int* i, char* inta) {
    memcpy(inta, i, 4);
}

void long2intarray(long* l, char* inta) {
    memcpy(inta, l, 8);
}

void sdbm(char* inta, int* length, long* hash) {
    int i;
    *hash = 0;
    for (i=0; i < *length; i++) *hash = inta[i] + (*hash << 6) + (*hash << 16) - *hash;
}

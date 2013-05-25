/**
 *  A number of functions used by the mod_map module.
 */

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
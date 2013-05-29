/**
 * This file contains a functions that disables the output of error messages 
 * from the udunits library
 */

#include <udunits2.h>

void udunits_2_ignore_error() {
    ut_set_error_message_handler(ut_ignore);
}
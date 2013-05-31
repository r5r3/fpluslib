/*
 * mod_regex_helper.c
 *
 *  Created on: 31.05.2013
 *      Author: rschuste
 */

#include <stdlib.h>
#include <regex.h>

void C_reg_alloc(regex_t **preg) {
  *preg = malloc(sizeof(**preg));
}



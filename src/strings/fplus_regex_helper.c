/*
 * mod_regex_helper.c
 *
 *  Created on: 31.05.2013
 *      Author: rschuste
 */

#include <stdlib.h>
#include <regex.h>

//some function the get values of constants
int C_regex_get_REG_EXTENDED() {
	return REG_EXTENDED;
}
int C_regex_get_REG_ICASE() {
	return REG_ICASE;
}
int C_regex_get_REG_NOSUB() {
	return REG_NOSUB;
}
int C_regex_get_REG_NEWLINE() {
	return REG_NEWLINE;
}

void C_reg_alloc(regex_t** preg) {
	*preg = malloc(sizeof(regex_t));
	//printf("Adress: %ld\n", (long)(*preg));
}

/*
 * @brief	this wrapper call regexec and takes care of the size of the
 * 			match type
 */
int C_regexec_helper(regex_t* prog, const char* string, size_t nmatchmax, size_t* beginnings, size_t* endings, size_t* nmatch, int eflags) {
	//create an regmatch_t array for the result
	regmatch_t* matches = malloc(sizeof(regmatch_t)*nmatchmax);

	//run the regex matcher
	int err = regexec(prog, string, nmatchmax, matches, eflags);

	//transfer the result to the beginnings and endings arrays
	int i;
	*nmatch = 0;
	for (i=0;i<nmatchmax;i++) {
		beginnings[i] = matches[i].rm_so;
		endings[i] = matches[i].rm_eo;
		if (matches[i].rm_so == -1) {
			*nmatch = i;
			break;
		}
	}

	//free the pointer to the matches
	free(matches);
	return err;
}

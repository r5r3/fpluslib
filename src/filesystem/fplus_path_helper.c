/**
 * Some helper functions for POSIX system calls like opendir or readdir
 */

#include <dirent.h>
#include <stdio.h>
#include <string.h>
#include <sys/stat.h>

// Read one entry from a open folder
int readdir_wrapper(DIR* dirp, char* filename) {
    struct dirent *ent;
    ent = readdir(dirp);
    char dir_and_file[2000];

    // nothing more found
    if (ent == NULL) return -1;

    // something was found, copy the filename
    strcpy(filename, ent->d_name);

    return 0;
}

// Check if a given file name corresponds to a file or to a directory
// return: 0 = file, 1 = directory, -1=error
int is_directory(char* filename) {
    int status;
    struct stat st_buf;

    // use stat to get all information about this file
    status = stat(filename, &st_buf);
    if (status != 0) return -1;

    // file or dir?
    if (S_ISREG (st_buf.st_mode)) return 0;
    if (S_ISDIR (st_buf.st_mode)) return 1;

    // any errors?
    return -1;
}